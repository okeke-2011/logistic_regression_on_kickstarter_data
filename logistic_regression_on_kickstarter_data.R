df = read.csv("https://tinyurl.com/KaggleDataCS112") # read the csv file
df = na.omit(df) # remove all rows containing na values

df$success = ifelse(df$usd_goal_real < df$usd_pledged_real, 1, 0) 

# convert deadline column from strings to dates
df$deadline = ymd(df$deadline) 
# convert launched column from strings to date and time
df$launched = ymd_hms(df$launched) 
# find difference between launch date and deadline
df$length = df$deadline - as.Date(df$launched) 
# remove all lengths greater than 60
df = df[df$length < 60, ]

set.seed(112) # set the seed
# randomly sample indexes for test set
test_index = sample(1:nrow(df), nrow(df)/5, replace = FALSE) 
# subset for training data 
df_train = df[-test_index, ]
# subset for test data 
df_test = df[test_index, ] 

glm_01 = glm(success~backers+length+as.factor(main_category)+usd_goal_real, 
             family = "binomial", 
             data = df_train) 

test_probs = predict(glm_01, 
                     newdata = df_test, 
                     type = "response") 
# generate predictions for training data
train_probs = predict(glm_01, 
                      newdata = df_train, 
                      type = "response") 

# empty array with all 0's
test_pred = rep(0, nrow(df_test)) 
# when probability is above 0.5 change 0 to 1
test_pred[test_probs>0.5] = 1 
# confusion matrix for predictions on the test data
table(test_pred, df_test$success) 
# RMSE of the prediction for test data 
paste("Test set RMSE:", round(mean(test_pred != df_test$success), 4)) 

# empty array with all 0's
train_pred = rep(0, nrow(df_train)) 
# when probability is above 0.5 change 0 to 1
train_pred[train_probs>0.5] = 1 
# confusion matrix for predictions on the training data
table(train_pred, df_train$success) 
# RMSE of the prediction for training data 
paste("Training set RMSE:", round(mean(train_pred != df_train$success), 4)) 

set.seed(112)
# k-folf cv
test_cv_error = suppressWarnings(cv.glm(df_test, glm_01, k = 5)$delta[2])
paste("Test set RMSE:", test_cv_error)
