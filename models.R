# Packages I need (I think)
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(caret)
library(splitstackshape)
library(rattle)
library(gt)
library(xgboost)
library(xgboostExplainer)
library(pROC)
library(SHAPforxgboost)
library(fastDummies)
library(forecast)

# Load in data
load('G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/data/model_df.rda')

# Split data
total_obs <- nrow(model_df)
train_index <- sample(1:total_obs, 0.8 * total_obs)
train_data <- model_df[train_index, ]
test_data <- model_df[-train_index, ]

######################################### RUN vs. PASS #########################################
# SIMPLE TREE - NO MOVEMENT ADDED
tree1 <- rpart(run_pass ~ quarter + down + offenseFormation + receiverAlignment,
               data = train_data)
fancyRpartPlot(tree1)
# Hm... that is not very exciting. Lets tune it and make a more interesting tree.

# Parameter Tuning
tree2 <- rpart(run_pass ~ quarter + down + offenseFormation + receiverAlignment,
               data = train_data, 
               control = rpart.control(minsplit = 100, minbucket = 30, cp = 0.00021))
fancyRpartPlot(tree2, cex = 0.5)
# That makes something more interesting, but how is it at prediction?
tree2_preds <- predict(tree2, test_data, type = 'class')
t1 <- table(tree2_preds, test_data$run_pass)
confusionMatrix(t1)
# Pretty good, but prediction off a single tree is not that valuable

# RANDOM FOREST - NO MOVEMENT ADDED
# First try - bagging model
train_data_bag <- train_data %>% 
                    na.omit()
bag1 <- randomForest(run_pass ~ quarter + down + offenseFormation + receiverAlignment,
                     data = train_data_bag,
                     mtry = 4,
                     ntree = 500)
bag1_preds <- predict(bag1, test_data, type = 'class')
t2 <- table(bag1_preds, test_data$run_pass)
confusionMatrix(t2)
# Plot error to see if we should increase number of trees
oob_error <- bag1$err.rate[, 1]
plot_dat <- cbind.data.frame(rep(1:length(oob_error)), oob_error)
names(plot_dat) <- c('trees', 'oob_error')
ggplot(plot_dat, aes(x = trees, y = oob_error)) +
  geom_line(alpha = 0.5, color = 'green') +
  labs(title = 'OOB Error Rate vs Number of Trees',
       x = 'Number of Trees',
       y = 'OOB Error Rate') +
  theme_minimal()
# Maybe it could get better?
# Parameter Tuning 
mtry<- c(1,2,3,4)
nodesize <- c(1, 10, 25, 50, 100, 200, 500, 1000)
params <- expand.grid(mtry, nodesize)
names(params) <- c('mtry', 'nodesize')
res_vec <- rep(NA, nrow(params))
for(i in 1:nrow(params)){
  set.seed(102701)
  mod <- randomForest(run_pass ~ quarter + down + offenseFormation + receiverAlignment,
                      data = train_data_bag,
                      mtry = params$mtry[i],
                      ntree = 200,
                      nodesize = params$nodesize[i])
  res_vec[i] <- 1 - mod$err.rate[nrow(mod$err.rate), 1]
}
summary(res_vec)
# Visualize best parameters
res_db <- cbind.data.frame(params, res_vec)
names(res_db)[3] <- 'oob_accuracy'
res_db$mtry <- as.factor(res_db$mtry)
res_db$nodesize <- as.factor(res_db$nodesize)
ggplot(res_db, aes(x = mtry, y = nodesize, fill = oob_accuracy)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$oob_accuracy), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'OOB Accuracy by MTry and Nodesize',
       x = 'MTry',
       y = 'Nodesize',
       fill = 'OOB Accuracy') +
  theme_minimal()
res_db[which.max(res_db$oob_accuracy), ]
# Looks like mtry of 3 and a nodesize of 200 will give the best accuracy
bag3 <- randomForest(run_pass ~ quarter + down + offenseFormation + receiverAlignment,
                     data = train_data_bag,
                     mtry = 3,
                     ntree = 1000,
                     nodesize = 200)
bag3_preds <- predict(bag3, test_data, type = 'class')
t3 <- table(bag3_preds, test_data$run_pass)
confusionMatrix(t3)

# XGBOOST - NO MOVEMENT ADDED
# Set up dummies
train_data_xgb <- train_data %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_rp_base <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(4:5, 32:51)]),
                                  label = as.numeric(train_data_xgb$run_pass) - 1)
dtest_xgb_rp_base <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 32:51)]),
                                 label = as.numeric(test_data_xgb$run_pass) - 1)
# Train and Predict
xgb1 <- xgboost(data = dtrain_xgb_rp_base,
                nrounds = 100,
                verbose = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error')
xgb1_preds <- predict(xgb1, dtest_xgb_rp_base)
pred_dat <- cbind.data.frame(xgb1_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb1_preds))
boost_pred_class[xgb1_preds > 0.4] <- 'run'
t4 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t4)
# Some Tuning
bst <- xgboost(data = dtrain_xgb_rp_base,
               nfold = 5,
               eta = 0.1,
               nrounds = 1000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'binary:logistic',
               eval_metric = 'auc',
               eval_metric = 'error')
# 139 is the best number of trees, so I will run 200 for my tuning loop
# Tuning max depth and min child weight
max_depth_vals <- c(3, 5, 7, 10, 15)
min_child_weight_vals <- c(1, 3, 5, 7, 10, 15)
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 200,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# AUC Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'AUC') +
  theme_minimal()
# Error Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'Error') +
  theme_minimal()
# Going to go with max depth of 3 and min child weight of 15
# Tuning gamma
gamma_vals <- c(0, 0.05, 0.1, 0.15, 0.2)
auc_vec <- error_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 300,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 3,
                   min_child_weight = 15,
                   gamma = gamma_vals[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, auc_vec, error_vec)
# Gamma of 0.20 is the best here
# Now retune the initial model with the things we know now
xgb2 <- xgboost(data = dtrain_xgb_rp_base,
                nfold = 5,
                eta = 0.1,
                nrounds = 1000,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 3,
                min_child_weight = 15,
                gamma = 0.2)
# Stops at 14 now!
# Now tune by subsampled and colsample_by_tree
subsample_vals <- c(0.6, 0.7, 0.8, 0.9, 1)
colsample_bytree_vals <- c(0.6, 0.7, 0.8, 0.9, 1)
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 200,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 3,
                   min_child_weight = 15,
                   gamma = 0.2,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# AUC Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'AUC') +
  theme_minimal()
# Error Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'Error') +
  theme_minimal()
# Subsample of 1 and colsample of 0.6 it is!
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_rp_base,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 200,
                    early_stopping_rounds = 20,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 3,
                    min_child_weight = 15,
                    gamma = 0.2,
                    subsample = 1,
                    colsample_bytree = 0.6)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_rp_base,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 200,
                    early_stopping_rounds = 20,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 3,
                    min_child_weight = 15,
                    gamma = 0.2,
                    subsample = 1,
                    colsample_bytree = 0.6)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_rp_base,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 200,
                    early_stopping_rounds = 20,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 3,
                    min_child_weight = 15,
                    gamma = 0.2,
                    subsample = 1,
                    colsample_bytree = 0.6)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_rp_base,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 200,
                    early_stopping_rounds = 20,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 3,
                    min_child_weight = 15,
                    gamma = 0.2,
                    subsample = 1,
                    colsample_bytree = 0.6)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_rp_base,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 200,
                    early_stopping_rounds = 20,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 3,
                    min_child_weight = 15,
                    gamma = 0.2,
                    subsample = 1,
                    colsample_bytree = 0.6)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_error_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'Error by Iteration and Eta',
       x = 'Iteration',
       y = 'Error',
       color = 'Eta') +
  theme_minimal()
# 0.3 gives the lowest error, let's finish up with our final model
xgb3 <- xgboost(data = dtrain_xgb_rp_base,
                nfold = 5,
                eta = 0.3,
                nrounds = 1000,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 3,
                min_child_weight = 15,
                gamma = 0.2,
                subsample = 1,
                colsample_bytree = 0.6)
# Final Prediction for This One
xgb3_preds <- predict(xgb3, dtest_xgb_rp_base)
pred_dat <- cbind.data.frame(xgb3_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb3_preds))
boost_pred_class[xgb3_preds > 0.5] <- 'run'
t5 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t5)
# SHAP
source("C:/Users/liams/Downloads/a_insights_shap_functions.r", echo=TRUE)
shap_result <- shap.score.rank(xgb_model = xgb3,
                               X_train = as.matrix(train_data_xgb[, c(4:5, 32:51)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(4:5, 32:51)]),
                       top_n = 10)
shap_rp_base <- plot.shap.summary(data_long = shap_long)
ggsave(shap_rp_base, file = "G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/shap_rp_base.jpeg", dpi = 600)


# SIMPLE TREE - WITH MOVEMENT
train_data_rp_move <- train_data %>% 
                      select(-c(playDescription, expectedPointsAdded))
tree4 <- rpart(run_pass ~ .,
               data = train_data_rp_move,
               control = rpart.control(minsplit = 100, minbucket = 30, cp = 0.001))
fancyRpartPlot(tree4, cex = 0.5)
tree4_preds <- predict(tree4, test_data, type = 'vector')
tree4_preds <- ifelse(tree4_preds > 1, 'run', 'pass')
t6 <- table(tree4_preds, test_data$run_pass)
confusionMatrix(t6)

# RANDOM FOREST - WITH MOVEMENT
train_data_bag <- train_data_rp_move %>% 
                    na.omit()
bag4 <- randomForest(run_pass ~ .,
                     data = train_data_bag,
                     mtry = 17,
                     ntree = 500)
bag4_preds <- predict(bag4, test_data, type = 'class')
t7 <- table(bag4_preds, test_data$run_pass)
confusionMatrix(t7)
# Plot error to see if we should increase number of trees
oob_error <- bag4$err.rate[, 1]
plot_dat <- cbind.data.frame(rep(1:length(oob_error)), oob_error)
names(plot_dat) <- c('trees', 'oob_error')
ggplot(plot_dat, aes(x = trees, y = oob_error)) +
  geom_line(alpha = 0.5, color = 'green') +
  labs(title = 'OOB Error Rate vs Number of Trees',
       x = 'Number of Trees',
       y = 'OOB Error Rate') +
  theme_minimal()
# Parameter tuning for bagging
mtry <- c(13, 14, 15, 16, 17)
nodesize <- c(1, 10, 25, 50, 100, 200, 500, 1000)
params <- expand.grid(mtry, nodesize)
names(params) <- c('mtry', 'nodesize')
res_vec <- rep(NA, nrow(params))
for(i in 1:nrow(params)){
  print(i)
  set.seed(102701)
  mod <- randomForest(run_pass ~ .,
                      data = train_data_bag,
                      mtry = params$mtry[i],
                      ntree = 500,
                      nodesize = params$nodesize[i])
  res_vec[i] <- 1 - mod$err.rate[nrow(mod$err.rate), 1]
}
summary(res_vec)
# Visualize best parameters
res_db <- cbind.data.frame(params, res_vec)
names(res_db)[3] <- 'oob_accuracy'
res_db$trees <- as.factor(res_db$mtry)
res_db$nodesize <- as.factor(res_db$nodesize)
ggplot(res_db, aes(x = trees, y = nodesize, fill = oob_accuracy)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$oob_accuracy), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'OOB Accuracy by Number of Trees and Nodesize',
       x = 'Number of Trees',
       y = 'Nodesize',
       fill = 'OOB Accuracy') +
  theme_minimal()
res_db[which.max(res_db$oob_accuracy), ]
# 13 mtry and nodesize of 50 does the best
bag5 <- randomForest(run_pass ~ .,
                     data = train_data_bag,
                     mtry = 13,
                     ntree = 50,
                     nodesize = 500)
bag5_preds <- predict(bag5, test_data, type = 'class')
t8 <- table(bag5_preds, test_data$run_pass)
confusionMatrix(t8)
# Doing pretty good ... Let's Go!
# XGBOOST - WITH MOVEMENT
# Set up dummies
train_data_xgb <- train_data_rp_move %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>%
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_rp_move <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                                  label = as.numeric(train_data_xgb$run_pass) - 1)
dtest_xgb_rp_move <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 8:51)]),
                                 label = as.numeric(test_data_xgb$run_pass) - 1)
# Train and Predict
xgb4 <- xgboost(data = dtrain_xgb_rp_move,
                nrounds = 100,
                verbose = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error')
xgb4_preds <- predict(xgb4, dtest_xgb_rp_move)
pred_dat <- cbind.data.frame(xgb4_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb4_preds))
boost_pred_class[xgb4_preds > 0.5] <- 'run'
t9 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t9)
# Would you look at that! 
# Some Tuning
bst <- xgboost(data = dtrain_xgb_rp_move,
               nfold = 5,
               eta = 0.1,
               nrounds = 2000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'binary:logistic',
               eval_metric = 'auc',
               eval_metric = 'error')
# Best iteration is 1585, so I will run 1600 for my tuning loop
# Tuning max depth and min child weight
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1600,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# AUC Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'AUC') +
  theme_minimal()
# Error Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'Error') +
  theme_minimal()
# Looks like the best is max depth of 7 and min child weight of 15
# Tuning gamma
auc_vec <- error_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1600,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 7,
                   min_child_weight = 15,
                   gamma = gamma_vals[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, auc_vec, error_vec)
# Gamme of 0.00 is the best here
# Now retune the initial model with the things we know now
xgb5 <- xgboost(data = dtrain_xgb_rp_move,
                nfold = 5,
                eta = 0.1,
                nrounds = 1600,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 7,
                min_child_weight = 15,
                gamma = 0.0)
# Now tune by subsample and colsample_by_tree
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 2500,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 7,
                   min_child_weight = 15,
                   gamma = 0.0,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# AUC Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'AUC') +
  theme_minimal()
# Error Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'Error') +
  theme_minimal()
# Subsample of 0.8 and colsample of 0.8 looks good
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_rp_move,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 2500,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 15,
                    gamma = 0.0,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_rp_move,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 2500,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 15,
                    gamma = 0.0,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_rp_move,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 2500,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 15,
                    gamma = 0.0,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_rp_move,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 2500,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 15,
                    gamma = 0.0,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_rp_move,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 2500,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 15,
                    gamma = 0.0,
                    subsample = 0.8,
                    colsample_bytree = 0.8)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_error_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'Error by Iteration and Eta',
       x = 'Iteration',
       y = 'Error',
       color = 'Eta') +
  theme_minimal()
# Funky stuff. 0.05 looks best to me
xgb6 <- xgboost(data = dtrain_xgb_rp_move,
                nfold = 5,
                eta = 0.05,
                nrounds = 2500,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 7,
                min_child_weight = 15,
                gamma = 0.0,
                subsample = 0.8,
                colsample_bytree = 0.8)
# Final Prediction for This One
xgb6_preds <- predict(xgb6, dtest_xgb_rp_move)
pred_dat <- cbind.data.frame(xgb6_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb6_preds))
boost_pred_class[xgb6_preds > 0.5] <- 'run'
t10 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t10)
# SHAP
shap_result <- shap.score.rank(xgb_model = xgb6,
                               X_train = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                       top_n = 10)
shap_rp_move <- plot.shap.summary(data_long = shap_long)
ggsave(shap_rp_move, file = "G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/shap_rp_move.jpeg", dpi = 600)

# SIMPLE TREE - WITH MOVEMENT AND S/A ADDED
train_data_rp_move_sa <- train_data %>% 
                      select(-c(playDescription, expectedPointsAdded))
tree5 <- rpart(run_pass ~ ., 
               data = train_data_rp_move_sa,
               control = rpart.control(minsplit = 100, minbucket = 30, cp = 0.002))
fancyRpartPlot(tree5, cex = 0.5)
tree5_preds <- predict(tree5, test_data, type = 'vector')
tree5_preds <- ifelse(tree5_preds > 1, 'run', 'pass')
t11 <- table(tree5_preds, test_data$run_pass)
confusionMatrix(t11)

# RANDOM FOREST - WITH MOVEMENT AND S/A ADDED
train_data_bag <- train_data_rp_move_sa %>% 
                    na.omit()
bag5 <- randomForest(run_pass ~ .,
                     data = train_data_bag,
                     mtry = 25,
                     ntree = 500)
bag5_preds <- predict(bag5, test_data, type = 'class')
t12 <- table(bag5_preds, test_data$run_pass)
confusionMatrix(t12)
# Plot error to see if we should increase the number of trees
oob_error <- bag5$err.rate[, 1]
plot_dat <- cbind.data.frame(rep(1:length(oob_error)), oob_error)
names(plot_dat) <- c('trees', 'oob_error')
ggplot(plot_dat, aes(x = trees, y = oob_error)) +
  geom_line(alpha = 0.5, color = 'green') +
  labs(title = 'OOB Error Rate vs Number of Trees',
       x = 'Number of Trees',
       y = 'OOB Error Rate') +
  theme_minimal()
# Parameter tuning
mtry <- c(20, 21, 22, 23, 24, 25)
nodesize <- c(1, 10, 25, 50, 100, 200, 500, 1000)
params <- expand.grid(mtry, nodesize)
names(params) <- c('mtry', 'nodesize')
res_vec <- rep(NA, nrow(params))
for(i in 1:nrow(params)){
  print(i)
  set.seed(102701)
  mod <- randomForest(run_pass ~ .,
                      data = train_data_bag,
                      mtry = params$mtry[i],
                      ntree = 500,
                      nodesize = params$nodesize[i])
  res_vec[i] <- 1 - mod$err.rate[nrow(mod$err.rate), 1]
}
summary(res_vec)
# Visualize best parameters
res_db <- cbind.data.frame(params, res_vec)
names(res_db)[3] <- 'oob_accuracy'
res_db$trees <- as.factor(res_db$mtry)
res_db$nodesize <- as.factor(res_db$nodesize)
ggplot(res_db, aes(x = trees, y = nodesize, fill = oob_accuracy)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$oob_accuracy), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'OOB Accuracy by Number of Trees and Nodesize',
       x = 'Number of Trees',
       y = 'Nodesize',
       fill = 'OOB Accuracy') +
  theme_minimal()
res_db[which.max(res_db$oob_accuracy), ]
# 25 trees with 50 nodesize does the best
bag6 <- randomForest(run_pass ~ .,
                     data = train_data_bag,
                     mtry = 25,
                     ntree = 50,
                     nodesize = 50)
bag6_preds <- predict(bag6, test_data, type = 'class')
t13 <- table(bag6_preds, test_data$run_pass)
confusionMatrix(t13)

# XGBOOST - WITH MOVEMENT AND S/A ADDED
# Set up dummies
train_data_xgb <- train_data_rp_move_sa %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>%
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_rp_move_sa <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                                     label = as.numeric(train_data_xgb$run_pass) - 1)
dtest_xgb_rp_move_sa <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 8:99)]),
                                    label = as.numeric(test_data_xgb$run_pass) - 1)
# Train and Predict
xgb7 <- xgboost(data = dtrain_xgb_rp_move_sa,
                nrounds = 100,
                verbose = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error')
xgb7_preds <- predict(xgb7, dtest_xgb_rp_move_sa)
pred_dat <- cbind.data.frame(xgb7_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb7_preds))
boost_pred_class[xgb7_preds > 0.5] <- 'run'
t14 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t14)
# Model Tuning
bst <- xgboost(data = dtrain_xgb_rp_move_sa,
               nfold = 5,
               eta = 0.1,
               nrounds = 2000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'binary:logistic',
               eval_metric = 'auc',
               eval_metric = 'error')
# Best iteration is 981, will train at 1200
# Tuning max depth and min child weight
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1200,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# AUC heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'AUC') +
  theme_minimal()
# Error heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'Error') +
  theme_minimal()
# Max Depth of 7 and min child weight of 3
# Tuning gamma
auc_vec <- error_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1200,
                   early_stopping_rounds = 20,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 7,
                   min_child_weight = 3,
                   gamma = gamma_vals[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, auc_vec, error_vec)
# THe best gamma is 5
# Now retune the initial model with the things we know now
xgb8 <- xgboost(data = dtrain_xgb_rp_move_sa,
                nfold = 5,
                eta = 0.1,
                nrounds = 1200,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 7,
                min_child_weight = 3,
                gamma = 5)
# Next tune by subsample and colsample by tree
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
auc_vec <- error_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1200,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'binary:logistic',
                   eval_metric = 'auc',
                   eval_metric = 'error',
                   max_depth = 7,
                   min_child_weight = 3,
                   gamma = 5,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  auc_vec[i] <- max(bst_tune$evaluation_log$test_auc_mean[bst_tune$best_ntreelimit])
  error_vec[i] <- min(bst_tune$evaluation_log$test_error_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c('auc', 'error')
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# AUC Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = auc)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$auc), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'AUC by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'AUC') +
  theme_minimal()
# Error Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = error)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$error), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'Error by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'Error') +
  theme_minimal()
# Colsample of 0.7 and subsample of 0.9 is best
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 1200,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 3,
                    gamma = 5,
                    subsample = 0.9,
                    colsample_bytree = 0.7)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 1200,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 3,
                    gamma = 5,
                    subsample = 0.9,
                    colsample_bytree = 0.7)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 1200,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 3,
                    gamma = 5,
                    subsample = 0.9,
                    colsample_bytree = 0.7)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 1200,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 3,
                    gamma = 5,
                    subsample = 0.9,
                    colsample_bytree = 0.7)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_rp_move_sa,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 1200,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'binary:logistic',
                    eval_metric = 'auc',
                    eval_metric = 'error',
                    max_depth = 7,
                    min_child_weight = 3,
                    gamma = 5,
                    subsample = 0.9,
                    colsample_bytree = 0.7)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_error_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_error_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'Error by Iteration and Eta',
       x = 'Iteration',
       y = 'Error',
       color = 'Eta') +
  theme_minimal()
# 0.05 is best
xgb8 <- xgboost(data = dtrain_xgb_rp_move_sa,
                nfold = 5,
                eta = 0.05,
                nrounds = 1200,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'binary:logistic',
                eval_metric = 'auc',
                eval_metric = 'error',
                max_depth = 7,
                min_child_weight = 3,
                gamma = 5,
                subsample = 0.9,
                colsample_bytree = 0.7)
# Final Prediction for This One
xgb8_preds <- predict(xgb8, dtest_xgb_rp_move_sa)
pred_dat <- cbind.data.frame(xgb8_preds, test_data_xgb$run_pass)
boost_pred_class <- rep('pass', length(xgb8_preds))
boost_pred_class[xgb8_preds > 0.5] <- 'run'
t15 <- table(boost_pred_class, test_data_xgb$run_pass)
confusionMatrix(t15)
# SHAP
shap_result <- shap.score.rank(xgb_model = xgb8,
                               X_train = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                       top_n = 10)
shap_rp_move_sa <- plot.shap.summary(data_long = shap_long)
ggsave(shap_rp_move_sa, file = "G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/shap_rp_move_sa.jpeg", dpi = 600)



############################################ EPA ANALYSIS ############################################
# Trust The Process ... These ones without movement may not do great
# RANDOM FOREST - NO MOVEMENT
train_data_epa_base <- train_data %>% 
                      select(-c(playDescription, run_pass))
tree5 <- rpart(expectedPointsAdded ~ quarter + down + offenseFormation + receiverAlignment,
               data = train_data_epa_base,
               control = rpart.control(minsplit = 200, minbucket = 60, cp = 0.001))
fancyRpartPlot(tree5, cex = 0.5)
tree5_preds <- predict(tree5, test_data, type = 'vector')
t11 <- accuracy(tree5_preds, test_data$expectedPointsAdded)
t11
# Yeah that is pretty bad ... probably not even worth tuning this one.

# BAGGING - NO MOVEMENT
train_data_bag <- train_data_epa_base %>% 
                    na.omit()
bag6 <- randomForest(expectedPointsAdded ~ quarter + down + offenseFormation + receiverAlignment,
                     data = train_data_bag,
                     mtry = 4,
                     ntree = 500)
bag6_preds <- predict(bag6, test_data, type = 'response')
t12 <- accuracy(bag6_preds, test_data$expectedPointsAdded)
t12
# Error is actually WORSE with this. Moving on to XGBoost

# XGBOOST - NO MOVEMENT
# Set up dummies
train_data_xgb <- train_data_epa_base %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>%
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_epa_base <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(2:3, 30:49)]),
                                   label = train_data_xgb$expectedPointsAdded)
dtest_xgb_epa_base <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 32:51)]),
                                  label = test_data_xgb$expectedPointsAdded)
# Train and Predict
xgb7 <- xgboost(data = dtrain_xgb_epa_base,
                nrounds = 100,
                verbose = 1,
                print_every_n = 20,
                objective = 'reg:squarederror',
                eval_metric = 'rmse')
xgb7_preds <- predict(xgb7, dtest_xgb_epa_base)
t13 <- accuracy(xgb7_preds, test_data_xgb$expectedPointsAdded)
t13
# It is even worse...
# Some Tuning
bst <- xgboost(data = dtrain_xgb_epa_base,
               nfold = 5,
               eta = 0.1,
               nrounds = 3000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'reg:squarederror',
               eval_metric = 'rmse')
# 707 is best here, lets use 1000 from here
# Tuning max depth and min child weight
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 1000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# RMSE Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'RMSE') +
  theme_minimal()
# Max depth of 3 and min child of 15 is best
# Tuning gamma
rmse_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 15,
                   gamma = gamma_vals[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, rmse_vec)
# 0.00 is the best
# Now retune the initial model with the things we know now
xgb8 <- xgboost(data = dtrain_xgb_epa_base,
                nfold = 5,
                eta = 0.1,
                nrounds = 1000,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'reg:squarederror',
                eval_metric = 'rmse',
                max_depth = 3,
                min_child_weight = 15, 
                gamma = 0.0)
# At least it stopped much sooner!
# Tuning subsample and colsample_by_tree
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_base,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 5,
                   gamma = 0.15,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# RMSE Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'RMSE') +
  theme_minimal()
# 0.7 for subsample and 0.7 for colsample
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_epa_base,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 5,
                    gamma = 0.00,
                    subsample = 0.7,
                    colsample_bytree = 0.7)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_epa_base,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 5,
                    gamma = 0.00,
                    subsample = 0.7,
                    colsample_bytree = 0.7)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_epa_base,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 5,
                    gamma = 0.00,
                    subsample = 0.7,
                    colsample_bytree = 0.7)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_epa_base,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 5,
                    gamma = 0.00,
                    subsample = 0.7,
                    colsample_bytree = 0.7)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_epa_base,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 5,
                    gamma = 0.00,
                    subsample = 0.7,
                    colsample_bytree = 0.7)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_rmse_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'RMSE by Iteration and Eta',
       x = 'Iteration',
       y = 'RMSE',
       color = 'Eta') +
  theme_minimal()
# 0.1 looks best
xgb9 <- xgboost(data = dtrain_xgb_epa_base,
                nfold = 5,
                eta = 0.1,
                nrounds = 3000,
                early_stopping_rounds = 50,
                verbose = 1,
                nthread = 1,
                print_every_n = 20,
                objective = 'reg:squarederror',
                eval_metric = 'rmse',
                max_depth = 3,
                min_child_weight = 5,
                gamma = 0.0,
                subsample = 0.7,
                colsample_bytree = 0.7)
# Final Prediction for This One
xgb9_preds <- predict(xgb9, dtest_xgb_epa_base)
t14 <- accuracy(xgb9_preds, test_data_xgb$expectedPointsAdded)
t14
# SHAP
shap_result <- shap.score.rank(xgb_model = xgb9,
                               X_train = as.matrix(train_data_xgb[, c(2:3, 30:49)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                       top_n = 10)
shap_epa_base <- plot.shap.summary(data_long = shap_long)
ggsave(shap_epa_base, file = "G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/shap_epa_base.jpeg", dpi = 600)

# BASIC TREE - WITH MOVEMENT
train_data_epa_move <- train_data %>% 
                      select(-c(playDescription, run_pass))
tree6 <- rpart(expectedPointsAdded ~ .,
               data = train_data_epa_move,
               control = rpart.control(minsplit = 100, minbucket = 30, cp = 0.001))
fancyRpartPlot(tree6, cex = 0.7)
tree6_preds <- predict(tree6, test_data, type = 'vector')
t15 <- accuracy(tree6_preds, test_data$expectedPointsAdded)
t15
# Meh, not doing much for me
# BAGGING - WITH MOVEMENT
train_data_bag <- train_data_epa_move %>% 
                    na.omit()
bag7 <- randomForest(expectedPointsAdded ~ .,
                     data = train_data_bag,
                     mtry = 28,
                     ntree = 500)
bag7_preds <- predict(bag7, test_data, type = 'response')
t16 <- accuracy(bag7_preds, test_data$expectedPointsAdded)
t16
# Same issue
# XGBOOST - WITH MOVEMENT
# Set up dummies
train_data_xgb <- train_data_epa_move %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>%
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_epa_move <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                                   label = train_data_xgb$expectedPointsAdded)
dtest_xgb_epa_move <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 8:51)]),
                                  label = test_data_xgb$expectedPointsAdded)
# Train and Predict
xgb10 <- xgboost(data = dtrain_xgb_epa_move,
                 nrounds = 100,
                 verbose = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse')
xgb10_preds <- predict(xgb10, dtest_xgb_epa_move)
t17 <- accuracy(xgb10_preds, test_data_xgb$expectedPointsAdded)
t17
# Same thing
# Some Tuning
bst <- xgboost(data = dtrain_xgb_epa_move,
               nfold = 5,
               eta = 0.1,
               nrounds = 3000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'reg:squarederror',
               eval_metric = 'rmse')
# Goes for the whole time
# Tuning max depth and min child weight
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# RMSE Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'RMSE') +
  theme_minimal()
# Max depth of 3 and min child of 15 is best
# Tuning gamma
rmse_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 15,
                   gamma = gamma_vals[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, rmse_vec)
# All are the same, lets drop it
# Now retune the initial model with the things we know now
xgb11 <- xgboost(data = dtrain_xgb_epa_move,
                 nfold = 5,
                 eta = 0.1,
                 nrounds = 3000,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 nthread = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse',
                 max_depth = 3,
                 min_child_weight = 15)
# Just keeps going
# Tuning subsample and colsample_by_tree
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 15,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# RMSE Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'RMSE') +
  theme_minimal()
# 0.9 for both!
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_epa_move,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 15,
                    subsample = 0.9,
                    colsample_bytree = 0.9)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_epa_move,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 15,
                    subsample = 0.9,
                    colsample_bytree = 0.9)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_epa_move,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 15,
                    subsample = 0.9,
                    colsample_bytree = 0.9)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_epa_move,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 15,
                    subsample = 0.9,
                    colsample_bytree = 0.9)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_epa_move,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 15,
                    subsample = 0.9,
                    colsample_bytree = 0.9)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_rmse_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'RMSE by Iteration and Eta',
       x = 'Iteration',
       y = 'RMSE',
       color = 'Eta') +
  theme_minimal()
# Best is 0.3
xgb12 <- xgboost(data = dtrain_xgb_epa_move,
                 nfold = 5,
                 eta = 0.3,
                 nrounds = 3000,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 nthread = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse',
                 max_depth = 3,
                 min_child_weight = 15,
                 subsample = 0.9,
                 colsample_bytree = 0.9)
# Final Prediction for This One
xgb12_preds <- predict(xgb12, dtest_xgb_epa_move)
t18 <- accuracy(xgb12_preds, test_data_xgb$expectedPointsAdded)
t18
# SHAP
shap_result <- shap.score.rank(xgb_model = xgb12,
                               X_train = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(2:3, 6:49)]),
                       top_n = 10)                       
shap_epa_move <- plot.shap.summary(data_long = shap_long)
ggsave(shap_epa_move, file = "G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/shap_epa_move.jpeg", dpi = 600)

# XGBOOST - WITH MOVEMENT AND S/A ADDED
# Since XGBoost has been the best before, that will be the only model that I run here
# Set up dummies
train_data_xgb <- train_data_epa_move %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>%
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
# Set up DMatrix
dtrain_xgb_epa_move_sa <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                                      label = train_data_xgb$expectedPointsAdded)
dtest_xgb_epa_move_sa <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(4:5, 8:99)]),
                                     label = test_data_xgb$expectedPointsAdded)
# Train and Predict
xgb13 <- xgboost(data = dtrain_xgb_epa_move_sa,
                 nrounds = 100,
                 verbose = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse')
xgb13_preds <- predict(xgb13, dtest_xgb_epa_move_sa)
t19 <- accuracy(xgb13_preds, test_data_xgb$expectedPointsAdded)
t19
# Some Tuning
bst <- xgboost(data = dtrain_xgb_epa_move_sa,
               nfold = 5,
               eta = 0.1,
               nrounds = 3000,
               early_stopping_rounds = 50,
               verbose = 1,
               nthread = 1,
               print_every_n = 20,
               objective = 'reg:squarederror',
               eval_metric = 'rmse')
# Goes for the whole time
# Tuning max depth and min child weight
cv_params <- expand.grid(max_depth_vals, min_child_weight_vals)
names(cv_params) <- c('max_depth', 'min_child_weight')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = cv_params$max_depth[i],
                   min_child_weight = cv_params$min_child_weight[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$max_depth <- as.factor(res_db$max_depth)
res_db$min_child_weight <- as.factor(res_db$min_child_weight)
# RMSE Heatmap
ggplot(res_db, aes(x = max_depth, y = min_child_weight, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Max Depth and Min Child Weight',
       x = 'Max Depth',
       y = 'Min Child Weight',
       fill = 'RMSE') +
  theme_minimal()
# Max depth of 3 and min child of 1 is best
# Tuning gamma
rmse_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 1,
                   gamma = gamma_vals[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
cbind.data.frame(gamma_vals, rmse_vec)
# Not much different, let's go with 0.10
# Now retune the initial model with the things we know now
xgb14 <- xgboost(data = dtrain_xgb_epa_move_sa,
                 nfold = 5,
                 eta = 0.1,
                 nrounds = 3000,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 nthread = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse',
                 max_depth = 3,
                 min_child_weight = 1,
                 gamma = 0.10)
# Just keeps going
# Tuning subsample and colsample_by_tree
cv_params <- expand.grid(subsample_vals, colsample_bytree_vals)
names(cv_params) <- c('subsample', 'colsample_bytree')
rmse_vec <- rep(NA, nrow(cv_params))
for(i in 1:nrow(cv_params)){
  print(i)
  set.seed(102701)
  bst_tune <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                   nfold = 5,
                   eta = 0.1,
                   nrounds = 3000,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   nthread = 1,
                   print_every_n = 20,
                   objective = 'reg:squarederror',
                   eval_metric = 'rmse',
                   max_depth = 3,
                   min_child_weight = 1,
                   gamma = 0.10,
                   subsample = cv_params$subsample[i],
                   colsample_bytree = cv_params$colsample_bytree[i])
  rmse_vec[i] <- min(bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit])
}
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- 'rmse'
res_db$subsample <- as.factor(res_db$subsample)
res_db$colsample_bytree <- as.factor(res_db$colsample_bytree)
# RMSE Heatmap
ggplot(res_db, aes(x = subsample, y = colsample_bytree, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       midpoint = mean(res_db$rmse), space = 'Lab',
                       na.value = 'gray', guide = 'colorbar', aesthetics = 'fill') +
  labs(title = 'RMSE by Subsample and Colsample by Tree',
       x = 'Subsample',
       y = 'Colsample by Tree',
       fill = 'RMSE') +
  theme_minimal()
# Subsample of 1 and colsample of 0.9
# Now just to check out different eta values
bst_mod_1 <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                    nfold = 5,
                    eta = 0.1,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 1,
                    gamma = 0.10,
                    subsample = 1,
                    colsample_bytree = 0.9)
bst_mod_2 <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                    nfold = 5,
                    eta = 0.3,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 1,
                    gamma = 0.10,
                    subsample = 1,
                    colsample_bytree = 0.9)
bst_mod_3 <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                    nfold = 5,
                    eta = 0.05,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 1,
                    gamma = 0.10,
                    subsample = 1,
                    colsample_bytree = 0.9)
bst_mod_4 <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                    nfold = 5,
                    eta = 0.01,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 1,
                    gamma = 0.10,
                    subsample = 1,
                    colsample_bytree = 0.9)
bst_mod_5 <- xgb.cv(data = dtrain_xgb_epa_move_sa,
                    nfold = 5,
                    eta = 0.005,
                    nrounds = 3000,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    nthread = 1,
                    print_every_n = 20,
                    objective = 'reg:squarederror',
                    eval_metric = 'rmse',
                    max_depth = 3,
                    min_child_weight = 1,
                    gamma = 0.10,
                    subsample = 1,
                    colsample_bytree = 0.9)
# Now to plot and see
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.3', nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- 'eta'
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.1', nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- 'eta'
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.05', nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- 'eta'
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.01', nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- 'eta'
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[, c('iter', 'test_rmse_mean')],
                        rep('0.005', nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- 'eta'
plot_dat <- rbind(pd1, pd2, pd3, pd4, pd5)
ggplot(plot_dat, aes(x = iter, y = test_rmse_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  labs(title = 'RMSE by Iteration and Eta',
       x = 'Iteration',
       y = 'RMSE',
       color = 'Eta') +
  theme_minimal()
# Best is .05
xgb15 <- xgboost(data = dtrain_xgb_epa_move_sa,
                 nfold = 5,
                 eta = 0.05,
                 nrounds = 3000,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 nthread = 1,
                 print_every_n = 20,
                 objective = 'reg:squarederror',
                 eval_metric = 'rmse',
                 max_depth = 3,
                 min_child_weight = 1,
                 gamma = 0.10,
                 subsample = 1,
                 colsample_bytree = 0.9)
# Final Prediction for This One
xgb15_preds <- predict(xgb15, dtest_xgb_epa_move_sa)
t20 <- accuracy(xgb15_preds, test_data_xgb$expectedPointsAdded)
t20
# SHAP
shap_result <- shap.score.rank(xgb_model = xgb15,
                               X_train = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                               shap_approx = F)
shap_long <- shap.prep(shap = shap_result,
                       X_train = as.matrix(train_data_xgb[, c(2:3, 6:97)]),
                       top_n = 10)
shap_epa_move_sa <- plot.shap.summary(data_long = shap_long)
ggsave(shap_epa_move_sa, file = "G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/shap_epa_move_sa.jpeg", dpi = 600)
