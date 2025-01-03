library(data.table)
library(dplyr)

plays <- fread('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/plays.csv')
players <- fread('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/players.csv')
# Load in res_db

# Rename columns in res_db
new_colnames <- c()
num_players <- 11
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "lineset_id", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "lineset_x", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "lineset_y", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "snap_id", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "snap_x", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "snap_y", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "lineset_id", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "lineset_x", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "lineset_y", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "snap_id", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "snap_x", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "snap_y", sep = "_"))
}
colnames(res_db) <- new_colnames

# Make small df for modeling (for now)
model_df <- plays %>% 
              slice(1:100)
model_df <- plays
# Slice up res_db (for now)
res_db <- res_db %>% 
              slice(1:100)

# Merge model_df and res_db
model_df <- cbind(model_df, res_db)

# Variable Selection
# Response variable - expectedPointsAdded from plays data
# 0a. Tracking data that I want to use - a(at snap), s(at snap), dir(at snap) - WILL ADD AT LATER DATE!
# 0b. Play data that I want to use - quarter, down, offenseFomation, recieverAlignment
# 1. Euclidian distance between line set and ball snap for each player
o_dist_moved <- sqrt((model_df[,c(95:105)] - model_df[,c(62:72)])^2 + 
                       (model_df[,c(106:116)] - model_df[,c(73:83)])^2)
names(o_dist_moved) <- paste("o_dist_moved", 1:11, sep = "_")
model_df <- cbind(model_df, o_dist_moved)

d_dist_moved <- sqrt((model_df[,c(161:171)] - model_df[,c(128:138)])^2 + 
                       (model_df[,c(172:182)] - model_df[,c(139:149)])^2)
names(d_dist_moved) <- paste("d_dist_moved", 1:11, sep = "_")
model_df <- cbind(model_df, d_dist_moved)

# Bring in player position from players data
model_df <- model_df %>% 
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_1_lineset_id" = "nflId")) %>% 
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_2_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_3_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_4_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_5_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_6_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_7_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_8_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_9_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_10_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("o_player_11_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position), 
                        by = c("d_player_1_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_2_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_3_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_4_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_5_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_6_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_7_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_8_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_9_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_10_lineset_id" = "nflId")) %>%
              left_join(players %>% 
                          select(nflId, position),
                        by = c("d_player_11_lineset_id" = "nflId"))
o_positioncolnames <- c()
for (i in 1:num_players) {
  o_positioncolnames <- c(o_positioncolnames, paste("o_player", i, "position", sep = "_"))
}
d_positioncolnames <- c()
for (i in 1:num_players) {
  d_positioncolnames <- c(d_positioncolnames, paste("d_player", i, "position", sep = "_"))
}
colnames(model_df)[205:215] <- o_positioncolnames
colnames(model_df)[216:226] <- d_positioncolnames

# Create new variable for run vs pass
model_df <- model_df %>% 
              mutate(run_pass = ifelse(passResult %in% c('C', 'I', 'IN', 'R', 'S'), 
                                       "pass", "run"))

# Select columns I will be using
model_df <- model_df %>% 
              select(playDescription, expectedPointsAdded, run_pass,
                     quarter, down, offenseFormation, receiverAlignment, 
                     o_dist_moved_1:o_dist_moved_11, d_dist_moved_1:d_dist_moved_11,
                     o_player_1_position:d_player_11_position) %>% 
              mutate_if(is.character, as.factor)

# Model Time
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
# Split data into training and test
total_obs <- nrow(model_df)
train_index <- sample(1:total_obs, 0.8 * total_obs)
train_data <- model_df[train_index,]
test_data <- model_df[-train_index,]

# FIRST - RUN VS PASS
# Random Forest - no movement added
rf_model_rp_base <- rpart(run_pass ~ quarter + down + offenseFormation + receiverAlignment, 
                          data = train_data[, 2:51])
rf_pred_rp_base <- predict(rf_model_rp_base, test_data, type = "class")
t1 <- table(test_data$run_pass, rf_pred_rp_base)
fancyRpartPlot(rf_model_rp_base)
confusionMatrix(t1, positive = 'pass')

# Bagging - no movement added
train_data_bag_rp <- train_data %>% 
                      na.omit()
bag_model_rp_base <- randomForest(run_pass ~ quarter + down + offenseFormation + receiverAlignment, 
                                  data = train_data_bag_rp[, 2:51],
                                  mtry = 4, 
                                  ntree = 200)
bag_pred_rp_base <- predict(bag_model_rp_base, test_data, type = "class")
varImpPlot(bag_model_rp_base)
t2 <- table(test_data$run_pass, bag_pred_rp_base)
confusionMatrix(t2, positive = 'pass')

# XGBoost - no movement added
# Set up
train_data_xgb_rp <- train_data %>% 
                      dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb_rp <- test_data %>%
                      dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
dtrain_rp_base <- xgb.DMatrix(data = as.matrix(train_data_xgb_rp[, c(4:5, 52:65)]), 
                              label = as.numeric(train_data_xgb_rp$run_pass) - 1)
dtest_rp_base <- xgb.DMatrix(data = as.matrix(test_data_xgb_rp[, c(4:5, 52:63)]),
                             label = as.numeric(test_data_xgb_rp$run_pass) - 1)
# Train
xgb_model_rp_base <- xgboost(data = dtrain_rp_base, 
                             nrounds = 100, 
                             objective = "binary:logistic", 
                             max_depth = 6, 
                             eta = 0.3, 
                             gamma = 0.1, 
                             min_child_weight = 1,
                             eval_metric = "auc",
                             eval_metric = 'error',
                             verbose = 1)
boost_preds_rp_base <- predict(xgb_model_rp_base, dtest_rp_base)
# SHAP
shap_values_rp_base <- xgb.importance(model = xgb_model_rp_base)
xgb.plot.importance(importance_matrix = shap_values_rp_base, top_n = 10)

# Random Forest - with movement added

# Bagging - with movement added

# XGBoost - with movement added

# SECOND - EPA Analysis
# Random Forest - no movement added
tree_model_base <- rpart(expectedPointsAdded ~ quarter + down + offenseFormation + receiverAlignment, 
                    data = train_data[, 2:50])
tree_pred <- predict(tree_model_base, test_data)
fancyRpartPlot(tree_model_base)

# Bagging - no movement added
train_data_bag <- train_data %>% 
                  na.omit()
bag_model_base <- randomForest(expectedPointsAdded ~ quarter + down + offenseFormation + receiverAlignment, 
                               data = train_data_bag[, 2:51],
                               mtry = 4, 
                               ntree = 200)
bag_pred <- predict(bag_model_base, test_data)
varImpPlot(bag_model_base)
# Watch out for what quarter it is based on this one

# XGBoost - no movement added
# Set up
library(fastDummies)
train_data_xgb <- train_data %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
test_data_xgb <- test_data %>% 
                    dummy_cols(select_columns = c("offenseFormation", "receiverAlignment"))
dtrain_base <- xgb.DMatrix(data = as.matrix(train_data_xgb[, c(3:4, 51:62)]), label = train_data_xgb$expectedPointsAdded)
dtest_base <- xgb.DMatrix(data = as.matrix(test_data_xgb[, c(3:4, 51:62)]), label = test_data_xgb$expectedPointsAdded)
# Train
xgb_model_base <- xgboost(data = dtrain_base, 
                          nrounds = 100, 
                          objective = "reg:squarederror", 
                          max_depth = 6, 
                          eta = 0.3, 
                          gamma = 0.1, 
                          min_child_weight = 1,
                          eval_metric = "rmse",
                          verbose = 1)
boost_preds_base <- predict(xgb_model_base, dtest_base)
# SHAP
shap_values_base <- xgb.importance(model = xgb_model_base)
xgb.plot.importance(importance_matrix = shap_values_base, top_n = 10)
# Once again quarter and down ... Hopefully the motion plays a part




library(fastDummies)

d_vals <- dummy_cols(model_df[, 4:ncol(model_df)], remove_selected_columns = TRUE)
model_df2 <- cbind(model_df[, 1:3], d_vals)
summary(model_df2)
head(d_vals)
total_obs <- nrow(model_df)
train_index <- sample(1:total_obs, 0.8 * nrow(model_df2))
train_data <- model_df2[train_index,]
test_data <- model_df2[-train_index,]



dtrain_base <- xgb.DMatrix(data = as.matrix(train_data[, c(4:242)]), 
                           label = train_data$expectedPointsAdded)
dtest_base <- xgb.DMatrix(data = as.matrix(test_data[, c(4:242)]), label = test_data$expectedPointsAdded)


# Train
xgb_model_base <- xgboost(data = dtrain_base, 
                          nrounds = 100, 
                          objective = "reg:squarederror", 
                          max_depth = 6, 
                          eta = 0.1, 
                          gamma = 0.1, 
                          min_child_weight = 1,
                          eval_metric = "rmse",
                          verbose = 1)
boost_preds_base <- predict(xgb_model_base, dtest_base)

shap_values_rp_base <- xgb.importance(model = xgb_model_base)
xgb.plot.importance(importance_matrix = shap_values_rp_base, top_n = 10)


write.csv(model_df, file = "model_data.csv")




names(model_df)

o_move_cols <- c(8:18)
o_pos_cols <- c(30:40)

model_df <- as.data.frame(model_df)

positions <- na.omit(unique(unlist(model_df[, o_pos_cols])))

model_df <- as.data.frame(model_df)

model_res <- as.data.frame(matrix(NA,nrow = nrow(model_df), ncol = length(positions) * 5))

for(i in 1:nrow(model_df)){
  
  for(j in 1:length(positions)){
    if(positions[j] %in% unlist(model_df[i, o_pos_cols])){
      loc <- which(unlist(model_df[i, o_pos_cols]) == positions[j])
      
      if(length(loc) > 1){
        print(paste(length(loc), positions[j]))
        
        for(x in 1:length(loc)){
          model_res[i, ((j - 1) * 5) + 1 + (x - 1)] <- model_df[i, o_move_cols[loc[x]]]
        }
        
      } else {
        model_res[i, ((j - 1) * 5) + 1] <- model_df[i, o_move_cols[loc]]
      }
    }
     
  }
}

names(model_res) <- paste(rep(positions,each =  5),1:5, sep = "_")

model_res[is.na(model_res)] <- 0

colSums(model_res)
model_use <- model_res[,which(colSums(model_res) > 0) ]




model_df2 <- cbind(model_df[, 1:3], model_use)
summary(model_df2)
head(d_vals)
total_obs <- nrow(model_df2)
train_index <- sample(1:total_obs, 0.8 * nrow(model_df2))
train_data <- model_df2[train_index,]
test_data <- model_df2[-train_index,]



dtrain_base <- xgb.DMatrix(data = as.matrix(train_data[, c(4:21)]), 
                           label = train_data$expectedPointsAdded)
dtest_base <- xgb.DMatrix(data = as.matrix(test_data[, c(4:21)]), label = test_data$expectedPointsAdded)


# Train
xgb_model_base <- xgboost(data = dtrain_base, 
                          nrounds = 100, 
                          objective = "reg:squarederror", 
                          max_depth = 6, 
                          eta = 0.1, 
                          gamma = 0.1, 
                          min_child_weight = 1,
                          eval_metric = "rmse",
                          verbose = 1)
boost_preds_base <- predict(xgb_model_base, dtest_base)

shap_values_rp_base <- xgb.importance(model = xgb_model_base)
xgb.plot.importance(importance_matrix = shap_values_rp_base, top_n = 10)


source("C:/Users/liams/Downloads/a_insights_shap_functions.r", echo=TRUE)
# Calculate SHAP importance
shap_result <- shap.score.rank(xgb_model = xgb_model_base, 
                               X_train =as.matrix(train_data[, c(4:21)]),
                               shap_approx = F)

shap_long = shap.prep(shap = shap_result,
                      X_train = as.matrix(train_data[, c(4:21)]), 
                      top_n = 10)


g_1 <- plot.shap.summary(data_long = shap_long)
ggsave(g_1, file = "shap_importance_1.jpeg", dpi = 600)

xgb_preds <-  predict(xgb_model_base, dtest_base)

plot_data <- cbind.data.frame(test_data$expectedPointsAdded, xgb_preds)
 names(plot_data) <- c("actual", "predicted")
ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point()




