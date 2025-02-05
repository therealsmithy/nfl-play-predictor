library(data.table)
library(dplyr)
load("G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/data/results_3.rda")
plays <- fread('G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/data/plays.csv')
players <- fread('G:/My Drive/MSBA/Sem1/ML/Final Project/nfl-play-predictor/data/players.csv')

# Pull out the data from the results list
name_vec <- colnames(res_db)
res_db <- as.data.frame(matrix(NA, nrow = length(res_list), ncol = 220))

for(i in 1:length(res_list)){
  if(!is.null(res_list[[i]])){
    res_db[i,] <- c(unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),2]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),3]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),4]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),5]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),6]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),7]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),8]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),9]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),10]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "off"),11]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),2]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),3]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),4]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),5]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),6]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),7]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),8]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),9]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),10]),
                        unlist(res_list[[i]][which(res_list[[i]][,1] == "def"),11]))
  }
}

names(res_db) <- name_vec

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
  new_colnames <- c(new_colnames, paste("o_player", i, "lineset_s", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "lineset_a", sep = "_"))
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
  new_colnames <- c(new_colnames, paste("o_player", i, "snap_s", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("o_player", i, "snap_a", sep = "_"))
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
  new_colnames <- c(new_colnames, paste("d_player", i, "lineset_s", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "lineset_a", sep = "_"))
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
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "snap_s", sep = "_"))
}
for (i in 1:num_players) {
  new_colnames <- c(new_colnames, paste("d_player", i, "snap_a", sep = "_"))
}
colnames(res_db) <- new_colnames

model_df <- cbind(plays, res_db)

# Variable Selection
# Response variable - expectedPointsAdded from plays data
# 0a. Tracking data that I want to use - a(at snap), s(at snap)
# 0b. Play data that I want to use - quarter, down, offenseFomation, recieverAlignment
# 1. Euclidian distance between line set and ball snap for each player
o_dist_moved <- sqrt((model_df[,c(117:127)] - model_df[,c(62:72)])^2 + 
                       (model_df[,c(128:138)] - model_df[,c(73:83)])^2)
names(o_dist_moved) <- paste("o_dist_moved", 1:11, sep = "_")
model_df <- cbind(model_df, o_dist_moved)

d_dist_moved <- sqrt((model_df[,c(227:237)] - model_df[,c(172:182)])^2 + 
                       (model_df[,c(238:248)] - model_df[,c(183:193)])^2)
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
colnames(model_df)[293:303] <- o_positioncolnames
colnames(model_df)[304:314] <- d_positioncolnames

# Create new variable for run vs pass
model_df <- model_df %>% 
  mutate(run_pass = ifelse(passResult %in% c('C', 'I', 'IN', 'R', 'S'), 
                           "pass", "run"))

# Select columns I will be using
model_df <- model_df %>% 
  select(playDescription, expectedPointsAdded, run_pass,
         quarter, down, offenseFormation, receiverAlignment, 
         o_dist_moved_1:o_dist_moved_11, d_dist_moved_1:d_dist_moved_11,
         o_player_1_snap_s:o_player_11_snap_s, d_player_1_snap_s:d_player_11_snap_s,
         o_player_1_snap_a:o_player_11_snap_a, d_player_1_snap_a:d_player_11_snap_a,
         o_player_1_position:d_player_11_position) %>% 
  mutate_if(is.character, as.factor)

# Dummy Up Positions
o_move_cols <- c(8:18)
o_s_cols <- c(30:40)
o_a_cols <- c(52:62)
o_pos_cols <- c(74:84)

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

# Bind with model_df
model_df <- cbind(model_df[, 1:7], model_use[, 1:24])

# Save it
save(model_df, file = "G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/model_df.rda")
