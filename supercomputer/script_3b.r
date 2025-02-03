library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
# Read in data
plays <- fread('plays.csv')
player_plays <- fread('player_play.csv')
tracking_1 <- fread('tracking_week_1.csv')
tracking_2 <- fread('tracking_week_2.csv')
tracking_3 <- fread('tracking_week_3.csv')
tracking_4 <- fread('tracking_week_4.csv')
tracking_5 <- fread('tracking_week_5.csv')
tracking_6 <- fread('tracking_week_6.csv')
tracking_7 <- fread('tracking_week_7.csv')
tracking_8 <- fread('tracking_week_8.csv')
tracking_9 <- fread('tracking_week_9.csv')

tracking <- rbind.data.frame(tracking_1, tracking_2, 
                             tracking_3, tracking_4,
                             tracking_5, tracking_6, 
                             tracking_7, tracking_8, 
                             tracking_9)

# Will use later to add player names to results to make it understandable
players_full <- fread('players.csv')

# Check out data
play_id <- tracking$playId[1]
game_id <- tracking$gameId[1]

j <- 329
i <- 1
res_db <- as.data.frame(matrix(NA, nrow = nrow(plays), ncol = 220))
res_list <- vector(mode = "list", length = nrow(plays))
# Pull motion data
for(j in 1:nrow(plays)){
  if(plays$gameId[j] %in% tracking$gameId){
    
    play_id <- plays$playId[j]
    game_id <- plays$gameId[j]
    print(j)
    players <- player_plays[which(playId == play_id & gameId == game_id),]
    
    off_players <- players[which(players$team == plays$possessionTeam[j]),]
    def_players <- players[which(players$team != plays$possessionTeam[j]),]
    
    start_pos_off <- start_pos_def <- snap_pos_off <- snap_pos_def <- as.data.frame(matrix(NA, nrow = 11, ncol = 5))
    track_use <- tracking[which(playId == play_id & gameId == game_id),]
    if("line_set" %in% track_use$event){
      for(i in 1:11){
        if(nrow(off_players) >= i){
          track_1 <- tracking[which(playId == play_id &
                                      gameId == game_id &
                                      nflId == off_players$nflId[i]),]
          
          start_pos_off[i, ] <- track_1[which(track_1$event  == "line_set"), c("nflId", "x", "y", "s", "a") ]
          snap_pos_off[i, ] <- track_1[which(track_1$event  %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")), c("nflId", "x", "y", "s", "a") ]
          
        } else {
          start_pos_off[i, ] <- NA
          snap_pos_off[i, ] <- NA
        }
        
        track_pre <- tracking[which(track_1$event  == "line_set"):which(track_1$event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")), ]
        if(nrow(def_players) >= i){
          track_2 <- tracking[which(playId == play_id &
                                      gameId == game_id &
                                      nflId == def_players$nflId[i]),]
          
          start_pos_def[i, ] <- track_2[which(track_2$event  == "line_set"), c("nflId", "x", "y", "s", "a") ]
          snap_pos_def[i, ] <- track_2[which(track_2$event  %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")), c("nflId", "x", "y", "s", "a") ]
        } else {
          start_pos_def[i, ] <- NA
          snap_pos_def[i, ] <- NA
        }
        
      }
      
      names(start_pos_off) <- c("nflId_start_off", "x_start_off", "y_start_off", "s_start_off", "a_start_off")
      names(snap_pos_off) <- c("nflId_snap_off", "x_snap_off", "y_snap_off",  "s_start_off", "a_start_off")
      names(snap_pos_def) <- c("nflId_snap_def", "x_snap_def", "y_snap_def",  "s_snap_def", "a_snap_def")
      names(start_pos_def) <- c("nflId_start_def", "x_start_def", "y_start_def",  "s_start_def", "a_start_def")
      
      res_db[j, ] <- c(unlist(start_pos_off), unlist(snap_pos_off), unlist(start_pos_def), unlist(snap_pos_def))
      
      the_play <- res_db[j, ]
      the_play <- as.data.frame(t(the_play))
      df1 <- the_play[1:11, ]
      df2 <- the_play[12:22, ]
      df3 <- the_play[23:33, ]
      df4 <- the_play[34:44, ]
      df5 <- the_play[45:55, ]
      df6 <- the_play[56:66, ]
      df7 <- the_play[67:77, ]
      df8 <- the_play[78:88, ]
      df9 <- the_play[89:99, ]
      df10 <- the_play[100:110, ]
      df11 <- the_play[111:121, ]
      df12 <- the_play[122:132, ]
      df13 <- the_play[133:143, ]
      df14 <- the_play[144:154, ]
      df15 <- the_play[155:165, ]
      df16 <- the_play[166:176, ]
      df17 <- the_play[177:187, ]
      df18 <- the_play[188:198, ]
      df19 <- the_play[199:209, ]
      df20 <- the_play[210:220, ]
      the_play_off <- cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)
      the_play_def <- cbind(df11, df12, df13, df14, df15, df16, df17, df18, df19, df20)
      the_play_df <- rbind(the_play_off, the_play_def)
      colnames(the_play_df) <- c('nflId', 'x_start', 'y_start', 
                                 's_start', 'a_start', 'nflId_snap', 
                                 'x_snap', 'y_snap', 's_snap', 'a_snap')
      poss <- c(rep('off', 11), rep('def', 11))
      the_play_df <- cbind(poss, the_play_df)
      the_play_df <- data.frame(the_play_df)
      the_play_df <- the_play_df %>%
        mutate_at(c('nflId', 'x_start', 'y_start', 's_start', 'a_start', 
                    'nflId_snap', 'x_snap', 'y_snap', 's_snap', 'a_snap'),
                  as.numeric)
      
      
    }
    res_list[[j]] <- the_play_df
  }
  save(res_db, res_list, file = "results_3_part_way.rda")
}


save(res_db, res_list, file = "results_3.rda")