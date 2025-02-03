library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
# Read in data
plays <- fread('plays.csv')
player_plays <- fread('player_play.csv')
tracking <- fread('tracking_week_1.csv')

# Will use later to add player names to results to make it understandable
players_full <- fread('players.csv')

# Check out data
play_id <- tracking$playId[1]
game_id <- tracking$gameId[1]

j <- 9
i <- 1
res_db <- as.data.frame(matrix(NA, nrow = nrow(plays), ncol = 132))
res_list <- vector(mode = "list", length = nrow(plays))
# Pull motion data
for(j in 1:nrow(plays)){
  if(plays$gameId[j] %in% tracking$gameId){
    print(j)
    play_id <- plays$playId[j]
    game_id <- plays$gameId[j]
    
    players <- player_plays[which(playId == play_id & gameId == game_id),]
    
    off_players <- players[which(players$team == plays$possessionTeam[j]),]
    def_players <- players[which(players$team != plays$possessionTeam[j]),]
    
    start_pos_off <- start_pos_def <- snap_pos_off <- snap_pos_def <- as.data.frame(matrix(NA, nrow = 11, ncol = 3))
    
    
    track_use <- tracking[which(playId == play_id & gameId == game_id),]
    if("line_set" %in% track_use$event){
      for(i in 1:11){
        track_1 <- tracking[which(playId == play_id & 
                                    gameId == game_id & 
                                    nflId == off_players$nflId[i]),]
        
        start_pos_off[i, ] <- track_1[which(track_1$event  == "line_set"), c("nflId", "x", "y") ]
        snap_pos_off[i, ] <- track_1[which(track_1$event  %in% c("ball_snap", "snap_direct")), c("nflId", "x", "y") ]
        
        track_pre <- tracking[which(track_1$event  == "line_set"):which(track_1$event %in% c("ball_snap", "snap_direct")), ]
        
        track_2 <- tracking[which(playId == play_id & 
                                    gameId == game_id & 
                                    nflId == def_players$nflId[i]),]
        
        start_pos_def[i, ] <- track_2[which(track_2$event  == "line_set"), c("nflId", "x", "y") ]
        snap_pos_def[i, ] <- track_2[which(track_2$event  %in% c("ball_snap", "snap_direct")), c("nflId", "x", "y") ]
      }
      
      names(start_pos_off) <- c("nflId_start_off", "x_start_off", "y_start_off")
      names(snap_pos_off) <- c("nflId_snap_off", "x_snap_off", "y_snap_off")
      names(snap_pos_def) <- c("nflId_snap_def", "x_snap_def", "y_snap_def")
      names(start_pos_def) <- c("nflId_start_def", "x_start_def", "y_start_def")
      
      res_db[i, ] <- c(unlist(start_pos_off), unlist(snap_pos_off), unlist(start_pos_def), unlist(snap_pos_def))
      
      the_play <- res_db[i, ]
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
      the_play_off <- cbind(df1, df2, df3, df4, df5, df6)
      the_play_def <- cbind(df7, df8, df9, df10, df11, df12)
      the_play_df <- rbind(the_play_off, the_play_def)
      colnames(the_play_df) <- c('nflId', 'x_start', 'y_start', 'nflId_snap', 'x_snap', 'y_snap')
      poss <- c(rep('off', 11), rep('def', 11))
      the_play_df <- cbind(poss, the_play_df)
      the_play_df <- data.frame(the_play_df)
      the_play_df <- the_play_df %>% 
        mutate_at(c('nflId', 'x_start', 'y_start', 'nflId_snap',
                    'x_snap', 'y_snap'), 
                  as.numeric)
      
      res_list[[i]] <- the_play_df
    }
    }
    
    

}

save(res_db, res_list, file = "results_1.rda")