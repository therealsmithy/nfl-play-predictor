library(data.table)

# Had to unzip, don't think I need to run this again
# unzip('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/nfl-big-data-bowl-2025.zip', 
#      exdir = 'G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data')

# Data read in for early visualizing
week1 <- fread('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/tracking_week_1.csv')

# Look at one play
play_ids <- week1$playId[1]
i <- 1
play_data <- week1[which(week1$playId == week1$playId[i] &
                          week1$gameId == week1$gameId[i]),]

library(ggplot2)
library(ggdark)
ggplot(play_data, aes(y = x, x = y, color = club)) +
  geom_point(aes(alpha = frameId)) +
  dark_theme_bw()

# Check out the next play
i <- 3750
play_data2 <- week1[which(week1$playId == week1$playId[i] &
                          week1$gameId == week1$gameId[i]),]

# Get the description for this play
plays <- fread('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/plays.csv')
play_des <- plays[which(plays$playId == week1$playId[i] &
                          plays$gameId == week1$gameId[i]),]

library(ggrepel)
p2 <- ggplot(play_data2, aes(y = y, x = x, color = club)) +
        geom_point(aes(alpha = frameId)) +
        dark_theme_bw() +
        geom_text_repel(data = play_data2[!is.na(play_data2$event) &
                                     play_data2$club == "football",], aes( label = event)) +
        labs(title = 'A Seahawks Pass Play', subtitle = play_des$playDescription)

# Let's try to animate
library(gganimate)
p2animate <- p2 + transition_time(frameId)
anim_save('seahawks_pass.gif')

# Check out expected points added
quantile(plays$expectedPointsAdded, probs = c(0.25, 0.5, 0.75))
plays$quartile <- cut(plays$expectedPointsAdded, 
                      breaks = quantile(plays$expectedPointsAdded, probs = 0:4/4, na.rm = TRUE),
                      include.lowest = TRUE,
                      labels = c('Q1', 'Q2', 'Q3', 'Q4'))

# # NOTE: These density plots are not interesting nor do they give me any idea of what
# # I am going to do moving forward.
# 
# # Distribution of expected points added based on different formations
# ggplot(plays) +
#   geom_density(aes(x = offenseFormation, fill = quartile, 
#                    group = expectedPointsAdded)) +
#   dark_theme_bw()
# 
# # Distribution of different formations
# library(plyr)
# count(plays$offenseFormation)
# ggplot(plays) +
#   geom_density(aes(x = offenseFormation, fill = expectedPointsAdded, 
#                    group = expectedPointsAdded)) +
#   dark_theme_bw()
# 
# # Distribution of different receiver alignment
# count(plays$receiverAlignment)
# ggplot(plays) +
#   geom_density(aes(x = receiverAlignment, fill = passResult)) +
#   dark_theme_bw()
# 
# # Distribution of dropback type
# count(plays$dropbackType)
# ggplot(plays) +
#   geom_density(aes(x = dropbackType, fill = passResult)) +
#   dark_theme_bw()
# 
# # Distribution of pass location type
# count(plays$passLocationType)
# ggplot(plays) +
#   geom_density(aes(x = passLocationType, fill = passResult)) +
#   dark_theme_bw()
# 
# # Distribution of rush location type
# count(plays$rushLocationType)
# ggplot(plays) +
#   geom_density(aes(x = rushLocationType, fill = pff_manZone)) +
#   dark_theme_bw()
# 
# # Distribution of pass coverage
# count(plays$pff_passCoverage)
# ggplot(plays) +
#   geom_density(aes(x = pff_passCoverage, fill = dropbackType)) +
#   dark_theme_bw()
# 
# # Distribution of man vs zone
# count(plays)
# ggplot(plays) +
#   geom_density(aes(x = pff_manZone, fill = dropbackType)) +
#   dark_theme_bw()

# Here is the interesting stuff again
# Read in player_plays
player_plays <- fread('G:/My Drive/MSBA/ML/Final Project/nfl-play-predictor/data/player_play.csv')
summary(player_plays)

# Check out player plays
library(dplyr)
ran_route <- player_plays %>% 
                filter(wasRunningRoute == 1)
summary(ran_route$receivingYards)

ggplot(ran_route) +
  geom_density(aes(x = receivingYards, fill = motionSinceLineset)) +
  dark_theme_bw()

# How many times was a player in motion and have a reception?
motion_reception <- player_plays %>% 
                      filter(hadPassReception == 1) %>%
                      filter(motionSinceLineset == 1)
summary(motion_reception$receivingYards)

# And let's see one of these plays
play_data3 <- week1[which(week1$playId == 236 &
                          week1$gameId == 2022090800),]
description_bills <- plays[which(plays$playId == 236 &
                             plays$gameId == 2022090800),]
p3 <- ggplot(play_data3, aes(y = y, x = x, color = club)) +
        geom_point(aes(alpha = frameId)) +
        dark_theme_bw() +
        geom_text_repel(data = play_data3[!is.na(play_data3$event) &
                                     play_data3$club == "football",], aes( label = event)) +
        labs(title = 'Motion for the Score', subtitle = description_bills$playDescription)
p3animate <- p3 + transition_time(frameId)

# The motion is there. Now I know I can track it and visualize it multiple ways.