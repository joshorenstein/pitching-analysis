# Download Statcast Data 


# Overview ----------------------------------------------------------------

# Getting and cleaning the Statcast data is primarily the work of Ethan Moore and Bill Petti
# https://github.com/ethanmoore21/PitchQuality/blob/master/xRV3_2021.R

# Josh  additions are:
# 1) adding a spin mirroring variable (spin_dir_diff) &
# 2) a dummy variable for whiffs
# 3) Added WOBA Run Values from Dan Meyer's work on Hardball Times

# Libraries --------------------------------------------------------------

start <- proc.time()

library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(lubridate)
library(here)


set.seed(5818)

# Data Acquisition from BaseballSavant.com -------------------------------

# baseballR function scrape_statcast_savant() only returns 40,000 pitches 
# at a time, so we must break the 2020 season up into several date ranges 
# to acquire all the data 

#you may also want to load in savant data from previous seasons as well

data1 = scrape_statcast_savant(start_date = "2020-07-23",
                               end_date = "2020-08-01",
                               player_type = "pitcher")

data2 = scrape_statcast_savant(start_date = "2020-08-02",
                               end_date = "2020-08-10",
                               player_type = "pitcher")

data3 = scrape_statcast_savant(start_date = "2020-08-11",
                               end_date = "2020-08-19",
                               player_type = "pitcher")

data4 = scrape_statcast_savant(start_date = "2020-08-20",
                               end_date = "2020-08-29",
                               player_type = "pitcher")

data5 = scrape_statcast_savant(start_date = "2020-08-30",
                               end_date = "2020-09-06",
                               player_type = "pitcher")

data6 = scrape_statcast_savant(start_date = "2020-09-07",
                               end_date = "2020-09-14",
                               player_type = "pitcher")

data7 = scrape_statcast_savant(start_date = "2020-09-15",
                               end_date = "2020-09-22",
                               player_type = "pitcher")

data8 = scrape_statcast_savant(start_date = "2020-09-23",
                               end_date = "2020-09-29",
                               player_type = "pitcher")

mlbraw1 = rbind(data1, data2, data3, data4, data5, data6, data7, data8) #bind all dataframes into one

mlbraw1 %>% write_csv("data/raw_data.csv")
rm(data1, data2, data3, data4, data5, data6, data7, data8) #remove individual dataframes


#add in spin direction from Bill Petti: https://t.co/hS48QqHtaF?amp=1 
#we only have this column for the 2020 season, so skip this if you are using data outside of 2020 and 
#remove release_spin_direction from feature list in feature_selection() function

#Add Bill Petti's spin direction data
spin_direction_pbp <- read.csv("data/spin_direction_pbp.csv")

#Add Dan Meyer's WOBA run values from Hardball Times - 
# https://bit.ly/3sLtPXA 

run_value <- read_csv("data/run_value.csv")
head(spin_direction_pbp)
#names(spin_direction_pbp)[3] = "player_name"

mlbraw <- mlbraw1 %>%
  left_join(spin_direction_pbp,by=c("game_pk","batter","pitcher","pitch_number","inning"))

names(mlbraw)
# Data Cleaning and Feature Creation -------------------------------------

#Honestly my comments here are going to be sparse because I wrote this code a long time ago.
#Essentially, we are just creating our response variable here. Skip to the next section if you
# don't particularly care how this is done!

#Derive Linear Weights for Balls in Play
mlbraw$des2 = NA
mlbraw$des2[grepl("single", mlbraw$des)] = "single"
mlbraw$des2[grepl("doubles", mlbraw$des)] = "double"
mlbraw$des2[grepl("ground-rule double", mlbraw$des)] = "double"
mlbraw$des2[grepl("triple", mlbraw$des)] = "triple"
mlbraw$des2[grepl("homer", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("grand slam", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("home run", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("reaches on a throwing error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("reaches on a fielding error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("reaches on a missed catch error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("hit by pitch", mlbraw$des)] = "hit_by_pitch"
mlbraw$des2[grepl("walk", mlbraw$des)] = "walk"
mlbraw$des2[grepl("strikes out", mlbraw$des)] = "strikeout"
mlbraw$des2[grepl("on strikes", mlbraw$des)] = "strikeout"
mlbraw$des2[grepl("sacrifice fly", mlbraw$des)] = "sac_fly"
mlbraw$des2[grepl("fielder's choice", mlbraw$des)] = "fielders_choice"
mlbraw$des2[grepl("force out", mlbraw$des)] = "fielders_choice"
mlbraw$des2[grepl("double play", mlbraw$des)] = "double_play"

mlbraw$des2[grepl("flies out", mlbraw$des) | 
              grepl("grounds out", mlbraw$des) |
              grepl("lines out", mlbraw$des) |
              grepl("pops out", mlbraw$des) |
              grepl("out on a sacrifice bunt", mlbraw$des)] = "field_out"

des_subset = mlbraw[!is.na(mlbraw$des2),]

des_subset$on_1b = ifelse(!is.na(des_subset$on_1b), 1, 0)
des_subset$on_2b = ifelse(!is.na(des_subset$on_2b), 1, 0)
des_subset$on_3b = ifelse(!is.na(des_subset$on_3b), 1, 0)

a = str_count(des_subset$des, "score")
b = str_count(des_subset$des, "homer")
c = str_count(des_subset$des, "grand slam")
a[is.na(a)] = 0
b[is.na(b)] = 0
c[is.na(c)] = 0

des_subset$runs_scored = a + b + c

des_subset$date1 = as.Date(des_subset$game_date)

des_subset = des_subset%>%
  arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)


#get Linear Weights table using baseballR
mlbraw2 = mlbraw%>%
  run_expectancy_code()

re_table <- mlbraw2%>%
  run_expectancy_table()

Season_RE = data.frame(matrix(ncol = 5, nrow = nrow(re_table)))
names(Season_RE) = c("outs_when_up", "on_1b", "on_2b", "on_3b", "RE")
library(stringr)

for (i in 1:nrow(re_table)){
  
  Season_RE$outs_when_up[i] = str_split(
    re_table$base_out_state, " ")[[i]][[1]]
  
  Season_RE$on_1b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[5]] == "1b", 1, 0)
  
  Season_RE$on_2b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[6]] == "2b", 1, 0)
  
  Season_RE$on_3b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[7]] == "3b", 1, 0)
  
}

Season_RE$RE = re_table$avg_re
Season_RE$outs_when_up = as.integer(Season_RE$outs_when_up)

mlb_re <- left_join(des_subset, Season_RE, by=c("on_1b", "on_2b", "on_3b", "outs_when_up"))

mlb_re$RE_after = ifelse(mlb_re$inning_topbot == lead(mlb_re$inning_topbot), lead(mlb_re$RE, 1), 0)
mlb_re$RE_diff = mlb_re$RE_after - mlb_re$RE

mlb_re$playRE = mlb_re$RE_diff + mlb_re$runs_scored

# mlb_re%>%
#   select(on_1b, on_2b, on_3b, outs_when_up, events, RE, RE_after, RE_diff, runs_scored, playRE)%>%
#   head(15) #makes sure it worked

mlb_LW <- mlb_re%>%
  group_by(des2)%>%
  summarise(lin_weight = mean(playRE, na.rm = T), count = n())%>%
  arrange(desc(lin_weight))

#print(mlb_LW, n=50)

#Derive Linear Weights for Non-Balls in Play

mlb2 = mlbraw
mlb2$des2[grepl("strike", mlbraw$description)] = "strike"
mlb2$des2[grepl("missed bunt", mlbraw$description)] = "strike"
mlb2$des2[grepl("ball", mlbraw$description)] = "ball"
mlb2$des2[grepl("pitchout", mlbraw$description)] = "ball"
#mlb2$des2[grepl("foul", mlbraw$description)] = "foul"  #treat foul balls the same as strikes
mlb2$des2[grepl("foul", mlbraw$description)] = "strike"
# mlb2$des2[grepl("walk", mlbraw$des)] = "walk"
# mlb2$des2[grepl("strikes out", mlbraw$des)] = "strikeout"

mlb2$des2[grepl("walk", mlbraw$des)] = "ball" #treat pitches ending in walks as balls
mlb2$des2[grepl("strikes out", mlbraw$des)] = "strike" #treat pitches ending in strikeouts as strikes


mlb2$on_1b = ifelse(!is.na(mlb2$on_1b), 1, 0)
mlb2$on_2b = ifelse(!is.na(mlb2$on_2b), 1, 0)
mlb2$on_3b = ifelse(!is.na(mlb2$on_3b), 1, 0)

a = str_count(mlb2$des, "score")
b = str_count(mlb2$des, "homer")
c = str_count(mlb2$des, "grand slam")
a[is.na(a)] = 0
b[is.na(b)] = 0
c[is.na(c)] = 0

mlb2$runs_scored = a + b + c

mlb2$date1 = as.Date(mlb2$game_date)

mlb2 = mlb2%>%
  arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)

mlb_re2 <- left_join(mlb2, Season_RE, by=c("on_1b", "on_2b", "on_3b", "outs_when_up"))

mlb_re2$RE_after = ifelse(mlb_re2$inning_topbot == lead(mlb_re2$inning_topbot), lead(mlb_re2$RE, 1), 0)
mlb_re2$RE_diff = mlb_re2$RE_after - mlb_re2$RE

mlb_re2$playRE = mlb_re2$RE_diff + mlb_re2$runs_scored

mlb_re2$playRE[mlb_re2$playRE == 0] = NA

mlb_re2= mlb_re2%>%fill(playRE, .direction = "up")

COUNT_RE_Matrix = mlb_re2%>%
  filter(balls != 4)%>%
  group_by(balls, strikes)%>%
  summarise(count_RE = mean(playRE, na.rm=T)) #Run Expectancy by count

COUNT_RE_Matrix%>%
  arrange(desc(count_RE))

#Finding linear weights of balls and strikes

mlb_re3 = left_join(mlb_re2, COUNT_RE_Matrix, by=c("balls", "strikes"))

mlb_re3$count_RE_after = ifelse(mlb_re3$at_bat_number == lead(mlb_re3$at_bat_number), lead(mlb_re3$count_RE, 1), 0)
mlb_re3$count_RE_diff = mlb_re3$count_RE_after - mlb_re3$count_RE

mlb_re3$count_playRE = mlb_re3$count_RE_diff + mlb_re3$runs_scored

BALL_STRIKE_LW = mlb_re3%>%
  filter(des2 %in% c("ball", "strike"))%>%
  group_by(des2)%>%
  summarise(lin_weight = mean(count_playRE, na.rm = T), count = n())%>%
  arrange(desc(lin_weight))

mlb_LW = rbind(mlb_LW, BALL_STRIKE_LW)

season_mlb = left_join(mlb2, mlb_LW[,1:2], by = "des2")

#give pitcher credit for only 1 out on situation-based outcomes
season_mlb$lin_weight[season_mlb$des2 == "field_error"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "double_play"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "fielders_choice"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "sac_fly"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]

season_mlb = season_mlb%>%
  filter(!is.na(des2))

head(season_mlb)

rm(a,b,c,mlb_LW, mlb_re, mlb_re2, mlb_re3, mlb2, mlbraw, mlbraw2, BALL_STRIKE_LW)

#Creation of variables like velo_diff and movement diff based on a pitcher's average fastball.
p_avgs <- season_mlb%>%
  filter(pitch_type %in% c("FF", "SI"))%>%
  group_by(player_name)%>%
  summarise(avg_velo = mean(release_speed,na.rm=T),
            avg_vmov = mean(pfx_z,na.rm=T),
            avg_hmov = mean(pfx_x,na.rm=T),
            avg_spin_dir = mean(release_spin_direction,na.rm=T)) #added spin 

#get fb averages columns
season_mlb3 <- left_join(p_avgs, season_mlb,  by = "player_name")

#get pitch differences
season_mlb4 <- season_mlb3%>%
  mutate(
    velo_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      release_speed - avg_velo, NA),
    hmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_x - avg_hmov, NA),
    vmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_z - avg_vmov, NA),
    spin_dir_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      abs(release_spin_direction - avg_spin_dir), NA)
  )

#manual fix of this for some reason

season_mlb4$velo_diff[is.na(season_mlb4$velo_diff)] = season_mlb4$release_speed[is.na(season_mlb4$velo_diff)] - 
  season_mlb4$avg_velo[is.na(season_mlb4$velo_diff)]

season_mlb4$hmov_diff[is.na(season_mlb4$hmov_diff)] = season_mlb4$pfx_x[is.na(season_mlb4$hmov_diff)] - 
  season_mlb4$avg_hmov[is.na(season_mlb4$hmov_diff)]

season_mlb4$vmov_diff[is.na(season_mlb4$vmov_diff)] = season_mlb4$pfx_z[is.na(season_mlb4$vmov_diff)] - 
  season_mlb4$avg_vmov[is.na(season_mlb4$vmov_diff)]

season_mlb4$spin_dir_diff[is.na(season_mlb4$spin_dir_diff)] = season_mlb4$release_spin_direction[is.na(season_mlb4$spin_dir_diff)] - 
  season_mlb4$avg_spin_dir[is.na(season_mlb4$spin_dir_diff)]

rm(season_mlb, season_mlb3, des_subset)  #get rid of dataframes we don't need anymore

#flip horizontal release position, horizontal movement, and spin direction measurements for LHP
#remove NA values, Random Forest does not like those!
season_mlb5 <- season_mlb4%>%
  filter(!is.na(lin_weight),
         !is.na(p_throws),
         !is.na(stand),
         !is.na(release_spin_rate),
         !is.na(release_extension),
         !is.na(release_spin_direction))%>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x),
         spin_dir_adj = ifelse(p_throws == "R", release_spin_direction, -release_spin_direction)) 

head(season_mlb5)
names(season_mlb5)
rm(season_mlb4)
names(season_mlb5)

#add whiffs and select columns for model
s <- season_mlb5 %>% 
  inner_join(run_value) %>% 
  mutate(est_woba = ifelse(des2 %in% c("ball"),woba_ball,
                           ifelse(description %in% c("strike"),woba_strike,estimated_woba_using_speedangle))) %>% #im going to leave this broken for now
  dplyr::select(player_name,pitch_type,game_date,release_speed,release_pos_x,balls,strikes,events,description,
         release_pos_y,release_pos_z,batter,pitcher,stand,p_throws,release_spin_rate,release_extension,release_spin_direction,lin_weight,
         velo_diff,hmov_diff,vmov_diff,spin_dir_diff,pfx_x,pfx_z,plate_x,plate_z,spin_dir_adj,est_woba,estimated_ba_using_speedangle,
         estimated_woba_using_speedangle,launch_speed,launch_angle,barrel,des2) %>% 
  mutate(whiff=(if_else(description %in% c("swinging_strike_blocked","swinging_strike","missed_bunt"),1,0)))


s %>% write_csv('scraps/raw_data.csv') #if you want to save the data uncomment this



