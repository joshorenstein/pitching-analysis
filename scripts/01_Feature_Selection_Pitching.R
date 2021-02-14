#Load some libraries
library(tidyverse)
library(tidymodels)
library(MASS)
library(VIF)
library(car)
library(broom)
head(s)
s %>% distinct(description)
#s <- read_csv('scraps/raw_data.csv') #if you need to read the data in 
sp <- s %>% dplyr::filter(description %in% c("called_strike","swinging_strike",
                                             "swinging_strike_blocked","foul_tip",
                                             "foul","foul_bunt")) #data for whiff model

df <- sp %>%   #data for fb whiff model %>% 
  #filter(pitch_type %in% c("FF","SI")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x), #create interaction variable for release side/horz break
         ht_i=abs(release_pos_z*pfx_z)) #create interaction variable for release height/vert break

df %>% group_by(whiff) %>% count() #see how many whiffs there are

df$whiff <- as.factor(df$whiff) #make the binary whiff variable a factor

# Make sure that you get the same random numbers
smp_size <- floor(1 * nrow(df)) #just going to use 2020 data as training and wait until '21 season for test data


## set the seed to make your partition reproducible
set.seed(61919)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df[train_ind, ]
#test <- df[-train_ind, ] #commented out until '21 season
nrow(train)/nrow(df)

#Make a dataset with all the potential variables for fastball model
train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                 release_pos_z,
                                 release_speed,release_spin_rate,release_spin_direction,plate_x,
                                 plate_z,
                                 pfx_x,pfx_z,whiff) %>% 
                          mutate(release_pos_x=abs(release_pos_x),
                                 release_pos_z=abs(release_pos_z),
                                 ht_i =release_pos_z*pfx_z,
                                 sd_i = release_pos_x*pfx_x)


#make a function to grab data on pitch type/p_throws/stand level

f <- function(data,type,pitcher,batter){
  data %>% filter(type==pitch_type&p_throws==pitcher&stand==batter)}

#Grab some data
ff_r <- f(train_select,"FF","R","R")
ff_l <- f(train_select,"FF","L","R")
si_l <- f(train_select,"SI","R","L")

#Variance Inflation Factor Test - RHP vs RHB
v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
            release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z+
            sd_i+ht_i,data=ff_r, family=binomial)

vif(v_check) #interaction variables > 10

#remove interactions
v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z,data=ff_r, family=binomial)
vif(v_check2) #looks much better

#double check results - RHP vs LHB
v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z++plate_x+plate_z,data=ff_l, family=binomial)
vif(v_check3)
#Check these results on the sinker data
v_check4 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z,data=si_l, family=binomial)

vif(v_check4)
#take the interactions out of fastball models
train_select <- train_select %>% dplyr::select(-c(
         ht_i,
         sd_i))

#Run same test for breaking balls/cutters/changeups
df_b <- sp %>%   #data for fb whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","CH","FC")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))

#Grab potential variables for breaking ball models
cu_train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                        release_pos_z,
                                        release_speed,release_spin_rate,release_spin_direction,
                                        hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                        pfx_x,pfx_z,whiff,sd_i,ht_i,plate_x,plate_z) %>% 
                                    mutate(release_pos_x=abs(release_pos_x),
                                           release_pos_z=abs(release_pos_z))

#Grab some curveball data
cu_r <- f(cu_train_select,"CU","R","R")
cu_l <- f(cu_train_select,"CU","L","R")
fc_l <- f(cu_train_select,"FC","R","L")

v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check) #remove spin_dir_diff and interactions

v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check2) #remove vmov_diff

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check3) #looks good 

#double check with LHP
v_check4 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=cu_l, family=binomial)
vif(v_check4) 

#Double Check Sliders
sl_r <- f(cu_train_select,"SL","R","R")

v_check5 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=sl_r, family=binomial)
vif(v_check5)

v_check6<- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=sl_r, family=binomial)
vif(v_check6)
#Check Changeups
ch_r <- f(cu_train_select,"CH","R","R")

ch_l <- f(cu_train_select,"CH","L","R")

v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=ch_r, family=binomial)

vif(v_check) #remove spin_dir_diff

v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=ch_r, family=binomial)

vif(v_check2) #remove vmov_diff

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=ch_l, family=binomial)

vif(v_check3)

#Ideal models
#FB MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z
#CU/SL MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z
#CH MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z
