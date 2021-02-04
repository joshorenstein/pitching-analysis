library(tidyverse)
library(tidymodels)
library(MASS)
library(VIF)
library(car)
library(broom)

#s <- read_csv('raw_data.csv')
sp <- s %>% filter(des2 %in% c("ball","strike")) #data for whiff model

df <- sp %>%   #data for fb whiff model %>% 
  #filter(pitch_type %in% c("FF","FC","SI")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))

df %>% group_by(whiff) %>% count()

df$whiff <- as.factor(df$whiff)

# Make sure that you get the same random numbers
smp_size <- floor(0.5 * nrow(df))
smp_size

## set the seed to make your partition reproductible
set.seed(61919)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df[train_ind, ]
test <- df[-train_ind, ]
nrow(train)/nrow(df)

train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                 release_pos_z,
                                 release_speed,release_spin_rate,release_spin_direction,
                                 pfx_x,pfx_z,whiff) %>% 
  mutate(release_pos_x=abs(release_pos_x),
         release_pos_z=abs(release_pos_z),
         ht_i =release_pos_z*pfx_z,
         sd_i = release_pos_x*pfx_x)
#make a function to grab data 
f <- function(data,type,pitcher,batter){
  data %>% filter(type==pitch_type&p_throws==pitcher&stand==batter)}

ff_r <- f(train_select,"FF","R","R")
ff_l <- f(train_select,"FF","L","R")
fc_l <- f(train_select,"FC","R","L")
dim(fc_l)
#Variance Inflation Factor Test - RHP vs RHB
v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
            release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i,data=ff_r, family=binomial)

vif(v_check) #interaction variables > 10

#remove interactions
v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z,data=ff_r, family=binomial)
vif(v_check2) #looks much better

#double check results - RHP vs LHB

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z,data=ff_l, family=binomial)
vif(v_check3)

v_check4 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z,data=fc_l, family=binomial)

vif(v_check4)
#take the interactions out of fastball models
train_select <- train_select %>% dplyr::select(-c(
         ht_i,
         sd_i))

#Run same test for breaking balls
df_b <- sp %>%   #data for fb whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","CH","FC")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))
#dim(sp)
#View(sp)

cu_train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                        release_pos_z,
                                        release_speed,release_spin_rate,release_spin_direction,
                                        hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                        pfx_x,pfx_z,whiff,sd_i,ht_i) %>% 
  mutate(release_pos_x=abs(release_pos_x),
         release_pos_z=abs(release_pos_z))
head(cu_train_select)
dim(cu_train_select)
cu_train_select %>% distinct(pitch_type)
cu_r <- f(cu_train_select,"CU","R","R")

cu_l <- f(cu_train_select,"CU","L","R")
fc_l <- f(cu_train_select,"FC","R","L")
v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff,data=cu_r, family=binomial)

vif(v_check) #remove spin_dir_diff and interactions

v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff,data=cu_r, family=binomial)

vif(v_check2) #remove vmov_diff

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff,data=cu_r, family=binomial)

vif(v_check3)

#double check with LHP
v_check4 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff,data=cu_l, family=binomial)
vif(v_check4) 

#Double Check Sliders
sl_r <- f(cu_train_select,"SL","R","R")

v_check5 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff,data=sl_r, family=binomial)
vif(v_check5)

v_check6<- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff,data=sl_r, family=binomial)
#Check Changeups
ch_r <- f(cu_train_select,"CH","R","R")

ch_l <- f(cu_train_select,"CH","L","R")

v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff,data=ch_r, family=binomial)

vif(v_check) #remove spin_dir_diff

v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff+spin_dir_diff,data=ch_r, family=binomial)

vif(v_check2) #remove vmov_diff

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff,data=ch_l, family=binomial)

vif(v_check3)

#FB MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z
#CU/SL MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff
#CH MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff