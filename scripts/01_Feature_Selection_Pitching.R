#Load some libraries
library(tidyverse)
library(tidymodels)
library(MASS, exclude='select')
library(VIF)
library(car)
library(broom)

df <- read_csv('scraps/raw_data.csv') #if you need to read the data in 

df$whiff <- as.factor(df$whiff) #make the binary whiff variable a factor

## set the seed to make your partition reproducible
set.seed(61919)
#train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df
#test <- df[-train_ind, ] #using 2020 season as train. will add 2021 season as test set
nrow(train)/nrow(df)

#Make a dataset with all the potential variables for fastball model
train_select <- train %>% 
  select(player_name,pitch_type,p_throws,stand,release_pos_x,release_pos_z,release_speed,
         release_extension,release_spin_rate,release_spin_direction,plate_x,plate_z,pfx_x,pfx_z,whiff)
# PCA on fastballs
train_pca <- train_select %>%
              filter(pitch_type %in% c("FF","SI") & p_throws=="R" & stand=="R") %>% 
              select(-c(player_name,pitch_type,p_throws,stand,whiff))

#scale the data
pr.out <- prcomp(train_pca, center = TRUE, scale. = TRUE)

# Inspect model output
summary(pr.out) #PCA doesn't seem to be applicable as it's not really reducing the data

## Variance Inflation Tests to Check for MultiCollinearity

#make a function to grab data on pitch type/p_throws/stand level
f <- function(data,type,pitcher,batter){
  data %>% filter(type==pitch_type&p_throws==pitcher&stand==batter)}

#Grab some data
ff_r <- f(train_select,"FF","R","R") #fastball RHP/RHB
ff_l <- f(train_select,"FF","R","L") #fastball RHP/LHB
si_l <- f(train_select,"SI","R","L") #sinker RHP/LHB

#Variance Inflation Factor Test - RHP vs RHB Fastballs
v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
            release_spin_rate+release_extension+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z,
            data=ff_r, family=binomial)

vif(v_check) # lookgs good

#double check results - RHP vs LHB
v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z++plate_x+plate_z,data=ff_l, family=binomial)
vif(v_check2)
#Check these results on the sinker data
v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z,data=si_l, family=binomial)

vif(v_check3)

#Run same test for breaking balls/cutters/changeups using additional variables 
#that compare data to fastball velo and movement

df_b <- df %>%   #data for fb whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","CH","FC"))

#Grab potential variables for breaking ball models
cu_train_select <- train %>% select(player_name,pitch_type,p_throws,stand,release_pos_x,release_extension,
                                        release_pos_z,release_speed,release_spin_rate,release_spin_direction,
                                        hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                        pfx_x,pfx_z,whiff,plate_x,plate_z) %>% 
                                    mutate(release_pos_x=abs(release_pos_x),release_pos_z=abs(release_pos_z))

#Grab some curveball data
cu_r <- f(cu_train_select,"CU","R","R")
cu_l <- f(cu_train_select,"CU","L","R")
fc_l <- f(cu_train_select,"FC","R","L")

v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check) #remove spin_dir_diff 

v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check2) #remove vmov_diff

v_check3 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=cu_r, family=binomial)

vif(v_check3) #looks good 

#double check with LHP
v_check4 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=cu_l, family=binomial)
vif(v_check4) 

#Double Check Sliders
sl_r <- f(cu_train_select,"SL","R","R")

v_check5 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=sl_r, family=binomial)
vif(v_check5)

v_check6<- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+velo_diff+plate_x+plate_z,data=sl_r, family=binomial)
vif(v_check6)
#Check Changeups
ch_r <- f(cu_train_select,"CH","R","R")

ch_l <- f(cu_train_select,"CH","R","L")

v_check <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=ch_r, family=binomial)

vif(v_check) #looks_good

#check vs LHB
v_check2 <- glm(whiff ~ release_speed+release_pos_x+release_pos_z+release_extension+
                  release_spin_rate+release_spin_direction+pfx_x+pfx_z
                +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z,data=ch_l, family=binomial)
vif(v_check2)


#Ideal models
#FB MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_extension+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z
#CU/SL MODEL: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_extension+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z
#CH MODEL: release_speed+release_pos_x+release_pos_z+release_extension+
#release_spin_rate+release_spin_direction+pfx_x+pfx_z
#+hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z
