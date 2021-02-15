library(tidyverse)
library(psych)
library(mgcv)
library(Hmisc)
sh <- s %>% filter(events !='null')


sh <- sh %>% mutate(HR=if_else(events=="home_run",1,0))
sh %>% group_by(HR) %>% summarise(n=n())
df <- sh %>%   #data for fb whiff model %>% 
  filter(pitch_type %in% c("FF","SI")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))

#train_ind <- sample(seq_len(nrow(df)), size = smp_size)

dim(df)

# Make sure that you get the same random numbers
smp_size <- floor(1 * nrow(df))
smp_size
s
## set the seed to make your partition reproductible
set.seed(61919)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df[train_ind, ]
test <- df[-train_ind, ]
nrow(train)/nrow(df)

#View(train)
train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                        release_pos_z,release_speed,release_spin_rate,release_spin_direction,
                                        hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                        pfx_x,pfx_z,HR,plate_x,plate_z)

library(modelr)

train_select %>% 
  group_by(pitch_type,p_throws,stand,HR) %>% 
  summarise(n=n())


#GAM models for fastballs and sinkers
tr <-  train_select %>%
  filter(pitch_type %in% c("FF","SI")) %>% 
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(HR ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z, data = .,family=binomial))


#View(tr)
tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))

#View(tr_data)
# Comment out the test code
# te <-
#   test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
#   full_join(tr) %>% 
#   group_by(pitch_type,stand,p_throws) %>% 
#   do(augment(.$fit[[1]], newdata = .$data[[1]])) 
# 
# te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))


# final %>% group_by(pitch_type,p_throws,stand) %>% 
# write_csv("exports/swing_miss_fb.csv")

#fastball summary data
#fb <- final %>% group_by(pitch_type,p_throws,stand)

fb_hr_totals <- tr_data %>% group_by(player_name,pitch_type) %>% 
  summarise(actual_HR_rate=sum(HR == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% 
  arrange(desc(prob)) %>% rename(exp_HR_rate = prob) %>% 
  dplyr::select(player_name,pitch_type,exp_HR_rate,n) %>% 
  arrange(player_name) %>% 
  pivot_wider(id_cols = player_name, 
              names_from = pitch_type, 
              values_from = c("exp_HR_rate", "n"))

fb_hr_totals[is.na(fb_hr_totals)] <- 0



final <- tr_data %>% group_by(pitch_type,p_throws,stand,player_name) %>%
  summarise(actual_hr_rate=sum(HR == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>%
  arrange(desc(prob)) %>% rename(exp_hr_rate = prob)

