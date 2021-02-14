library(tidyverse)
library(psych)
library(mgcv)

f_p <- s %>% 
  filter(balls==0 & strikes==0) %>% 
  mutate(f_p_str=if_else(description %in% c("ball","blocked_ball","pitchout"),"ball","strike")) %>% 
  group_by(player_name,f_p_str) %>% 
  summarise(n=n())  %>% 
  spread(f_p_str,n) %>% 
  replace_na(replace = list(ball = 0)) %>% 
  replace_na(replace = list(strike =0)) %>% 
  mutate(n=ball+strike,f_p_str=(strike/n)) %>% 
  arrange(desc(f_p_str)) %>% 
  dplyr::select(player_name,f_p_str)

df <- s %>%   #data for fb whiff model %>% 
  filter(pitch_type %in% c("FF","SI")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))

#train_ind <- sample(seq_len(nrow(df)), size = smp_size)


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


train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                        release_pos_z,release_speed,release_spin_rate,release_spin_direction,
                                        hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                        pfx_x,pfx_z,swing,plate_x,plate_z)

library(modelr)

train_select %>% 
  group_by(pitch_type,p_throws,stand) %>% 
  summarise(n=n()) 

#str(train_select$swing)
train_select$swing <- as.factor(train_select$swing)
#GAM models for fastballs and sinkers
tr <-  train_select %>%
  filter(pitch_type %in% c("FF","SI")) %>% 
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = glm(swing ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z, data = .,family=binomial,
                control = list(maxit = 50)))


tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))

# Comment out the test code
# te <-
#   test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
#   full_join(tr) %>% 
#   group_by(pitch_type,stand,p_throws) %>% 
#   do(augment(.$fit[[1]], newdata = .$data[[1]])) 
# 
# te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))


final <- tr_data %>% group_by(pitch_type,p_throws,stand,player_name) %>% 
  summarise(actual_swing_rate=sum(swing == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>% 
  filter(n>10) %>% 
  arrange(desc(prob)) %>% rename(exp_swing_rate = prob) 

# final %>% group_by(pitch_type,p_throws,stand) %>% 
# write_csv("exports/swing_miss_fb.csv")

#fastball summary data
fb <- final %>% group_by(pitch_type,p_throws,stand) 

fb_totals <- tr_data %>% group_by(player_name,pitch_type) %>% 
  summarise(actual_swing_rate=sum(swing == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% 
  arrange(desc(prob)) %>% rename(exp_swing_rate = prob) %>% 
  dplyr::select(player_name,pitch_type,exp_swing_rate,n) %>% 
  arrange(player_name) %>% 
  pivot_wider(id_cols = player_name, 
              names_from = pitch_type, 
              values_from = c("exp_swing_rate", "n")) %>% 
  dplyr::select(-c(n_FF,n_SI))

fb_totals[is.na(fb_totals)] <- 0

head(fb_totals)
head(f_p)
head(whiffs)
#View(fb_totals)
pitch_update <- whiffs %>% left_join(f_p) %>% left_join(fb_totals,by=c("player_name"="player_name"))
View(pitch_update)
