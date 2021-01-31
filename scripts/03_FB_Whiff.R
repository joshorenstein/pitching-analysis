
library(tidyverse)
library(tidymodels)

#s <- read_csv('raw_data.csv')
sp <- s %>% filter(des2 %in% c("ball","strike")) #data for whiff model

df <- sp %>%   #data for fb whiff model %>% 
  filter(pitch_type %in% c("FF","FC","SI")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))
#dim(sp)
#View(sp)
df %>% group_by(whiff) %>% count()
#df %>% distinct(events) %>% write_csv('events.csv')
#df %>% distinct(events) %>% write_csv('events.csv')
names(df)
dim(df)


df$whiff <- as.factor(df$whiff)
names(df)


# Make sure that you get the same random numbers
smp_size <- floor(0.5 * nrow(df))
smp_size

## set the seed to make your partition reproductible
set.seed(61919)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df[train_ind, ]
test <- df[-train_ind, ]
nrow(train)/nrow(df)

names(train)
train_select <- train %>% select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                 release_pos_z,
                                 release_speed,release_spin_rate,release_spin_direction,
                                 pfx_x,pfx_z,whiff,sd_i,ht_i) %>% 
  mutate(release_pos_x=abs(release_pos_x),
         release_pos_z=abs(release_pos_z))

summary(train_select)
library(modelr)

train_select %>% 
  group_by(pitch_type,p_throws,stand) %>% 
  summarise(n=n()) 
names(train_select)
tr <-  train_select %>%
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = glm(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z+sd_i+ht_i, data = .,family=binomial))

tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))

dim(tr_data)
te <-
  test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 
names(te)
te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))
dim(te)
final <- te %>% group_by(pitch_type,p_throws,stand,player_name) %>% 
  summarise(mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>% 
  filter(n>10) %>% 
  arrange(desc(prob)) %>% rename(exp_whiff_rate = prob) 

final %>% group_by(pitch_type,p_throws,stand) %>% 
  slice_max(order_by = exp_whiff_rate, n = 5) %>% 
  write_csv("exports/top_5_swing_miss_fb.csv")
