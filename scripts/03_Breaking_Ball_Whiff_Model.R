
library(tidyverse)
library(tidymodels)


sp <- s %>% filter(des2 %in% c("ball","strike")) #data for whiff model
sp %>% distinct(pitch_type)
df <- sp %>%   #data for fb whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","FC")) %>% 
  mutate(sd_i=abs(release_pos_x*pfx_x),
         ht_i=abs(release_pos_z*pfx_z))
#dim(sp)
#View(sp)
df %>% group_by(pitch_type,whiff) %>% count()
#df %>% distinct(events) %>% write_csv('events.csv')
#df %>% distinct(events) %>% write_csv('events.csv')
names(df)
dim(df)


df$whiff <- as.factor(df$whiff)
names(df)


# Make sure that you get the same random numbers
smp_size <- floor(1 * nrow(df))
smp_size

## set the seed to make your partition reproductible
set.seed(61919)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <-df[train_ind, ]
test <- df[-train_ind, ]
nrow(train)/nrow(df)

names(train)
head(train)
train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,release_pos_x,
                                 release_pos_z,
                                 release_speed,release_spin_rate,release_spin_direction,
                                 hmov_diff,vmov_diff,velo_diff,spin_dir_diff,
                                 pfx_x,pfx_z,whiff,sd_i,ht_i) %>% 
  mutate(release_pos_x=abs(release_pos_x),
         release_pos_z=abs(release_pos_z))

library(modelr)

train_select %>% 
  group_by(pitch_type,p_throws,stand) %>% 
  summarise(n=n()) 


tr <-  train_select %>%
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(whiff ~ release_speed+release_pos_x+release_pos_z+release_spin_rate
               +release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff, data = .,family=binomial))
names(tr)
tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))
#View(tr_data)
dim(tr_data)

te <-
  test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

dim(te)
te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))
dim(te)
#te$whiff <- as.numeric(te$whiff) #Check whiff calculation
str(te)
final <- tr_data %>% group_by(pitch_type,p_throws,stand,player_name) %>% 
  summarise(actual_whiff_rate=sum(whiff == "1")/n(),mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>% rename(exp_whiff_rate=prob) %>% 
  filter(n>10) %>% 
  arrange(desc(exp_whiff_rate)) 
View(final)
# 
# final %>% group_by(pitch_type,p_throws,stand) %>%
#   write_csv("exports/swing_miss_br.csv")

br <- final %>% group_by(pitch_type,p_throws,stand) 

head(br)
View(br)
