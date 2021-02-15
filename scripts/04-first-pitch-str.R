library(tidyverse)
library(psych)
library(mgcv)

### First Pitch Strike Percentage ###
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

pitch_update <- whiffs %>% left_join(f_p) 
# #View(pitch_update)
