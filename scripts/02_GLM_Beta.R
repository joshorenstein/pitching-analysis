
library(tidyverse)

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

library(rsample)

# rh_rb <- fb_update %>% filter(p_throws=="R"&stand=="R")
# rh_lb <- fb_update %>% filter(p_throws=="R"&stand=="L")
# rh <- df_update %>% filter(p_throws=="R")
# dim(rh)
# Make sure that you get the same random numbers
smp_size <- floor(0.75 * nrow(df_update))
smp_size

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df_update)), size = smp_size)
train <- df_update[train_ind, ]
test <- rh[-train_ind, ]
nrow(train)/nrow(df_update)

library(modelr)

str(train)

rmod
t <- train %>%
  filter(arm_slot %in% c(1,2) & pitch_type %in% (c("FF","FC","SI","SL","CH","CU"))) %>% 
  group_by(arm_slot,pitch_type,p_throws,stand) %>%
  nest() %>%
  mutate(mdl = map(data, ~ glm(whiff~release_speed+release_spin_rate+pfx_x+pfx_z, data=.,family = binomial(link = "logit")))) %>%
  mutate(fit = map(mdl, ~ .$fitted.values)) %>%
  mutate(data = map2(data, mdl, add_predictions))
tr <- t %>% select(-mdl, -fit) %>% unnest(cols = c(data)) 

tr$prob <- exp(tr$pred)/(1+exp(tr$pred))

train_summary <- tr %>% group_by(player_name,stand,p_throws,pitch_type) %>% 
  summarise(release_speed=mean(release_speed),release_spin_rate=mean(release_spin_rate),prob=mean(prob),count=n()) %>% 
  filter(count >20) %>% arrange(desc(prob)) 

train_summary <- na.omit(train_summary)
train_summary

head(train_summary)  
summary(train_summary)

