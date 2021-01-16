library(tidyverse)
#read the data in
df <- read_csv("data/df.csv")
#filter for swings
whiffs <- df %>% filter(description %in% c("foul","hit_into_play",
                                 "hit_into_play_score",
                                 "swinging_strike_blocked",
                                 "swinging_strike",
                                 "hit_into_play_no_out",
                                 "foul_tip","foul_bunt","missed_bunt",
                                 "bunt_foul_tip")) %>% 
  mutate(whiff=as.factor(if_else(description %in% c("swinging_strike_blocked","swinging_strike","missed_bunt"),1,0)))

### Cluster pitchers based on release point ###
fb <- whiffs %>% filter(pitch_type =="FF")
mean_fb <-  fb %>% group_by(player_name,pitcher) %>% summarise(ht=mean(release_pos_z),sd=mean(abs(release_pos_x)))
mean_fb
# Convert the features of the data
test.data <- as.matrix(mean_fb[3:4])
dim(test.data)
# Check column means and standard deviations
colMeans(test.data)
summary(test.data)
apply(test.data,2,sd)

# Scale the test.data
data.scaled <- scale(test.data)
# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)
# Create a hierarchical clustering model: data.hclust
clusters <- hclust(d=data.dist,method="complete")
plot(clusters)
abline(h=4,lty=2)

# Cut tree so that it has 7 clusters
fb_clusters <- cutree(clusters,k=3)
fb_cluster_df <- data.frame(fb_clusters)
head(fb_cluster_df)
fb_cluster_df$fb_clusters <- as.character(fb_cluster_df$fb_clusters)
fb_cluster_df
# Compare clusters
summary <- mean_fb %>% ungroup() %>%  bind_cols(fb_cluster_df) %>% 
  dplyr::group_by(fb_clusters) %>% 
  dplyr::summarise(ht=mean(ht),sd=mean(sd),count=n())
summary

fb_update <- mean_fb %>% ungroup() %>%  bind_cols(fb_cluster_df) %>% select(player_name,pitcher,fb_clusters) %>% inner_join(fb)
fb_update$fb_clusters <- as.factor(fb_update$fb_clusters)

###################################################################
## Code for Applied Machine Learning by Max Kuhn @ RStudio::conf
## https://github.com/topepo/rstudio-conf-2018

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

rh_rb <- fb_update %>% filter(p_throws=="R"&stand=="R")
rh_lb <- fb_update %>% filter(p_throws=="R"&stand=="L")
# Make sure that you get the same random numbers
smp_size <- floor(0.75 * nrow(rh_rb))
smp_size

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(rh_rb)), size = smp_size)
train <- rh_rb[train_ind, ]
test <- rh_rb[-train_ind, ]
nrow(train)/nrow(rh_rb)

library(modelr)

str(train)
mod <- train %>%
  group_by(fb_clusters) %>%
  nest() %>%
  mutate(mdl = map(df, ~ lm(release_speed~release_spin_rate, data=.))) %>%
  mutate(fit = map(mdl, ~ .$fitted.values))
rmod
t <- train %>%
  group_by(fb_clusters) %>%
  nest() %>%
  mutate(mdl = map(data, ~ glm(whiff~release_speed+release_spin_rate+pfx_x+pfx_z, data=.,family = binomial(link = "logit")))) %>%
  mutate(fit = map(mdl, ~ .$fitted.values)) %>%
  mutate(data = map2(data, mdl, add_predictions))
tr <- t %>% select(-mdl, -fit) %>% unnest(cols = c(data)) 

names(tr)
tr$prob <- exp(tr$pred)/(1+exp(tr$pred))
