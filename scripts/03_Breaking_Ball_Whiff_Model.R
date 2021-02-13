
df <- sp %>%   #data for Curve/Slider/Cutter whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","FC","CH"))

df %>% group_by(pitch_type,whiff) %>% count()

df$whiff <- as.factor(df$whiff)

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
#Grab data for breaking ball model
train_select <- train %>% dplyr::select(player_name,pitch_type,p_throws,stand,
                          release_speed,release_pos_x,release_pos_z,release_spin_rate,
                          release_spin_direction,pfx_x,pfx_z,hmov_diff,velo_diff,whiff,
                          plate_x,plate_z) 

train_select %>% 
  group_by(pitch_type,p_throws,stand) %>% 
  summarise(n=n()) 

#GAM model for breaking balls
tr <-  train_select %>%
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(whiff ~ release_speed+release_pos_x+release_pos_z+release_spin_rate
               +release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z, data = .,family=binomial))

tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))

# Comment out the test data
# te <-
#   test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
#   full_join(tr) %>% 
#   group_by(pitch_type,stand,p_throws) %>% 
#   do(augment(.$fit[[1]], newdata = .$data[[1]])) 

#te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))

#te$whiff <- as.numeric(te$whiff) #Check whiff calculation

final <- tr_data %>% group_by(pitch_type,p_throws,stand,player_name) %>% 
  summarise(actual_whiff_rate=sum(whiff == "1")/n(),mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>% rename(exp_whiff_rate=prob) %>% 
  filter(n>10) %>% 
  arrange(desc(exp_whiff_rate)) 

# 
# final %>% group_by(pitch_type,p_throws,stand) %>%
#   write_csv("exports/swing_miss_br.csv")

#Summarized breaking ball model
br <- final %>% group_by(pitch_type,p_throws,stand) 
#$View(br)

br_totals <- tr_data %>% group_by(player_name,pitch_type) %>% 
  summarise(actual_whiff_rate=sum(whiff == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% 
  arrange(desc(prob)) %>% rename(exp_whiff_rate = prob) %>% 
  dplyr::select(player_name,pitch_type,exp_whiff_rate,n) %>% 
  arrange(player_name) %>% 
  pivot_wider(id_cols = player_name, 
              names_from = pitch_type, 
              values_from = c("exp_whiff_rate", "n"))

br_totals
br_totals[is.na(br_totals)] <- 0

#Bind all the data
full_df <- bind_rows(fb,br)
totals_df <- fb_totals %>% 
  left_join(br_totals)


whiffs <- totals_df %>% 
  mutate(whiff_product=n_FF*exp_whiff_rate_FF+
                        n_SI*exp_whiff_rate_SI+
                        n_SL*exp_whiff_rate_SL+
                        n_CH*exp_whiff_rate_CH+
                        n_FC*exp_whiff_rate_FC+
                        n_CU*exp_whiff_rate_CU,
        n=n_FF+n_SI+n_SL+n_CH+n_FC+n_CU,
        exp_whiff_rate=whiff_product/n) %>% 
  arrange(desc(exp_whiff_rate)) %>% 
  dplyr::select(player_name,exp_whiff_rate,n,
                exp_whiff_rate_FF,exp_whiff_rate_SI,exp_whiff_rate_SL,
                exp_whiff_rate_CH,exp_whiff_rate_CU,exp_whiff_rate_FC,
                n_FF,n_SI,n_SL,n_CH,n_CU,n_FC)