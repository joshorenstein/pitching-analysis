sh <- s %>%  filter(events !='null')
#View(sh)

sh <- sh %>% mutate(HR=if_else(events=="home_run",1,0))

df <- sh %>%   #data for Curve/Slider/Cutter whiff model %>% 
  mutate(pitch_type=(if_else(pitch_type == "FS","CH",if_else(pitch_type=="KC","CU",pitch_type)))) %>% 
  filter(pitch_type %in% c("CU","SL","FC","CH"))

df %>% group_by(pitch_type,HR) %>% count()

#df$whiff <- as.factor(df$whiff)

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
                                        release_spin_direction,pfx_x,pfx_z,hmov_diff,velo_diff,HR,
                                        plate_x,plate_z,vmov_diff,spin_dir_diff) 

train_select %>% 
  group_by(pitch_type,p_throws,stand) %>% 
  summarise(n=n()) 

#GAM model for breaking balls
tr <-  train_select %>%
  filter(pitch_type!="CH") %>% 
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(HR ~ release_speed+release_pos_x+release_pos_z+release_spin_rate
               +release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z, data = .,family=binomial))

tr_ch <-  train_select %>%
  filter(pitch_type == "CH") %>% 
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(HR ~ release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z
               +hmov_diff+vmov_diff+velo_diff+spin_dir_diff+plate_x+plate_z, data = .,family=binomial,
               method="REML",bs="re"))

tr_data <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data_ch <- train_select %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
  full_join(tr_ch) %>% 
  group_by(pitch_type,stand,p_throws) %>% 
  do(augment(.$fit[[1]], newdata = .$data[[1]])) 

tr_data$prob <- exp(tr_data$.fitted)/(1+exp(tr_data$.fitted))
tr_data_ch$prob <- exp(tr_data_ch$.fitted)/(1+exp(tr_data_ch$.fitted))

# Comment out the test data
# te <-
#   test %>% group_by(pitch_type,stand,p_throws) %>% nest() %>% 
#   full_join(tr) %>% 
#   group_by(pitch_type,stand,p_throws) %>% 
#   do(augment(.$fit[[1]], newdata = .$data[[1]])) 

#te$prob <- exp(te$.fitted)/(1+exp(te$.fitted))

#te$whiff <- as.numeric(te$whiff) #Check whiff calculation
#View(tr_data)

# 
# final %>% group_by(pitch_type,p_throws,stand) %>%
#   write_csv("exports/swing_miss_br.csv")

t <- tr_data %>% bind_rows(tr_data_ch)

#Summarized breaking ball model
#br <- final %>% group_by(pitch_type,p_throws,stand) 
#$View(br)
#Bind all the data


br_hr_totals <- t  %>% group_by(player_name,pitch_type) %>% 
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

br_hr_totals[is.na(br_hr_totals)] <- 0
br_hr_totals


totals_hr_df <- fb_hr_totals %>% 
  left_join(br_hr_totals)

HRs <- totals_hr_df %>% 
  mutate(HR_product=n_FF*exp_HR_rate_FF+
           n_SI*exp_HR_rate_SI+
           n_SL*exp_HR_rate_SL+
           n_CH*exp_HR_rate_CH+
           n_FC*exp_HR_rate_FC+
           n_CU*exp_HR_rate_CU,
         n1=n_FF+n_SI+n_SL+n_CH+n_FC+n_CU,
         exp_HR_rate=HR_product/n1) %>% 
  arrange(desc(exp_HR_rate)) %>% 
  dplyr::select(player_name,exp_HR_rate,n1,
                exp_HR_rate_FF,exp_HR_rate_SI,exp_HR_rate_SL,
                exp_HR_rate_CH,exp_HR_rate_CU,exp_HR_rate_FC,
                n_FF,n_SI,n_SL,n_CH,n_CU,n_FC) %>% 
  rename(n_FF1=n_FF,n_SI1=n_SI,n_SL1=n_SL,n_CH1=n_CH,n_CU1=n_CU,n_FC1=n_FC)


df_final <- pitch_update %>% left_join(HRs)
df_final %>% write_csv("results/pitch_level_results.csv")

df_m <- 
  df_final %>% 
  rename(PA=n1) %>% 
  dplyr::select(player_name,exp_whiff_rate,f_p_str,exp_HR_rate,PA) %>% 
  rename(whiff_rate=exp_whiff_rate,f_strike=f_p_str,HR_PCT=exp_HR_rate)

final_br <- t %>% group_by(pitch_type,p_throws,stand,player_name) %>% 
  summarise(actual_hr_rate=sum(HR == "1")/n(),
            mph=mean(release_speed),rpm=mean(release_spin_rate),
            axis=mean(release_spin_direction),
            pfx_x=mean(pfx_x),
            pfx_z=mean(pfx_z),
            prob=round(mean(prob),2),n=n()) %>% arrange(p_throws,stand) %>%
  arrange(desc(prob)) %>% rename(exp_hr_rate = prob)

hr_data <- final %>% bind_rows(final_br)

