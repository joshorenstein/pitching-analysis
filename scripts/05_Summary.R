head(full_df)
names(full_df)
View(full_df)

full_df %>% group_by(player_name,pitch_type,p_throws,stand) %>% filter(n>50) %>% 
  summarise(whiff_rate=mean(actual_whiff_rate),exp_whiff_rate=mean(exp_whiff_rate),
            mph=mean(mph),rpm=mean(rpm),n=n()) %>% 
  mutate(diff=exp_whiff_rate-whiff_rate) %>%
  arrange(desc(diff)) %>% View()
full_df %>% group_by(pitch_type,p_throws,stand) %>% filter(n>50) %>% 
  summarise(whiff_rate=mean(actual_whiff_rate),exp_whiff_rate=mean(exp_whiff_rate),
            mph=mean(mph),rpm=mean(rpm),n=n()) %>% 
  mutate(diff=exp_whiff_rate-whiff_rate) %>%
  arrange(desc(diff)) %>% View()
