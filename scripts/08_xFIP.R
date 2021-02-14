library(baseballr)
View(df_model)
df_model <- df_m %>% filter(PA>50)
#df_model <- df_model %>% rename(swing_pct=exp_swing_rate)
pitchers <- daily_pitcher_bref("2019-04-01", "2019-10-03") %>% 
  fip_plus() %>% 
  #dplyr::select(season, Name, IP, ERA, SO, uBB, HBP, HR, FIP, wOBA_against, wOBA_CON_against) %>%
  arrange(desc(IP)) 
head(pitchers)
fg <- read_csv("data/fg-2019.csv")
names(fg)
head(fg)
names(pitchers)
f <- fg %>% 
  inner_join(pitchers,by=c("Name"="Name")) %>% 
  rename(whiff_rate=sw_str) %>% 
  mutate(SO_PCT = SO/IP,BB_PCT = uBB/IP,HR_PCT = HR/IP) %>% 
  filter(AB>10)
head(f)
m1 <- lm(SO_perc~whiff_rate,data=f)
#m2 <- lm(BB_PCT~f_strike+swing_pct,data=f)
m2 <- lm(FIP~SO_perc+HR_PCT,data=f)
m3 <- lm(FIP~SO_perc+uBB_perc+HR_PCT,data=f)
names(f)
summary(m3)
names(df_model)
df_model$HR_PCT <- scale(df_model$HR_PCT, center = TRUE, scale = TRUE)
df_model$f_strike <- scale(df_model$f_strike, center = TRUE, scale = TRUE)
df_model$f_strike <- scale(df_model$swing_pct, center = TRUE, scale = TRUE)
df_model$whiff_rate <- scale(df_model$whiff_rate, center=TRUE,scale=TRUE)


mean_target <- mean(f$whiff_rate) #desired mean/sd of the data
sd_target   <- sd(f$whiff_rate)
mean_target_1 <- mean(f$f_strike) #desired mean/sd of the data
sd_target_1   <- sd(f$f_strike)
mean_target_2 <- mean(f$HR_PCT) #desired mean/sd of the data
sd_target_2 <- sd(f$HR_PCT)
mean_target_3 <- mean(f$FIP)
sd_target_3 <- sd(f$FIP)
mean_target_4 <- mean(f$swing_pct)
sd_target_4 <- sd(f$swing_pct)

df_model$whiff_rate <- mean_target + (df_model$whiff_rate - mean(df_model$whiff_rate)) * sd_target/sd(df_model$whiff_rate) #according to the given formula following the link you provided
df_model$f_strike <- mean_target_1 + (df_model$f_strike - mean(df_model$f_strike)) * sd_target_1/sd(df_model$f_strike) #according to the given formula following the link you provided
df_model$HR_PCT <- mean_target_2 + (df_model$HR_PCT - mean(df_model$HR_PCT)) * sd_target_2/sd(df_model$HR_PCT) #according to the given formula following the link you provided
#df_model$swing_pct <- mean_target_4 + (df_model$swing_pct - mean(df_model$swing_pct)) * sd_target_4/sd(df_model$swing_pct) #according to the given formula following the link you provided
df_model$whiff_rate <- as.numeric(df_model$whiff_rate)
df_model$f_strike <- as.numeric(df_model$f_strike)
df_model$HR_PCT <- as.numeric(df_model$HR_PCT)
#df_model$swing_pct <- as.numeric(df_model$swing_pct)
summary(m3)
head(df_model)
summary(df_model)
summary(f)


pitchers_20 <- daily_pitcher_bref("2020-04-01", "2020-10-03") %>% 
  fip_plus() %>% 
  arrange(desc(IP)) 
p <- pitchers_20 %>% dplyr::select(Name,uBB_perc)
names(df_model)
final <- df_model %>%
  inner_join(p,by=c("player_name"="Name")) %>% 
  add_predictions(m1) %>% 
  rename(SO_perc=pred) %>% 
  add_predictions(m2) %>% 
  rename(FIP_based_on_stuff=pred) %>% 
  add_predictions(m3) %>% 
  rename(Predicted_FIP=pred) %>% 
  arrange(FIP_based_on_stuff) %>% 
  mutate(diff=Predicted_FIP-FIP_based_on_stuff)
names(final)
results <- final %>% 
  mutate(FIP_based_on_stuff=round(FIP_based_on_stuff,2)) %>% 
  mutate(Predicted_FIP=round(Predicted_FIP,2)) %>% 
  dplyr::select(player_name,FIP_based_on_stuff,Predicted_FIP,uBB_perc) %>% 
  left_join(df_m) %>% 
  mutate(exp_whiff_pct=round(whiff_rate*100,1),f_strike_pct=round(f_strike*100,1),
         exp_homerun_pct=round(HR_PCT*100,1),uBB_perc=round(uBB_perc*100,1)) %>% 
  dplyr::select(player_name,FIP_based_on_stuff,Predicted_FIP,exp_whiff_pct,exp_homerun_pct,uBB_perc,PA) %>% 
  arrange(FIP_based_on_stuff)
#View(results)
results %>% write_csv("results/results.csv")
p <- 
  results %>% filter(PA>=150& FIP_based_on_stuff<=3.5)
#View(p)
q <- ggplot(p, aes(y = reorder(player_name, -FIP_based_on_stuff), x = FIP_based_on_stuff)) + 
  geom_bar(stat = "identity",fill="gray")
q + labs(x="predicted FIP based on stuff",y="pitcher")
ggsave("results/predicted-fip.pdf", width = 6, height = 4)
