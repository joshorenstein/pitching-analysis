

df_model <- df_m %>% filter(PA>50)
pitchers <- daily_pitcher_bref("2019-04-01", "2019-10-03") %>% 
  fip_plus() %>% 
  #dplyr::select(season, Name, IP, ERA, SO, uBB, HBP, HR, FIP, wOBA_against, wOBA_CON_against) %>%
  arrange(desc(IP)) 
head(pitchers)
fg <- read_csv("data/fg-2019.csv")

f <- fg %>% 
  inner_join(pitchers) %>% 
  rename(whiff_rate=`sw-str`,f_strike=`f-strike`) %>% 
  mutate(SO_PCT = SO/IP,BB_PCT = uBB/IP,HR_PCT = HR/IP) %>% 
  filter(AB>10)

m1 <- lm(SO_perc~whiff_rate,data=f)
m2 <- lm(BB_PCT~f_strike,data=f)
m3 <- lm(FIP~SO_perc+uBB_perc+HR_PCT,data=f)

df_model$HR_PCT <- scale(df_model$HR_PCT, center = TRUE, scale = TRUE)
df_model$f_strike <- scale(df_model$f_strike, center = TRUE, scale = TRUE)
df_model$whiff_rate <- scale(df_model$whiff_rate, center=TRUE,scale=TRUE)


mean_target <- mean(f$whiff_rate) #desired mean/sd of the data
sd_target   <- sd(f$whiff_rate)
mean_target_1 <- mean(f$f_strike) #desired mean/sd of the data
sd_target_1   <- sd(f$f_strike)
mean_target_2 <- mean(f$HR_PCT) #desired mean/sd of the data
sd_target_2 <- sd(f$HR_PCT)
mean_target_3 <- mean(f$FIP)
sd_target_3 <- sd(f$FIP)

df_model$whiff_rate <- mean_target + (df_model$whiff_rate - mean(df_model$whiff_rate)) * sd_target/sd(df_model$whiff_rate) #according to the given formula following the link you provided
df_model$f_strike <- mean_target_1 + (df_model$f_strike - mean(df_model$f_strike)) * sd_target_1/sd(df_model$f_strike) #according to the given formula following the link you provided
df_model$HR_PCT <- mean_target_2 + (df_model$HR_PCT - mean(df_model$HR_PCT)) * sd_target_2/sd(df_model$HR_PCT) #according to the given formula following the link you provided
df_model$whiff_rate <- as.numeric(df_model$whiff_rate)
df_model$f_strike <- as.numeric(df_model$f_strike)
df_model$HR_PCT <- as.numeric(df_model$HR_PCT)

final <- df_model %>%
  add_predictions(m1) %>% 
  rename(SO_perc=pred) %>% 
  add_predictions(m2) %>% 
  rename(uBB_perc=pred) %>% 
  add_predictions(m3) %>% 
  na.omit() 

results <- final %>% 
  mutate(pred_FIP=round(pred,2)) %>% 
  dplyr::select(player_name,pred_FIP) %>% 
  left_join(df_m) %>% 
  mutate(exp_whiff_pct=round(whiff_rate*100,1),f_strike_pct=round(f_strike*100,1),exp_homerun_pct=round(HR_PCT*100,1)) %>% 
  dplyr::select(player_name,pred_FIP,exp_whiff_pct,f_strike_pct,exp_homerun_pct,PA) %>% 
  arrange(pred_FIP)
head(results)
results %>% write_csv("results/results.csv")
p <- 
  results %>% filter(pFIP<3.5&PA>=200)
p
q <- ggplot(p, aes(y = reorder(player_name, -pFIP), x = pFIP)) + geom_bar(stat = "identity",fill="gray")
q + labs(x="predicted FIP based on stuff",y="pitcher")
ggsave("results/predicted-fip.pdf", width = 6, height = 4)
