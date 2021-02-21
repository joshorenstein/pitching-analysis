library(modelr)
h <- hr_data %>% select(pitch_type,p_throws,stand,player_name,actual_hr_rate,exp_hr_rate)

#View(final)
final <- whiffs_data %>% 
  left_join(h) %>% 
  rename(whiff_rate=exp_whiff_rate) %>% 
  rename(HR_PCT=exp_hr_rate) %>% 
  filter(n>40)
final <- as.data.frame(final)
final[is.na(final)] <- 0 
str(final)

final$HR_PCT <- scale(final$HR_PCT, center = TRUE, scale = TRUE)
final$whiff_rate <- scale(final$whiff_rate, center=TRUE,scale=TRUE)

#class(final$HR_PCT)
# mean_target_2
# sd_target_2
final$whiff_rate <- mean_target + (final$whiff_rate - mean(final$whiff_rate)) * sd_target/sd(final$whiff_rate) 
#final$f_strike <- mean_target_1 + (final$f_strike - mean(final$f_strike)) * sd_target_1/sd(final$f_strike) 
final$HR_PCT <- mean_target_2 + (final$HR_PCT - mean(final$HR_PCT)) * sd_target_2/sd(final$HR_PCT)
final$whiff_rate <- as.numeric(final$whiff_rate)
final$HR_PCT <- as.numeric(final$HR_PCT)
str(final)
#View(final)
q <- final %>% 
  ungroup() %>% 
  select(pitch_type,player_name,p_throws,stand,whiff_rate,HR_PCT,n) %>% 
  arrange(player_name,pitch_type) 
head(q)
n <- q %>% 
  group_by(pitch_type,player_name,p_throws) %>% 
  summarise(n=sum(n)) %>% 
  arrange(player_name)
names(q)
whiff_u <- q %>% 
  group_by(pitch_type,player_name,stand) %>% 
  mutate(whiffs=whiff_rate*n) %>%
  select(-c(whiff_rate,HR_PCT,n)) %>% 
  spread(stand,whiffs) %>% 
  arrange(player_name)  %>% 
  replace_na(replace = list(L = 0)) %>% 
  replace_na(replace=list(R=0)) %>% 
  mutate(whiffs=L+R) %>% 
  select(-c(L,R)) %>% 
  inner_join(n) %>% 
  mutate(whiff_rate=whiffs/n) %>% 
  select(-c(whiffs))
names(whiff_u)
HR_u <- q %>% 
  group_by(pitch_type,player_name,stand) %>% 
  mutate(HR=HR_PCT*n) %>%
  select(-c(HR_PCT,whiff_rate,n)) %>% 
  spread(stand,HR) %>% 
  arrange(player_name)  %>% 
  replace_na(replace = list(L = 0)) %>% 
  replace_na(replace=list(R=0)) %>% 
  mutate(HR=L+R) %>% 
  select(-c(L,R)) %>% 
  inner_join(n) %>% 
  mutate(HR_PCT=HR/n) %>% 
  select(-c(HR,n))
head(whiff_u)
head(HR_u)
fin <- whiff_u %>% inner_join(HR_u)
#View(HR_u)
head(final)
final %>% distinct(player_name)
f <- final %>% 
  filter(n>=40) %>% 
  add_predictions(m1) %>% 
  rename(SO_perc=pred) %>% 
  add_predictions(m2) %>% 
  rename(pred_fip=pred) %>% 
  mutate(pred_fip=pred_fip*-1)
#View(fin)
f1 <- fin %>%
  filter(n>=40) %>% 
  add_predictions(m1) %>% 
  rename(SO_perc=pred) %>% 
  add_predictions(m2) %>% 
  rename(pred_fip=pred) %>% 
  mutate(pred_fip=pred_fip*-1)

totals <- 
f %>% 
  select(pitch_type,p_throws,stand,player_name,pred_fip) %>% 
  group_by(pitch_type) %>% 
  spread(pitch_type,pred_fip) %>% 
  mutate_if(is.numeric,percent_rank) %>% 
  mutate(across(where(is.numeric), round, 2)*100) %>% 
  inner_join(r) %>% 
  write_csv("results/best-pitches-by-batter-side.csv",na="")
names(totals)
totals_1 <- 
  f1 %>% 
  select(pitch_type,p_throws,player_name,pred_fip) %>% 
  group_by(pitch_type) %>% 
  spread(pitch_type,pred_fip) %>% 
  mutate_if(is.numeric,percent_rank) %>% 
  mutate(across(where(is.numeric), round, 2)*100) %>% 
  inner_join(r) %>% 
  write_csv("results/best-pitches-overall.csv",na="")


# 
# #join to actual whiff data and do a comparison of actual vs expected whiffs
# actual_whiff_rate <- whiffs_data %>%
#   filter(n>=10) %>% 
#   dplyr::select(player_name,p_throws,stand,pitch_type,actual_whiff_rate) %>% 
#   group_by(pitch_type) %>% 
#   spread(pitch_type,actual_whiff_rate) %>% 
#   dplyr::select(player_name,p_throws,stand,FF,SI,FC,SL,CU,CH) %>% 
#   mutate_if(is.numeric,percent_rank) %>% 
#   inner_join(exp_whiff_rate) %>% 
#   mutate(dFF=xFF-FF,dSI=xSI-SI,dFC=xFC-FC,dSL=xSL-SL,dCU=xCU-CU,dCH=xCH-CH) %>% 
#   dplyr::select(player_name,p_throws,stand,xFF,xSI,xFC,xSL,xCU,xCH,FF,SI,FC,SL,CU,CH,dFF,dSI,dFC,dSL,dCU,dCH) %>% 
#   mutate_at(4:21, funs(round(., 2))) 
# 
# #select just the pitch type percentile ranks
# me <- actual_whiff_rate %>% 
#   dplyr::select(player_name,p_throws,stand,xFF,xSI,xFC,xSL,xCU,xCH) #%>% 
# 
# #rank the pitch types
# m <- me %>% 
#   dplyr::select(-c(player_name,p_throws,stand)) %>% 
#   rowwise() %>% 
#   do(data.frame(t(rank(-unlist(.))))) %>% rename(FF=xFF,SI=xSI,SL=xSL,CU=xCU,CH=xCH,FC=xFC) %>% 
#   cbind(me) %>% 
#   # mutate(FF=ifelse(is.na(xFF),NA,FF),
#   #        SI=ifelse(is.na(xSI),NA,SI),
#   #        SL=ifelse(is.na(xSL),NA,SL),
#   #        FC=ifelse(is.na(xFC),NA,FC),
#   #        CU=ifelse(is.na(xCU),NA,CU),
#   #        CH=ifelse(is.na(xCH),NA,CH)) %>% 
#   dplyr::select(player_name,p_throws,stand,FF,SI,FC,SL,CU,CH)
# 
# #rank order the best 4 pitch types for each player
# m[paste0("sugg", 1:4)] <- t(apply(m[-3], 1, FUN = function(x) names(sort(x))))
# m
# #clean up the results with some brute force code that i need to functionalize
# final <- m %>% inner_join(me) %>% 
#   dplyr::select(player_name,p_throws,stand,sugg1,sugg2,sugg3,sugg4,xFF,xSI,xFC,xSL,xCU,xCH) %>% 
#   mutate(sugg4=if_else((sugg4=="FF" & is.na(xFF)==TRUE)|
#                          (sugg4=="SI" & is.na(xSI)==TRUE)|
#                          (sugg4=="FC" & is.na(xFC)==TRUE)|
#                          (sugg4=="CH" & is.na(xCH)==TRUE)|
#                          (sugg4=="SL" & is.na(xSL)==TRUE)|
#                          (sugg4=="CU" & is.na(xCU)==TRUE),'',sugg4)) %>% 
#   mutate(sugg3=if_else((sugg3=="FF" & is.na(xFF)==TRUE)|
#                          (sugg3=="SI" & is.na(xSI)==TRUE)|
#                          (sugg3=="FC" & is.na(xFC)==TRUE)|
#                          (sugg3=="CH" & is.na(xCH)==TRUE)|
#                          (sugg3=="SL" & is.na(xSL)==TRUE)|
#                          (sugg3=="CU" & is.na(xCU)==TRUE),'',sugg3)) %>% 
#   mutate(sugg2=if_else((sugg2=="FF" & is.na(xFF)==TRUE)|
#                          (sugg2=="SI" & is.na(xSI)==TRUE)|
#                          (sugg2=="FC" & is.na(xFC)==TRUE)|
#                          (sugg2=="CH" & is.na(xCH)==TRUE)|
#                          (sugg2=="SL" & is.na(xSL)==TRUE)|
#                          (sugg2=="CU" & is.na(xCU)==TRUE),'',sugg2))

head(totals)
#grab some player data


#Grab some data
exports <- function(data,players){
  data %>% filter(player_name %in% c(players))
  }

pirates_split <- exports(totals,c("Steven Brault","Chad Kuhl","Mitch Keller","Tyler Anderson",
        "JT Brubaker","Wil Crowe","Richard Rodriguez",
        "Chris Stratton","Michael Feliz","Kyle Crick","Chasen Shreve",
        "Sam Howard","David Bednar","Carson Fulmer","Luis Oviedo")) %>% 
  mutate(team="PIT")

pirates_ovr <- exports(totals_1,c("Steven Brault","Chad Kuhl","Mitch Keller","Tyler Anderson",
                                  "JT Brubaker","Wil Crowe","Richard Rodriguez",
                                  "Chris Stratton","Michael Feliz","Kyle Crick","Chasen Shreve",
                                  "Sam Howard","David Bednar","Carson Fulmer","Luis Oviedo")) %>% 
                                  mutate(team="PIT")
giants_split <- exports(totals,c("Kevin Gausman","Johnny Cueto","Anthony DeSclafani","Alex Wood","Aaron Sanchez",
                                   "Reyes Moronta","Tyler Rogers","Jake McGee","Matt Wisler","Jarlin Garcia","Trevor Gott",
                                   "Sam Selman","Nick Tropeano","Logan Webb")) %>% 
  mutate(team="SF")

giants_ovr <- exports(totals_1,c("Kevin Gausman","Johnny Cueto","Anthony DeSclafani","Alex Wood","Aaron Sanchez",
                                   "Reyes Moronta","Tyler Rogers","Jake McGee","Matt Wisler","Jarlin Garcia","Trevor Gott",
                                   "Sam Selman","Nick Tropeano","Logan Webb")) %>% 
  mutate(team="SF")

mariners_split <- exports(totals,c("Marco Gonzales","Yusei Kikuchi",
                                 "Justus Sheffield","Chris Flexen",
                                 "Justin Dunn","Nick Margevicius",
                                 "Rafael Montero","Kendall Graveman",
                                 "Anthony Misiewicz","Casey Sadler",
                                 "Keynan Middleton","Will Vest",
                                 "Erik Swanson")) %>% 
  mutate(team="SEA")

mariners_ovr <- exports(totals_1,c("Marco Gonzales","Yusei Kikuchi",
                                 "Justus Sheffield","Chris Flexen",
                                 "Justin Dunn","Nick Margevicius",
                                 "Rafael Montero","Kendall Graveman",
                                 "Anthony Misiewicz","Casey Sadler",
                                 "Keynan Middleton","Will Vest",
                                 "Erik Swanson")) %>% 
  mutate(team="SEA")

bluejays_split <- exports(totals,c("Hyun Jin Ryu","Robbie Ray","Nate Pearson","Tanner Roark",
                                 "Steven Matz","Kirby Yates","Jordan Romano","Rafael Dolis",
                                 "Julian Merryweather","Ryan Borucki","Ross Stripling",
                                 "Tyler Chatwood","Shun Yamaguchi")) %>% 
  mutate(team="TOR")

bluejays_ovr <- exports(totals_1,c("Hyun Jin Ryu","Robbie Ray","Nate Pearson","Tanner Roark",
                                 "Steven Matz","Kirby Yates","Jordan Romano","Rafael Dolis",
                                 "Julian Merryweather","Ryan Borucki","Ross Stripling",
                                 "Tyler Chatwood","Shun Yamaguchi")) %>% 
  mutate(team="TOR")




ovr <- rbind(bluejays_ovr,mariners_ovr,giants_ovr,pirates_ovr) 
head(ovr)
dim(r)

split <- rbind(bluejays_split,mariners_split,giants_split,pirates_split)
split_1 <- r %>% select(player_name,Predicted_FIP) %>% 
  inner_join(split,by=c("player_name"="player_name")) %>% 
  select(-Predicted_FIP.y) %>% 
  rename(Predicted_FIP=Predicted_FIP.x)
names(split_1)
#print to csv
#View(ovr)
ovr %>% write_csv('data/overall.csv',na="")
split_1 %>% write_csv('data/split.csv',na="")



# 
# redsox <- final %>% filter(player_name %in% c("Nathan Eovaldi","Eduardo Rodriguez","Garrett Richards","Martin Perez",
#                            "Nick Pivetta","Matt Barnes","Adam Ottavino","Ryan Brasier","Matt Andriese",
#                            "Phillips Valdez","Darwinzon Hernandez","Austin Brice","Garrett Whitlock"))
# 
# 
# mariners <- final %>% filter(player_name %in% c("Marco Gonzales","Yusei Kikuchi",
#                                                 "Justus Sheffield","Chris Flexen",
#                                                 "Justin Dunn","Nick Margevicius",
#                                                 "Rafael Montero","Kendall Graveman",
#                                                 "Anthony Misiewicz","Casey Sadler",
#                                                 "Keynan Middleton","Will Vest",
#                                                 "Erik Swanson"))
# 
# 
