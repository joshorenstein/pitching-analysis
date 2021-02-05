head(full_df)
names(full_df)
#View(full_df)

#just keep rows that have 10 or more pitches
exp_whiff_rate <- full_df %>%
  dplyr::filter(n>=10) %>% 
  dplyr::select(player_name,p_throws,stand,pitch_type,exp_whiff_rate) %>% 
  group_by(pitch_type) %>% 
  spread(pitch_type,exp_whiff_rate) %>% 
  dplyr::select(player_name,p_throws,stand,FF,SI,FC,SL,CU,CH) %>%
  mutate_if(is.numeric,percent_rank) %>% 
  rename(xFF=FF,xSI=SI,xFC=FC,xSL=SL,xCU=CU,xCH=CH)


#join to actual whiff data and do a comparison of actual vs expected whiffs
actual_whiff_rate <- full_df %>%
  filter(n>=10) %>% 
  dplyr::select(player_name,p_throws,stand,pitch_type,actual_whiff_rate) %>% 
  group_by(pitch_type) %>% 
  spread(pitch_type,actual_whiff_rate) %>% 
  dplyr::select(player_name,p_throws,stand,FF,SI,FC,SL,CU,CH) %>% 
  mutate_if(is.numeric,percent_rank) %>% 
  inner_join(exp_whiff_rate) %>% 
  mutate(dFF=xFF-FF,dSI=xSI-SI,dFC=xFC-FC,dSL=xSL-SL,dCU=xCU-CU,dCH=xCH-CH) %>% 
  dplyr::select(player_name,p_throws,stand,xFF,xSI,xFC,xSL,xCU,xCH,FF,SI,FC,SL,CU,CH,dFF,dSI,dFC,dSL,dCU,dCH) %>% 
  mutate_at(4:21, funs(round(., 2))) 

#select just the pitch type percentile ranks
me <- actual_whiff_rate %>% 
  dplyr::select(player_name,p_throws,stand,xFF,xSI,xFC,xSL,xCU,xCH) #%>% 

#rank the pitch types
m <- me %>% 
  dplyr::select(-c(player_name,p_throws,stand)) %>% 
  rowwise() %>% 
  do(data.frame(t(rank(-unlist(.))))) %>% rename(FF=xFF,SI=xSI,SL=xSL,CU=xCU,CH=xCH,FC=xFC) %>% 
  cbind(me) %>% 
  # mutate(FF=ifelse(is.na(xFF),NA,FF),
  #        SI=ifelse(is.na(xSI),NA,SI),
  #        SL=ifelse(is.na(xSL),NA,SL),
  #        FC=ifelse(is.na(xFC),NA,FC),
  #        CU=ifelse(is.na(xCU),NA,CU),
  #        CH=ifelse(is.na(xCH),NA,CH)) %>% 
  dplyr::select(player_name,p_throws,stand,FF,SI,FC,SL,CU,CH)

#rank order the best 4 pitch types for each player
m[paste0("sugg", 1:4)] <- t(apply(m[-3], 1, FUN = function(x) names(sort(x))))
m
#clean up the results with some brute force code that i need to functionalize
m %>% inner_join(me) %>% 
  dplyr::select(player_name,p_throws,stand,sugg1,sugg2,sugg3,sugg4,xFF,xSI,xFC,xSL,xCU,xCH) %>% 
  mutate(sugg4=if_else((sugg4=="FF" & is.na(xFF)==TRUE)|
                         (sugg4=="SI" & is.na(xSI)==TRUE)|
                         (sugg4=="FC" & is.na(xFC)==TRUE)|
                         (sugg4=="CH" & is.na(xCH)==TRUE)|
                         (sugg4=="SL" & is.na(xSL)==TRUE)|
                         (sugg4=="CU" & is.na(xCU)==TRUE),'',sugg4)) %>% 
  mutate(sugg3=if_else((sugg3=="FF" & is.na(xFF)==TRUE)|
                         (sugg3=="SI" & is.na(xSI)==TRUE)|
                         (sugg3=="FC" & is.na(xFC)==TRUE)|
                         (sugg3=="CH" & is.na(xCH)==TRUE)|
                         (sugg3=="SL" & is.na(xSL)==TRUE)|
                         (sugg3=="CU" & is.na(xCU)==TRUE),'',sugg3)) %>% 
  mutate(sugg2=if_else((sugg2=="FF" & is.na(xFF)==TRUE)|
                         (sugg2=="SI" & is.na(xSI)==TRUE)|
                         (sugg2=="FC" & is.na(xFC)==TRUE)|
                         (sugg2=="CH" & is.na(xCH)==TRUE)|
                         (sugg2=="SL" & is.na(xSL)==TRUE)|
                         (sugg2=="CU" & is.na(xCU)==TRUE),'',sugg2))

#grab the mets 
mets <- m %>% filter(player_name %in% c("Jacob deGrom","Carlos Carrasco","Marcus Stroman","David Peterson",
                             "Joey Lucchesi","Edwin Diaz","Jeurys Familia","Trevor May","Miguel Castro",
                             "Dellin Betances","Robert Gsellman","Aaron Loup","Drew Smith","Brad Brach","Seth Lugo",
                             "Jacob Barnes","Stephen Tarpley","Corey Oswalt","Trevor Bauer")) 
#print to csv
mets %>% write_csv('mets.csv')    
