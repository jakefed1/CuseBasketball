#Syracuse competition

#load in data
library(tidyverse)
pbp_data <- read.csv("syracuse_gsw_basic_pbp.csv")
result_data <- read.csv("syracuse_gsw_basic_results.csv")
positional <- read.csv("positionaldata.csv")

def_pos <- merge(result_data, positional, by.x = "result_def1", by.y = "Player")


pbp_data <- pbp_data %>%
  filter(off_team == "GSW")

result_data <- result_data %>%
  filter(off_team == "GSW")%>%
  filter(result_type != "Broadcast Error")

shot_merged <- shot_merged %>%
  filter(off_team == "GSW")%>%
  filter(result_type != "Broadcast Error")




merged2 <- pbp_data %>%
  right_join(result_data, by= c("id", "result_id", "season", "season_type", "nba_game_id", "home", "away", "game_date", "quarter",
                                              "poss_id", "off_team", "def_team", "poss_time", "oncourt_id"))

merged %>%
  filter(player_type == "Ball-Handler")


merged %>%
  filter(result_tov_reason == "Def Forced")%>%
  group_by(play_type)%>%
  summarize(Count = n())%>%
  mutate(percent = Count/6968)%>%
  arrange(desc(percent))



length(unique(pbp_data$poss_id))
length(unique(pbp_data$nba_game_id))

#aprox 6968 possessions

def_pos$ShotResult<-ifelse(def_pos$points>0,1,0)



result_data3 <- def_pos%>%
  group_by(result_off, result_type, Pos, ShotResult)%>%
  summarize(Count2=n()) %>%
  ungroup() %>%
  group_by(result_off) %>%
  mutate(pct = Count2/sum(Count2),
         sum = sum(Count2)) %>%
  ungroup()


dta4 <- def_pos %>% #EPA by offensive personnel type, passing 
  filter(result_off == "Stephen Curry" & result_type == "3+D Pull-Up") %>%
  group_by(Pos, ShotResult) %>%
  summarize(count = n()) %>%
  mutate(pct = count/sum(count))%>%
  arrange(desc(pct)) %>%
  ungroup() %>%
  filter(count >=10) %>%
  filter(ShotResult == 1)


dta4 <- def_pos %>% #EPA by offensive personnel type, passing 
  filter(result_off == "Draymond Green" & Pos == "PF") %>%
  group_by(result_type) %>%
  summarize(count = n()) %>%
  mutate(pct = count/sum(count))%>%
  arrange(desc(pct)) %>%
  ungroup() 

shot_merged<- results_merged %>%
  filter(result_contest == "Average" | result_contest == "Contested" result_contest == "Open" | result_contest == "Poor" |
           result_contest == "Alter" | result_contest == "Block" | result_contest == "Goaltend" | result_contest == "Plus")
          

result4 %>%
  group_by(result_type)%>%
  summarize(count = n())%>%
  mutate(shotdist = count/sum(count))%>%
  arrange(desc(shotdist)) %>%
  ungroup()



result5 %>%
  ggplot(aes(x = result_type, y = shotdist)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



results_merged <- result_data %>%
  left_join(pbp_data, by = c("nba_game_id", "quarter", 
                         "result_id", "home", "away", "game_date",
                         "poss_time", "season", "season_type", "poss_id", "off_team", "def_team", "oncourt_id"))


results_merged <- results_merged %>%
  filter(off_team == "GSW")%>%
  filter(result_type != "Broadcast Error")
#IMPORTANT

result4 <- result4 %>%
  group_by(nba_game_id, quarter, poss_id) %>%
  mutate(row_counter = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

#possession type
result4 %>%
  group_by(poss_type)%>%
  summarize(count3 = n(),
            possession = sum(row_counter))%>%
  mutate(possrate = possession/sum(possession))%>%
  arrange(desc(possrate))




#
#important 
#possession rate by play
results_merged <- results_merged %>%
  group_by(nba_game_id, quarter, poss_id) %>%
  mutate(row_counter = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

results_mergedtable <- results_merged %>%
  group_by(poss_type, play_type)%>%
  summarize(count3 = n(),
            possession = sum(row_counter))%>%
  mutate(playrate = count3/sum(possession))%>%
  arrange(desc(playrate))
  


#shot location
shot_merged$shotresult<-ifelse(shot_merged$points>0,1,0)

shottable <- shot_merged %>%
  group_by(shotresult, result_zone_condensed)%>%
  summarize(count3 = n())%>%
  mutate(shottyperate = count3/nrow(shot_merged))%>%
  filter(shotresult == 1)%>%
  arrange(desc(shottyperate)) 


# WITH SHOT RESULT
#shows fg percent from each place
# shoots 67% from the RIM
shot_merged2<- shot_merged%>%
  group_by(result_zone_condensed, shotresult, quarter)%>%
  summarize(count = n())%>%
  mutate(fgpercent= count/sum(count))%>%
  ungroup()%>%
  filter(count > 10)%>%
  arrange(desc(fgpercent))%>%
  arrange(desc(shotresult))

shot_merged2

#graph shows field goal percentage in each zone
shot_merged2%>%
  filter(shotresult == 1) %>%
  arrange(desc(fgpercent))%>%
  filter(count > 20)%>%
  ggplot(aes(x = result_zone_condensed, y = fgpercent)) + 
  geom_bar(stat = "identity")+
  facet_grid(. ~quarter)  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#who gets offensive rebounds 
#what type of plays
result_merged2 %>%
  group_by(orb_win_name)%>%
  summarize(count = n())%>%
  filter(count < 300)%>%
  arrange(desc(count))


shot_merged <- shot_merged %>%
  mutate(
    result_zone_condensed = case_when(
      result_zone %in% c("Mid-Left Above Break 3", 
                         "Right Above Break 3", 
                         "Left Above Break 3", 
                         "Mid-Right Above Break 3", 
                         "Right Corner 3", 
                         "Left Corner 3") ~ "Three-Point Attempt",
      result_zone %in% c("Mid-Right Midrange", 
                         "Left Short Mid", 
                         "Right Short Mid", 
                         "Right Midrange", 
                         "Left Midrange",
                         "Mid-Left Midrange") ~ "Midrange Attempt",
      result_zone %in% c("Mid-Left Short Paint",
                         "Mid-Right Short Paint",
                         "Mid-Left Deep Paint", 
                         "Mid-Right Deep Paint", "Rim") ~ "Paint",
      result_zone %in% c("Short Backcourt", "Deep Backcourt") ~ "Backcourt"
    )
  )




assist_pass <- shot_merged %>%
  filter(assisted == 1)%>%
  group_by(result_off, passer_name, result_zone_condensed)%>%
  summarize(count = n())%>%
  arrange(desc(count))

shot_merged%>%
  group_by(assisted, shotresult)%>%
  summarize(count = n())%>%
  filter(count >50)%>%
  mutate(assistrate = /sum(count))





to_rate <- shot_merged %>%
  group_by(result_off, passer_name, assisted)%>%
  summarize(count)


#screen probabiltiy
result_merged2 %>%
  filter(play_type == "Ball Screen")%>%
  summarize(count = n())%>%
  mutate(screenprob = count/nrow(result_merged2))


#part of Jake's table 6

#Cool graph for ball screens 
shot_merged %>%
  filter(play_type == "Ball Screen")%>%
  group_by(result_off, result_zone_condensed)%>%
  summarize(count = n())%>%
  filter(count > 10)%>%
  mutate(screenpersonrate = count/sum(count))%>%
  arrange(desc(count))%>%
  ungroup() %>%
  filter(result_off != "Jonathan Kuminga")%>%
  ggplot(aes(x = result_zone_condensed, y = screenpersonrate, fill = count))+
  geom_bar(stat = "identity")+
  facet_grid(. ~result_off)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "Shot Result After a Player Receives a Screen")

  #partially condensed
shot_merged <- shot_merged %>%
  mutate(
    result_zone_semi = case_when(
      result_zone %in% c("Left Above Break 3",
                         "Left Corner 3") ~ "Left Three-Point Attempt",
      result_zone %in% c("Right Above Break 3", 
                         "Right Corner 3") ~ "Right Three-Point Attempt",
      result_zone %in% c("Mid-Left Above Break 3", 
                         "Mid-Right Above Break 3") ~ "Center Three-Point Attempt",
      result_zone %in% c("Left Short Mid",
                         "Left Midrange") ~ "Left Midrange Attempt",
      result_zone %in% c( "Right Short Mid", 
                         "Right Midrange") ~ "Right Midrange Attempt",
      result_zone %in% c("Mid-Right Midrange", 
                         "Mid-Left Midrange") ~ "Center Midrange Attempt",
      result_zone %in% c("Mid-Left Short Paint",
                         "Mid-Right Short Paint",
                         "Mid-Left Deep Paint", 
                         "Mid-Right Deep Paint", "Rim") ~ "Paint",
      result_zone %in% c("Short Backcourt", "Deep Backcourt") ~ "Backcourt"
    )
  )


shot_merged <- shot_merged %>%
  mutate(contest_level = case_when(
      result_contest %in% c("Alter",
                         "Block", "Contested") ~ "Great",
      result_contest %in% c("Average", 
                         "Plus") ~ "Good",
      result_contest %in% c("Open", 
                         "Poor") ~ "Bad"))


#location of shot of ball screen

shot_merged %>%
  group_by(result_zone_semi, contest_level,season_type)%>%
  summarize(count = n())%>%
  filter(result_zone_semi == "Right Midrange Attempt")%>%
  mutate(shotproportion = count/sum(count))%>%
  filter(count > 10) %>% 
  ungroup()%>%
  ggplot(aes(x = result_zone_semi, y = count, fill = (contest_level)))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  facet_wrap(~season_type)+
  labs(fill = "Contest level", x = "Shot Location", y = "Shot Distribution", title = "Contest Level Based Off Shot Location")
  


shot_merged %>%
  filter(result_zone_condensed == "Midrange Attempt" & season_type == "Playoffs") %>% 
  group_by(result_off) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(desc(pct))
  



shot_merged %>%
  filter(season_type == "Playoffs" & poss_type == "Transition") %>% 
  group_by(result_zone_semi) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(desc(pct))
#GREAT GRAPH
  #make or miss screen 
  shot_merged %>%
  filter(play_type == "Ball Screen" & result_off == "Jordan Poole")%>%
  group_by(shotresult, result_off, result_zone_condensed)%>%
  summarize(count = n())%>%
  filter(count > 5)%>%
  mutate(screenpersonrate = count/sum(count))%>%
  arrange(desc(count))%>%
  ungroup() %>%
  ggplot(aes(x = result_zone_condensed, y = count, fill = factor(shotresult)))+
  geom_bar(stat = "identity")+
  facet_grid(. ~result_off)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values=c("red",
                            "green"))+
  labs(title = "Shot Location If Player Shoots Right After a Screen", x = "Location of Shot", y = "Count", fill = "Shot Made")


table_1_ppp <- 
  results_merged %>%
  group_by(nba_game_id,quarter, poss_id, poss_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))


table_1_ppp <- 
  results_merged %>%
  group_by(nba_game_id,quarter, poss_id, poss_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))



table_1_ppp <-
  table_1_ppp %>%
  group_by(poss_type) %>%
  summarize(possession_count = n(),
            points_total = sum(points_total)) %>%
  ungroup() %>%
  mutate(ppp = points_total/possession_count)


table_2_ppp <- 
  results_merged %>%
  group_by(nba_game_id,quarter, poss_id, play_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))



table_2_ppp <-
  table_2_ppp %>%
  group_by(play_type) %>%
  summarize(possession_count = n(),
            points_total = sum(points_total)) %>%
  ungroup() %>%
  filter(possession_count > 500 & play_type != "NULL")%>%
  mutate(ppp = points_total/possession_count)%>%
  ggplot(aes(x = play_type, y = ppp, fill = possession_count))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "Impact of Play Type on Points Per Possession")



setdiff(unique(results_merged$result_type), c("Take Foul", "TOV", "Side Out", "Timeout", "Jump Ball", "Def. 3 Seconds", "Run Clock", "Broadcast Error"))


#3 PLAYERS
#STEPH
shot_merged %>%
  filter(result_off == "Jordan Poole")%>%
  group_by(result_zone_semi)%>%
  summarize(count= n())%>%
  filter(count > 10) %>% 
  arrange(desc(count))%>%
  ggplot(aes(x = result_zone_semi, y = count))+
  geom_bar(stat = "identity", fill = "blue", color = "yellow")+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
 labs(title = "Jordan Poole's Shot Distribution", x = "Shot Location", y= "Count")


shot_merged %>%
  filter(result_off == "Stephen Curry")%>%
  group_by(result_zone_semi)%>%
  filter(season_type == "Playoffs") %>% 
  summarize(count= n())%>%
  arrange(desc(count))%>%
  ggplot(aes(x = result_zone_semi, y = count))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


shot_merged %>%
  filter(result_off == "Stephen Curry")%>%
  group_by(result_contest, shotresult, result_zone_semi)%>%
  summarize(count= n())%>%
  mutate(fg_pct = count/sum(count))%>%
  arrange(desc(count))%>%
  filter(count > 1)%>%
  filter(result_contest == "Contested" | result_contest == "Open") %>% 
  ggplot(aes(x = result_zone_semi, y = fg_pct, fill = result_contest))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

synergy <- shot_merged%>%
  group_by(season_type, result_zone_semi)%>%
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(desc(pts)) %>% 
  ungroup()%>%
  filter(count_shot > 20)%>%
ggplot(aes(x = result_zone_semi, y = pps, fill = factor(season_type))) +
  geom_bar(stat= "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(fill = "Season Type", x = "Shot Location", title = "Difference in PPS between Regular Season and Playoffs")


shot_merged%>%
  filter(result_off == "Stephen Curry" |result_off == "Jordan Poole"| result_off == "Andrew Wiggins" & passer_name != "NULL")%>%
  group_by(result_off, passer_name)%>%
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(desc(pts)) %>% 
  ungroup()%>%
  filter(passer_name != "NULL")%>%
  filter(count_shot > 30)%>%
  ggplot(aes(x = result_off, y = pps)) +
  geom_point(aes(size = count_shot, color = passer_name), alpha = 0.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylim(1,1.5)


shot_merged %>%
  filter(season_type == "Playoffs" & play_type == "Iso")%>%
  group_by(result_off, result_zone_semi)%>%
  summarize(count = n())%>%
  arrange(desc(count))

playoffs <- read.csv("playoffplays.csv")

playoffs2 <- playoffs %>%
  filter(season_type == "Playoffs")

regular <- playoffs %>%
  filter(season_type == "Regular Season")


colnames(regular)[1]<- "regular_seas" 
colnames(regular)[5]<- "regular_pps" 

total <- merge(regular, playoffs2, by = c("play_type", "result_zone_condensed_7"))
difftotal <- total %>%
  mutate(diff_pps = pps - regular_pps)%>%
  filter(prop_shotzone.y > 0.01)


difftotal %>%
  filter(play_type == "Iso" & prop_shotzone.y > 0.08)%>%
  ggplot(aes(x = result_zone_condensed_7, y = diff_pps))+
           geom_bar(stat = "identity")


playtype_playoffs <- read.csv("justin.csv")

playtype_playoffs2 <- playtype_playoffs %>%
  filter(season_type == "Playoffs")

regular2 <- playtype_playoffs %>%
  filter(season_type == "Regular Season")


colnames(regular2)[1]<- "regular_seas" 
colnames(regular2)[5]<- "regular_ppp" 

total2 <- merge(regular2, playtype_playoffs2, by = c("play_type", "poss_type"))
difftotal2 <- total2 %>%
  mutate(diff_ppp = ppp - regular_ppp)%>%
  filter(playrate.y > 0.01)




screens <- read.csv("playoffs_screens.csv")


screenstable <- screens %>%
  group_by(screener_BH_pasting, season_type) %>% 
  filter(total_points >10 & screener_BH_pasting == "Draymond Green") %>% 
  ggplot(aes(x = player_name, y = pps, fill = factor(season_type))) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue")+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "")



screens %>%
  group_by(season_type, screener_BH_pasting, player_name) %>% 
  filter(total_points >20) %>% 
  ggplot(aes(x = player_name, y = pps, fill = factor(screener_BH_pasting))) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~season_type)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(fill = "Screener Name", title = "Impact on Screener on Points Per Shot", subtitle = "Regular Season vs Playoffs(Minimum of 20 points created)", x = "Player Name", y = "Points Per Shot")

#POOLE
shot_merged %>%
  filter(result_off == "Jordan Poole")%>%
  group_by(result_zone_semi)%>%
  summarize(count= n())%>%
  arrange(desc(count))+
  ggplot(aes(x = result_zone_semi, y = count))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


shot_merged %>%
  filter(result_off == "Jordan Poole")%>%
  group_by(result_zone_semi)%>%
  filter(season_type == "Playoffs") %>% 
  summarize(count= n())%>%
  arrange(desc(count))%>%
  ggplot(aes(x = result_zone_semi, y = count))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


shot_merged %>%
  filter(result_off == "Jordan Poole")%>%
  group_by(result_contest, shotresult, result_zone_semi)%>%
  summarize(count= n())%>%
  mutate(fg_pct = count/sum(count))%>%
  arrange(desc(count))%>%
  filter(count > 15)%>%
  filter(result_contest == "Average" | result_contest == "Open" | result_contest == "Plus" | result_contest == "Poor") %>% 
  ggplot(aes(x = result_zone_semi, y = fg_pct, fill = result_contest))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

shot_merged%>%
  filter(result_off == "Jordan Poole" & passer_name != "NULL")%>%
  group_by(passer_name, shotresult)%>%
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(desc(pts)) %>% 
  ungroup()



#DRAYMOND
shot_merged %>%
  filter(passer_name == "Draymond Green" & assisted ==1)%>%
  group_by(result_zone_semi, result_off)%>%
  summarize(count= n())%>%
  arrange(desc(count))%>%
  filter(count >10) %>% 
  ggplot(aes(x = result_zone_semi, y = count, fill = result_off))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  labs(title = "Shot Location Where Draymond Green Most Frequently Assists", subtitle = "Broken Down by Player (Min 10 Shots)",
       x = "Shot Location", fill = "Shooter")






results_merged %>%
  filter(result_off == "Draymond Green")%>%
  group_by(play_type)%>%
  summarize(count= n())%>%
  arrange(desc(count))%>%
  ggplot(aes(x = play_type, y = count))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




results_merged %>%
  filter(result_off == "Draymond Green")%>%
  group_by(result_zone_semi)%>%
  summarize(count= n())%>%
  filter(season_type == "Playoffs") %>% 
  arrange(desc(count))%>%
  ggplot(aes(x = result_zone_semi, y = count))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


results_merged %>%
  filter(result_off == "Draymond Green" & play_type == "Post" & doubleteam == 1) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(desc(pts)) %>% 
  ungroup()







shot_merged %>%
  filter(season_type == "Regular Season")%>%
  group_by(shotresult, result_zone_condensed)%>%
  summarize(count= n())%>%
  mutate(fg_pct = count/sum(count))%>%
  arrange(desc(count))%>%
  filter(count > 10)%>%
  filter(result_contest == "Average" | result_contest == "Open" | result_contest == "Plus" | result_contest == "Poor") %>% 
  ggplot(aes(x = result_zone_semi, y = fg_pct, fill = result_contest))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

shot_merged%>%
  filter(result_off == "Andrew Wiggins" & passer_name != "NULL")%>%
  group_by(passer_name, shotresult)%>%
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(desc(pts)) %>% 
  ungroup()
  







#with location
shot_merged %>%
  filter(result_type == "Catch & Shoot")%>%
  group_by(result_off, shotresult, result_zone_semi) %>% 
  summarize(count = n())%>%
  mutate(catchshootpct = count/sum(count)) %>% 
  filter(shotresult == 1 & count > 10) %>% 
  arrange(desc(catchshootpct))
  

shot_merged %>%
  filter(result_type == "3+D Pull-Up" | result_type == "1-2D Pull-Up")%>%
  group_by(result_off, shotresult, result_zone_semi) %>% 
  summarize(count = n())%>%
  mutate(pulluppct = count/sum(count))%>%
  filter(count > 10 & shotresult == 1)%>%
  arrange(desc(pulluppct))


shot_merged %>%
  filter(result_off == "Andrew Wiggins")%>%
  filter(result_zone_condensed == "Three-Point Attempt")%>%
  filter(play_type == "Ball Screen")%>%
  group_by(result_zone_condensed, result_type, play_type, shotresult)%>%
  summarize(count = n())%>%
  arrange(desc(count))


fgpct <- shot_merged %>%
  filter(play_type == "Ball Screen")%>%
  group_by(shotresult, result_off, result_zone_condensed)%>%
  summarize(count = n())%>%
  filter(count > 10)%>%
  mutate(fgpct = count/sum(count))%>%
  arrange(desc(count))
fgscreen <- merge(fgpct, ball_screen, by = c("result_off", "result_zone_condensed"))


results_merged$doubleteam <- ifelse(results_merged$result_def2_id == "NULL", 0,1)

results_merged %>%
  group_by(play_type, poss_type) %>%
 # filter(play_type == "Post")%>%
  summarize(count3 = n(),
            possession = sum(row_counter))%>%
  mutate(ppp = sum(points)/sum(possession))%>%
  arrange(desc(ppp))




#double team graph
doubleteam <- shot_merged %>%
  filter(doubleteam == 1)%>%
  group_by(doubleteam,result_off, shotresult)%>%
  summarize(count = n())%>%
  mutate(fgpct = count/sum(count))%>%
  filter(shotresult ==1)%>%
  filter(count >10)%>%
  arrange(desc(count))%>%
  filter(result_off != "Andre Iguodala" & result_off != "Chris Chiozza" &
           result_off != "Juan Toscano-Anderson" &result_off != "Moses Moody" & result_off != "Otto Porter Jr.") %>% 
  ggplot(aes(x = result_off, y = count, fill = factor(doubleteam), alpha = fgpct))+
  geom_bar(stat= "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



shot_merged <- shot_merged %>%
  arrange(id.x)

#FG% by type of shot 
shot_merged %>%
  #filter(result_off == "Stephen Curry" | result_off == "Klay Thompson")%>%
  group_by(season_type, play_type, shotresult)%>%
  summarize(count = n())%>%
  mutate(fg_pct = count/sum(count))%>%
  arrange(desc(count))%>%
  filter(shotresult == 1)%>%
  filter(count > 30)%>%
  ggplot(aes(x = play_type, y = fg_pct, fill = count))+
  geom_bar(stat = "identity")+
  facet_wrap(. ~season_type)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "FG% by Type of Shot", subtitle = "Playoffs vs. Regular Season", x= "Shot Location", y = "FG%")





#defense
pbp_data%>%
  filter(off_def == "DEF")%>%
filter(off_team == "GSW")%>%
  group_by(play_type)%>%
  summarize(count = n())%>%
  arrange(desc(count))










result_merged2 %>%
  group_by(passer_name)%>%
  filter(assisted == 1)%>%
  summarize(count = n(), totalpoints = sum(points))%>%
  mutate(pointperpass = count/totalpoints)%>%
  arrange(desc(pointperpass))










result_merged2 <- head(result_merged2, - 3)  
data_new1 
result_merged2$points <- as.numeric(result_merged2$points)

sum(result_merged2$points)

shot_merged%>%
  group_by(result_zone_semi, points)%>%
  summarize(Count=n()) %>%
  arrange(desc(Count)) %>%
  filter(points > 1) %>%
  filter(Count > 200)%>%
  ggplot(aes(x = result_zone_semi, y = Count)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


pbp_data%>%
  group_by(play_zone)%>%
  summarize(Count2=n()) %>%
  arrange(desc(Count2))




ggplot(results_data, aes(result_type)) + 
  geom_bar()