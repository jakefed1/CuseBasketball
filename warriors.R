library(tidyverse)
options(scipen = 999)
library(RColorBrewer)

pbp <- read_csv(file = "syracuse_gsw_basic_pbp.csv")
results <- read_csv(file = "syracuse_gsw_basic_results.csv")

pbp <-
  pbp %>% 
  arrange(id)

results <-
  results %>% 
  arrange(id)


merged <- results %>%
  left_join(pbp, by = c("nba_game_id", "quarter", "result_id",
                        "home", "away", "game_date", "poss_time", "season",
                         "season_type", "poss_id", "off_team", "def_team", "oncourt_id"))

view(merged)

merged <-
  merged %>% 
  filter(result_type != "Broadcast Error")

warriors <-
  merged %>% 
  filter(off_team == "GSW")

view(warriors)

#Table 1

#We want to calculate the transition rate
#poss_in_transition/#poss
#But the issue is some possessions take multiple rows
#So we're creating a counter to assign each row a 1 or a 0 based on
#whether it is the first row in that possession
#And then we're counting all the 1's to get # of possessions
possession_counter <-
  warriors %>% 
  group_by(nba_game_id, quarter, poss_id) %>% 
  mutate(row_counter = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

d1 <- possession_counter %>%
  group_by(nba_game_id,quarter, poss_id, poss_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))

d1 <- d1 %>%
  group_by(poss_type) %>%
  summarize(possession_count = n(),
            points_total = sum(points_total)) %>%
  ungroup() %>%
  mutate(ppp = points_total/possession_count)


#There are 11,477 possessions in the season according to the following code
total_number_possessions <-
  possession_counter_counts %>% 
  summarize(total_poss = sum(possession))

#Transition rate: possessions with transition / possessions
#etc.
#These are rough estimates, as there are other things in the data
#that make these imperfect
trans_rate <- 802 / 11477
half_rate <- 9163 / 11477
secondary_rate <- 806/11477

#Trans_rate = 7%
#Secondary_rate = 7%
#Half_rate = 80%

#Next step: see if their transition rate changes based on whether the possession
#follows a make, miss, or turnover

#I don't know how to do that


#Table 2

#We want to figure out the percentage of possessions that have at
#least one of each play type

#Naomi did this
table_2 <-
  possession_counter %>% 
  group_by(play_type) %>% 
  summarize(count3 = n(),
            possession = sum(row_counter)) %>% 
  mutate(playrate = count3/sum(possession)) %>% 
  arrange(desc(playrate)) %>% 
  select(play_type, possession, playrate) %>% 
  ungroup()

#Table 3

#a)
#We want to find their shots by location
#i) Shot chart: This is publicly available
#ii) 3pt rate, rim rate, mid rate:

warriors_shots <-
  warriors %>% 
  filter(result_num == 1) %>% 
  filter(result_contest != "NULL") %>% 
  filter(result_contest != "Fouled")

#Save the above for later

#Just realized result_zone is different and better than play_zone. LFG
#Let's check the counts of shots in each zone, just for the heck of it
#I don't need the possession counter here... just doing shots
table_3_allzones <-
  warriors_shots %>% 
  group_by(result_zone) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots)) %>% 
  arrange(desc(prop_shotzone)) %>% 
  ungroup()


#Figure out how I want to group these into mids and rims

#I would do C&S vs drib J, but honestly I think that's more informative
#for individual players than the whole team

# table 4: Play_type by poss_type
table_4 <-
  possession_counter %>% 
  group_by(poss_type, play_type) %>% 
  summarize(count3 = n(),
            possession = sum(row_counter)) %>% 
  mutate(playrate = count3/sum(possession)) %>% 
  arrange(poss_type, desc(playrate)) %>% 
  select(-count3) %>% 
  ungroup()

# table 5: shot_type by poss_type
table_5 <-
  warriors_shots %>% 
  group_by(poss_type, result_zone) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  ungroup()

# table 6: shot_type by play_type
table_6 <-
  warriors_shots %>% 
  group_by(play_type, result_zone) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(play_type, desc(prop_shotzone)) %>% 
  ungroup()

# table 7: shot_type by play_type by poss_type
table_7 <-
  warriors_shots %>% 
  group_by(poss_type, play_type, result_zone) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(poss_type, play_type, desc(prop_shotzone)) %>% 
  ungroup()

warriors_shots_condensed <- warriors_shots %>%
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
                         "Mid-Right Deep Paint",
                         "Rim") ~ "Paint",
      result_zone %in% c("Short Backcourt", "Deep Backcourt") ~ "Backcourt"
    )
  )

#Let's remake the shot graphs with the zones. These will be more useful.
table_3_zones_condensed <-
  warriors_shots_condensed %>% 
  group_by(result_zone_condensed) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots)) %>% 
  arrange(desc(prop_shotzone)) %>% 
  ungroup()

# table 5: shot_type by poss_type
table_5_condensed <-
  warriors_shots_condensed %>% 
  group_by(poss_type, result_zone_condensed) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  ungroup()

# table 6: shot_type by play_type
table_6_condensed <-
  warriors_shots_condensed %>% 
  group_by(play_type, result_zone_condensed) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(play_type, desc(prop_shotzone)) %>% 
  ungroup()

# table 7: shot_type by play_type by poss_type
table_7_condensed <-
  warriors_shots_condensed %>% 
  group_by(poss_type, play_type, result_zone_condensed) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(poss_type, play_type, desc(prop_shotzone)) %>% 
  ungroup()

#Next step: add points per possession to every table that has possessions, and
#points per shot for every table that has shots

#Table 1: not really useful. We already assume mostly operate out of half court.
#Table 2: not useful, unless we integrate poss_type
#Table 3: Not useful.
#Table 3 condensed: Very useful as basic bio information.
#Expansion idea: 7 zones: paint, left mid, right mid, mid mid, left 3, right 3, mid 3
#Visualization ideas for table_3_condensed: Table, Owen J Phillips drawing the court

#Table 4: Useful, pretty much only for half-court though.
#Ideas for visualization: bar graph, side bar graph, etc.
#When we add in ppp, we could do height of the bars for usage and fill of the bars for efficiency
arranged <-
  table_4_ppp %>% 
  arrange(poss_type, play_type, ppp)
#Table 5: Useful for half-court, and for transition because of crazy percentages.
#Expansion idea: 7 zones: paint, left mid, right mid, mid mid, left 3, right 3, mid 3
#Visualization ideas: Courts, but facet wrapped

#Table 6: Useful. We can see what types of plays end in certain areas. 
#For example, the warriors look to get to the rim on isos.
#And, more specifically, they only run ball screens in the middle of the court.
#Expansion idea: the 7 zones
#Expansion idea: Do this for specific players. See who shoots most on screens, etc.
#See who is good at that, etc. See what areas they especially do it in.
#Helps us for scouting and defensive recommendations.

#Table 7: Not useful.

warriors_shots_condensed_7 <- warriors_shots_condensed %>%
  mutate(
    result_zone_condensed_7 = case_when(
      result_zone %in% c("Left Above Break 3", "Left Corner 3") ~ "Left 3",
      result_zone %in% c("Right Corner 3", "Right Above Break 3") ~ "Right 3",
      result_zone %in% c("Mid-Left Above Break 3", "Mid-Right Above Break 3") ~ "Middle 3",
      result_zone %in% c("Right Midrange", "Right Short Mid") ~ "Right Midrange",
      result_zone %in% c("Left Midrange", "Left Short Mid") ~ "Left Midrange",
      result_zone %in% c("Mid-Right Midrange", "Mid-Left Midrange") ~ "Middle Midrange",
      result_zone %in% c("Mid-Left Short Paint",
                         "Mid-Right Short Paint",
                         "Mid-Left Deep Paint",
                         "Mid-Right Deep Paint",
                         "Rim") ~ "Paint",
      result_zone %in% c("Short Backcourt", "Deep Backcourt") ~ "Backcourt"
    )
  )
#Table 3 with 7 zones
table_3_zones_condensed_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(result_zone_condensed_7) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots)) %>% 
  arrange(desc(prop_shotzone)) %>% 
  ungroup()
#Thoughts: not useful. Just doesn't really help us.

#Table 5 with 7 zones
table_5_condensed_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(poss_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  ungroup()
#Thoughts: Extremely useful for half-court.
#We can include both 3 zones and 7 zones. 3 for basic bio info,
#and 7 to show they use the middle of the court more than the sides.
#Also, don't forget their super high rim rate in transition.
#Plus, in transition, when they shoot 3s, they are mostly on the sides. So they send shooters to the sides.
#Perhaps ball handler is instructed not to pull up, to either find a corner 3 or get to the rim?

#Table 6 with 7 zones:
table_6_condensed_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n()) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot)) %>% 
  arrange(play_type, desc(prop_shotzone)) %>% 
  ungroup()
#Thoughts: Extremely useful. Ball screens and DHOs are concentrated in the middle.

#Let's add the PPP and PPS to all tables.


#Let's draw courts to visualize these for the most part.

#Things to do tomorrow:
#Somehow, we all have different tables. We have to decide 1, so we can use the same data the whole time.

#Perhaps for the courts, we can include both efficiency (PPP) and frequency.
#Maybe like color for efficiency, and _____ for frequency?




#New tables with points per possession (or points per shot when applicable)

table_1_ppp <- 
  possession_counter %>%
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
  mutate(ppp = points_total/possession_count,
         poss_rate = possession_count/sum(possession_count)) %>% 
  select(poss_type, poss_rate, ppp) %>% 
  arrange(desc(poss_rate))

table_2_ppp <- 
  possession_counter %>%
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
  mutate(ppp = points_total/possession_count,
         play_rate = possession_count/sum(possession_count)) %>% 
  select(play_type, play_rate, ppp) %>% 
  arrange(desc(play_rate))

table_3_allzones_pps <-
  warriors_shots %>% 
  group_by(result_zone) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots),
         pps = pts/count_shot) %>% 
  arrange(desc(prop_shotzone)) %>% 
  ungroup() %>% 
  select(result_zone, prop_shotzone, pps) %>% 
  arrange(desc(prop_shotzone))

table_4_ppp <- 
  possession_counter %>%
  group_by(nba_game_id,quarter, poss_id, poss_type, play_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))

table_4_ppp <-
  table_4_ppp %>%
  group_by(poss_type, play_type) %>%
  summarize(possession_count = n(),
            points_total = sum(points_total)) %>%
  ungroup() %>%
  mutate(ppp = points_total/possession_count) %>% 
  group_by(poss_type) %>% 
  mutate(playrate = possession_count/sum(possession_count)) %>%
  arrange(poss_type, desc(playrate)) %>% 
  select(poss_type, play_type, playrate, ppp)

table_5_pps <-
  warriors_shots %>% 
  group_by(poss_type, result_zone) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  ungroup() %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  select(poss_type, result_zone, prop_shotzone, pps)

table_6_pps <-
  warriors_shots %>% 
  group_by(play_type, result_zone) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  ungroup() %>% 
  arrange(play_type, desc(prop_shotzone)) %>% 
  select(play_type, result_zone, prop_shotzone, pps)

table_7_pps <-
  warriors_shots %>% 
  group_by(poss_type, play_type, result_zone) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(poss_type, play_type, desc(prop_shotzone)) %>% 
  ungroup() %>% 
  select(poss_type, play_type, result_zone, prop_shotzone, pps)

#Condensed versions
table_3_zones_condensed_pps <-
  warriors_shots_condensed %>% 
  group_by(result_zone_condensed) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots),
         pps = pts/count_shot) %>% 
  arrange(desc(prop_shotzone)) %>% 
  select(result_zone_condensed, prop_shotzone, pps) %>% 
  ungroup()

table_5_condensed_pps <-
  warriors_shots_condensed %>% 
  group_by(poss_type, result_zone_condensed) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  select(poss_type, result_zone_condensed, prop_shotzone, pps) %>% 
  ungroup()

table_6_condensed_pps <-
  warriors_shots_condensed %>% 
  group_by(play_type, result_zone_condensed) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(play_type, desc(prop_shotzone)) %>% 
  select(play_type, result_zone_condensed, prop_shotzone, pps) %>%
  ungroup()

table_7_condensed_pps <-
  warriors_shots_condensed %>% 
  group_by(poss_type, play_type, result_zone_condensed) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(poss_type, play_type, desc(prop_shotzone)) %>% 
  select(poss_type, play_type, result_zone_condensed, prop_shotzone, pps) %>%
  ungroup()


#7 zones
table_3_zones_condensed_pps_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/nrow(warriors_shots),
         pps = pts/count_shot) %>% 
  arrange(desc(prop_shotzone)) %>% 
  select(result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()

table_5_condensed_pps_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(poss_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(poss_type, desc(prop_shotzone)) %>% 
  select(poss_type, result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()

table_6_condensed_pps_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(play_type, desc(prop_shotzone)) %>%
  select(play_type, result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()

table_7_condensed_pps_7 <-
  warriors_shots_condensed_7 %>% 
  group_by(poss_type, play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(poss_type, play_type, desc(prop_shotzone)) %>% 
  select(poss_type, play_type, result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()


#Visualizations time

#table_3 will be just a table
#table_4: filter halfcourt, bar graph with playrate in descending order left to right,
#fill the bars with darker red for higher ppp
#table_5_condensed_pps_7: Perfect. Figure out how to do it later.
#table_6_condensed_pps_7: Perfect. Figure out how to do it later.
#table_7

#Justin said add mean contest rate on table_6_condensed_pps_7
warriors_shots_condensed_7_contest <- warriors_shots_condensed_7 %>%
  mutate(
    contest_group = case_when(
      result_contest == "Block" ~ 0,
      result_contest == "Alter" ~ 1,
      result_contest == "Plus" ~ 2,
      result_contest == "Average" ~ 3,
      result_contest == "Poor" ~ 4,
      result_contest == "Open" ~ 5
    )
  )
table_6_condensed_pps_7_contest <-
  warriors_shots_condensed_7_contest %>% 
  group_by(play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T),
            mean_contest = mean(contest_group, na.rm = TRUE)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(play_type, desc(prop_shotzone)) %>%
  select(play_type, result_zone_condensed_7, prop_shotzone, pps, mean_contest) %>%
  ungroup()

#Let's make that table 4 bar graph

table_4_half_ppp <-
  table_4_ppp %>% 
  filter(poss_type == "Halfcourt") %>% 
  filter(play_type != "NA") %>% 
  arrange(desc(playrate)) %>% 
  head(9)
## plot

table_4_bar <-
  table_4_half_ppp %>% 
  ggplot(mapping = aes(x = reorder(play_type, -playrate), y = playrate, fill = ppp)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(
    x = "Play Type",
    y = "Percentage of Possessions with Play Type",
    title = "Most Common Play Types Run by Warriors",
    subtitle = "Halfcourt Sets Only") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
table_4_bar

#Warriors color codes: save these for later use
#FFC72C
#1D428A

#table_3 will be just a table
#table_4: filter halfcourt, bar graph with playrate in descending order left to right,
#fill the bars with darker red for higher ppp
#table_5_condensed_pps_7: Perfect. Figure out how to do it later.
#table_6_condensed_pps_7: Perfect. Figure out how to do it later.
#table_7

#Group by playoffs and not playoffs

table_3_zones_condensed_pps_playoffs <-
  warriors_shots_condensed %>% 
  group_by(season_type, result_zone_condensed) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(season_type, desc(prop_shotzone)) %>% 
  select(season_type, result_zone_condensed, prop_shotzone, pps) %>% 
  ungroup()

table_4_ppp_playoffs <- 
  possession_counter %>%
  group_by(season_type, nba_game_id,quarter, poss_id, poss_type, play_type) %>%
  summarize(points_total = sum(as.numeric(points), na.rm = T),
            possession_total = sum(row_counter)) %>%
  ungroup() %>%
  mutate(poss_id = row_number()) %>%
  select(-c(nba_game_id, quarter))

table_4_ppp_playoffs <-
  table_4_ppp_playoffs %>%
  group_by(season_type, poss_type, play_type) %>%
  summarize(possession_count = n(),
            points_total = sum(points_total)) %>%
  ungroup() %>%
  mutate(ppp = points_total/possession_count) %>% 
  group_by(season_type, poss_type) %>% 
  mutate(playrate = possession_count/sum(possession_count)) %>%
  arrange(season_type, poss_type, desc(playrate)) %>% 
  select(season_type, poss_type, play_type, playrate, ppp)

#Then write the csv

table_5_condensed_pps_7_playoffs <-
  warriors_shots_condensed_7 %>% 
  group_by(season_type, poss_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(season_type, poss_type, desc(prop_shotzone)) %>% 
  select(season_type, poss_type, result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()

#Then write the csv

table_6_condensed_pps_7_playoffs <-
  warriors_shots_condensed_7 %>% 
  group_by(season_type, play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(season_type, play_type, desc(prop_shotzone)) %>%
  select(season_type, play_type, result_zone_condensed_7, prop_shotzone, pps) %>%
  ungroup()
#Then write the csv

table_7_condensed_pps_7_playoffs <-
  warriors_shots_condensed_7 %>% 
  group_by(season_type, poss_type, play_type, result_zone_condensed_7) %>% 
  summarize(count_shot = n(),
            pts = sum(as.numeric(points), na.rm = T)) %>% 
  mutate(prop_shotzone = count_shot/sum(count_shot),
         pps = pts/count_shot) %>% 
  arrange(season_type, poss_type, play_type, desc(prop_shotzone)) %>% 
  select(season_type, poss_type, play_type, result_zone_condensed_7,count_shot, prop_shotzone, pps) %>%
  ungroup()
#Then write the csv