library(tidyverse)
library(scales)
library(plotly)
load("gls2017.RData")
load("pls2017.RData")
setwd("~/Desktop/Level - R/Stattleship")

#Populate tables
players <- data.frame()

for( i in 1:length(pls)) {
  players <- rbind(players, pls[[i]]$players)
}
players <-
  players %>% 
  filter(active==TRUE)

players_with_salary <- players[,c(1,14,19,23,26,39)]

players1 <- players[,c(1,14,19,23,39)]


teams <- data.frame()
for( i in 1:length(pls)) {
  teams <- rbind(teams, pls[[i]]$teams)
}
teams <- teams[,c(1,9,10)]
players_with_team_name <- merge(players1, teams, by.x = "team_id", by.y = "id")
players_with_team_name$team_id <- NULL
players_with_team_name <- players_with_team_name %>% 
  group_by(id) %>% 
  filter(row_number(id) == 1)


game_logs <- data.frame()
for( i in 1:length(gls2017)) {
  game_logs <- rbind(game_logs, gls2017[[i]]$game_logs)
}

players_with_game_stats <- merge(players_with_team_name, game_logs, by.x = "id", by.y = "player_id")
players_with_game_stats$id.y <- NULL

players_with_game_stats <- players_with_game_stats %>% 
  rename(team_name = name)

#Team names
team_names <- c("Arizona", 
                "Atlanta", 
                "Baltimore", 
                "Boston", 
                "Chi. Cubs", 
                "Chi. White Sox", 
                "Cincinnati",
                "Cleveland",
                "Colorado",
                "Detroit",
                "Houston",
                "Kansas City",
                "LA Angels",
                "LA Dodgers",
                "Miami",
                "Milwaukee",
                "Minnesota",
                "NY Mets",
                "NY Yankees",
                "Oakland",
                "Philadelphia",
                "Pittsburgh",
                "San Diego",
                "San Francisco",
                "Seattle",
                "St. Louis",
                "Tampa Bay",
                "Texas",
                "Toronto",
                "Washington")


  
##############################################################################################################

#Define hitters and populate stats for each player
hitters <- players_with_game_stats[,c(1,2,3,4,5,9,10,25,29,30,32,33,34,35,36,38,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55)]


hitters <- hitters %>% 
  filter(position_abbreviation=="IF" 
         | position_abbreviation=="SS" 
         | position_abbreviation=="C" 
         | position_abbreviation=="1B" 
         | position_abbreviation=="2B" 
         | position_abbreviation=="LF" 
         | position_abbreviation=="CF" 
         | position_abbreviation=="OF" 
         | position_abbreviation=="3B" 
         | position_abbreviation=="DH" 
         | position_abbreviation=="RF") %>% 
  mutate(games_played1 = case_when(game_played==TRUE ~ 1,
                                          game_played==FALSE ~ 0))


#Hitters Stats
hitters_stats <- hitters %>% 
  group_by(id, first_name, last_name, position_abbreviation, team_name) %>% 
  summarise(wOBA = ((0.69*sum(walks, na.rm=TRUE)) + (0.72*sum(hit_by_pitch, na.rm = TRUE)) + 
                      (0.88*sum(singles, na.rm = TRUE)) + (1.247*sum(doubles, na.rm = TRUE)) + 
                      (1.578*sum(triples, na.rm=TRUE)) + 
                      (2.031*sum(home_runs, na.rm = TRUE)))/
                  (sum(at_bats, na.rm = TRUE) + sum(walks, na.rm = TRUE) + 
                  sum(intentional_walks, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE) + 
                  sum(hit_by_pitch, na.rm = TRUE)),
            OPS = mean(on_base_plus_slugging, na.rm = TRUE),
            small_ball = sum(stolen_bases, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE) + 
              sum(sacrifice_hits, na.rm = TRUE),
            runs_batted_in = sum(runs_batted_in, na.rm = TRUE),
            games_played = sum(games_played1, na.rm = TRUE),
            at_bats = sum(at_bats, na.rm = TRUE),
            hits = sum(hits, na.rm = TRUE),
            walks = sum(walks, na.rm = TRUE),
            intentional_walks = sum(intentional_walks, na.rm = TRUE),
            hit_by_pitch = sum(hit_by_pitch, na.rm = TRUE),
            singles = sum(singles, na.rm = TRUE),
            doubles = sum(doubles, na.rm = TRUE),
            triples = sum(triples, na.rm = TRUE),
            home_runs = sum(home_runs, na.rm = TRUE),
            extra_base_hits = sum(extra_base_hits, na.rm = TRUE),
            batting_average = sum(hits, na.rm = TRUE)/(sum(at_bats, na.rm = TRUE)-sum(walks, na.rm = TRUE)-sum(sacrifice_flys, na.rm = TRUE)-sum(sacrifice_hits, na.rm = TRUE)),
            runs = sum(runs, na.rm = TRUE),
            stolen_bases = sum(stolen_bases, na.rm = TRUE),
            strikeouts = sum(strikeouts, na.rm = TRUE),
            on_base_percentage = mean(on_base_percentage, na.rm = TRUE),
            slugging_percentage = mean(slugging_percentage, na.rm = TRUE),
            sacrifice_flys = sum(sacrifice_flys, na.rm = TRUE),
            sacrifice_hits = sum(sacrifice_hits, na.rm = TRUE),
            grounded_into_double_plays = sum(grounded_into_double_plays, na.rm = TRUE),
            rlisp_two_out = sum(rlisp_two_out, na.rm = TRUE),
            left_on_base = sum(left_on_base, na.rm = TRUE))


#Create roster of 9 hitters for each team
hitters_roster_function <- function(team_names){
  hitters_stats %>% 
    filter(team_name==team_names) %>% 
    arrange(desc(games_played)) %>% 
    head(9)
}

hitters_rosters <- map_dfr(.x=team_names, .f=hitters_roster_function)

#Hitting stats significances
team_sums <- hitters_rosters %>% 
    group_by(team_name) %>%
    summarize(wOBA = ((0.69*sum(walks, na.rm=TRUE)) + (0.72*sum(hit_by_pitch, na.rm = TRUE)) + 
                        (0.88*sum(singles, na.rm = TRUE)) + (1.247*sum(doubles, na.rm = TRUE)) + 
                        (1.578*sum(triples, na.rm=TRUE)) + 
                        (2.031*sum(home_runs, na.rm = TRUE)))/
                   (sum(at_bats, na.rm = TRUE) + sum(walks, na.rm = TRUE) + 
                   sum(intentional_walks, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE) + 
                   sum(hit_by_pitch, na.rm = TRUE)),
              OPS = mean(OPS, na.rm = TRUE),
              small_ball = sum(stolen_bases, na.rm = TRUE) + sum(sacrifice_flys, na.rm = TRUE) + 
                sum(sacrifice_hits, na.rm = TRUE),
              runs_batted_in = sum(runs_batted_in, na.rm = TRUE)) %>% 
  mutate(wins = case_when(team_name=="Arizona" ~ 93,
                          team_name=="Atlanta" ~ 72,
                          team_name=="Baltimore" ~ 75,
                          team_name=="Boston" ~ 93,
                          team_name=="Chi. Cubs" ~ 92,
                          team_name=="Chi. White Sox" ~ 67,
                          team_name=="Cincinnati" ~ 68,
                          team_name=="Cleveland" ~ 102,
                          team_name=="Colorado" ~ 87,
                          team_name=="Detroit" ~ 64,
                          team_name=="Houston" ~ 101,
                          team_name=="Kansas City" ~ 80,
                          team_name=="LA Angels" ~ 80,
                          team_name=="LA Dodgers" ~ 104,
                          team_name=="Miami" ~ 77,
                          team_name=="Milwaukee" ~ 86,
                          team_name=="Minnesota" ~ 85,
                          team_name=="NY Mets" ~ 70,
                          team_name=="NY Yankees" ~ 91,
                          team_name=="Oakland" ~ 75,
                          team_name=="Philadelphia" ~ 66,
                          team_name=="Pittsburgh" ~ 75,
                          team_name=="San Diego" ~ 71,
                          team_name=="San Francisco" ~ 64,
                          team_name=="Seattle" ~ 78,
                          team_name=="St. Louis" ~ 83,
                          team_name=="Tampa Bay" ~ 80,
                          team_name=="Texas" ~ 78,
                          team_name=="Toronto" ~ 76,
                          team_name=="Washington" ~ 97))

cor(team_sums$wOBA, team_sums$wins)
#0.7253
a <- lm(team_sums$wOBA ~ team_sums$wins)
anova(a)
#f-value = 31.086

cor(team_sums$OPS, team_sums$wins)
#0.6811
b <- lm(team_sums$OPS ~ team_sums$wins)
anova(b)
#f-value = 24.227

cor(team_sums$small_ball, team_sums$wins)
#0.3982
c <- lm(team_sums$small_ball ~ team_sums$wins)
anova(c)
#f-value = 5.2773

cor(team_sums$runs_batted_in, team_sums$wins)
#0.6785
d <- lm(team_sums$runs_batted_in ~ team_sums$wins)
anova(d)
#f-value = 23.882

############################################################################################

#Define hitters and populate stats for each player
pitchers <- players_with_game_stats[,c(1,2,3,4,5,9,10,37,57,58,58,60,61,63,64,65,68,69,70,71,72,78,85,86,87,88,89,90,93,94,95,96)]

pitchers <- pitchers %>%
  filter(position_abbreviation=="SP" | position_abbreviation=="P" | position_abbreviation=="RP") %>% 
  mutate(earned_runs = (earned_run_average*innings_pitched)/9) %>% 
  mutate(games_started1 =  case_when(game_started==TRUE ~ 1,
                                     game_started==FALSE ~ 0))

pitchers_stats <- pitchers %>% 
  group_by(id, first_name, last_name, position_abbreviation, team_name) %>% 
  summarise(era = (sum(earned_runs, na.rm = TRUE) * 9)/sum(innings_pitched, na.rm = TRUE)
            , whip = (sum(pitcher_walks, na.rm = TRUE) + sum(pitcher_hits, na.rm = TRUE))/sum(innings_pitched, na.rm = TRUE)
            , strikeouts_per_nine = (sum(pitcher_strikeouts, na.rm = TRUE)/sum(innings_pitched, na.rm=TRUE))*9
            , my_metric = (sum(ground_ball_outs, na.rm = TRUE))/(sum(ground_ball_outs, na.rm = TRUE) + sum(fly_ball_outs, na.rm = TRUE) + sum(pitcher_strikeouts, na.rm = TRUE)) + sum(quality_starts, na.rm = TRUE),
            innings_pitched = sum(innings_pitched, na.rm = TRUE),
            games_started = sum(games_started1, na.rm = TRUE),
            holds = sum(holds, na.rm = TRUE),
            saves = sum(saves, na.rm = TRUE))

#Create rosters of 5 starting pitchers, 1 closer, and 4 relief pitchers for each team.
starting_pitchers <- pitchers_stats %>%
  filter(position_abbreviation=="SP" | position_abbreviation=="P",
         between(games_started,5,35))

starting_pitchers_roster_function <- function(team_names){
  starting_pitchers %>% 
    filter(team_name==team_names) %>% 
    arrange(desc(games_started)) %>% 
    head(5)
}

starting_pitchers_rosters <- map_dfr(.x=team_names, .f=starting_pitchers_roster_function)

closers <- pitchers_stats %>%
  filter(position_abbreviation=="RP" | position_abbreviation=="P",
         saves>=1)

closers_roster_function <- function(team_names){
  closers %>% 
    filter(team_name==team_names) %>% 
    arrange(desc(saves)) %>% 
    head(1)
}

closers_rosters <- map_dfr(.x=team_names, .f=closers_roster_function)


relief_pitchers <- pitchers_stats %>%
  filter(position_abbreviation=="RP" | position_abbreviation=="P",
         innings_pitched<150)

relief_pitchers_roster_function <- function(team_names){
  relief_pitchers %>% 
    filter(team_name==team_names, !(id %in% closers_rosters$id), !(id %in% starting_pitchers_rosters$id)) %>% 
    arrange(desc(innings_pitched)) %>% 
    head(4)
}

relief_pitchers_rosters <- map_dfr(.x=team_names, .f=relief_pitchers_roster_function)

#Pitcher stat significances
starting_pitchers_team_sums <- starting_pitchers_rosters %>% 
  group_by(team_name) %>% 
  summarise(era = mean(era, na.rm = TRUE)
            , whip = mean(whip, na.rm = TRUE)
            , strikeouts_per_nine = mean(strikeouts_per_nine, na.rm = TRUE)
            , my_metric = mean(my_metric, na.rm = TRUE)) %>% 
  mutate(wins = case_when(team_name=="Arizona" ~ 93,
                          team_name=="Atlanta" ~ 72,
                          team_name=="Baltimore" ~ 75,
                          team_name=="Boston" ~ 93,
                          team_name=="Chi. Cubs" ~ 92,
                          team_name=="Chi. White Sox" ~ 67,
                          team_name=="Cincinnati" ~ 68,
                          team_name=="Cleveland" ~ 102,
                          team_name=="Colorado" ~ 87,
                          team_name=="Detroit" ~ 64,
                          team_name=="Houston" ~ 101,
                          team_name=="Kansas City" ~ 80,
                          team_name=="LA Angels" ~ 80,
                          team_name=="LA Dodgers" ~ 104,
                          team_name=="Miami" ~ 77,
                          team_name=="Milwaukee" ~ 86,
                          team_name=="Minnesota" ~ 85,
                          team_name=="NY Mets" ~ 70,
                          team_name=="NY Yankees" ~ 91,
                          team_name=="Oakland" ~ 75,
                          team_name=="Philadelphia" ~ 66,
                          team_name=="Pittsburgh" ~ 75,
                          team_name=="San Diego" ~ 71,
                          team_name=="San Francisco" ~ 64,
                          team_name=="Seattle" ~ 78,
                          team_name=="St. Louis" ~ 83,
                          team_name=="Tampa Bay" ~ 80,
                          team_name=="Texas" ~ 78,
                          team_name=="Toronto" ~ 76,
                          team_name=="Washington" ~ 97))

cor(starting_pitchers_team_sums$era, starting_pitchers_team_sums$wins)
#-0.7775
e <- lm(starting_pitchers_team_sums$era ~ starting_pitchers_team_sums$wins)
anova(e)
#F-value = 46.038

cor(starting_pitchers_team_sums$whip, starting_pitchers_team_sums$wins)
#-0.8095
f <- lm(starting_pitchers_team_sums$whip ~ starting_pitchers_team_sums$wins)
anova(f)
#46.635

cor(starting_pitchers_team_sums$strikeouts_per_nine, starting_pitchers_team_sums$wins)
#0.7185
g <- lm(starting_pitchers_team_sums$strikeouts_per_nine ~ starting_pitchers_team_sums$wins)
anova(g)
#29.759

cor(starting_pitchers_team_sums$my_metric, starting_pitchers_team_sums$wins)
#0.5839
h <- lm(starting_pitchers_team_sums$my_metric ~ starting_pitchers_team_sums$wins)
anova(h)
#19.114

relief_pitchers_team_sums <- relief_pitchers_rosters %>% 
  group_by(team_name) %>% 
  summarise(era = mean(era, na.rm = TRUE)
            , whip = mean(whip, na.rm = TRUE)
            , strikeouts_per_nine = mean(strikeouts_per_nine, na.rm = TRUE)
            , holds = sum(holds, na.rm = TRUE)) %>% 
  mutate(wins = case_when(team_name=="Arizona" ~ 93,
                          team_name=="Atlanta" ~ 72,
                          team_name=="Baltimore" ~ 75,
                          team_name=="Boston" ~ 93,
                          team_name=="Chi. Cubs" ~ 92,
                          team_name=="Chi. White Sox" ~ 67,
                          team_name=="Cincinnati" ~ 68,
                          team_name=="Cleveland" ~ 102,
                          team_name=="Colorado" ~ 87,
                          team_name=="Detroit" ~ 64,
                          team_name=="Houston" ~ 101,
                          team_name=="Kansas City" ~ 80,
                          team_name=="LA Angels" ~ 80,
                          team_name=="LA Dodgers" ~ 104,
                          team_name=="Miami" ~ 77,
                          team_name=="Milwaukee" ~ 86,
                          team_name=="Minnesota" ~ 85,
                          team_name=="NY Mets" ~ 70,
                          team_name=="NY Yankees" ~ 91,
                          team_name=="Oakland" ~ 75,
                          team_name=="Philadelphia" ~ 66,
                          team_name=="Pittsburgh" ~ 75,
                          team_name=="San Diego" ~ 71,
                          team_name=="San Francisco" ~ 64,
                          team_name=="Seattle" ~ 78,
                          team_name=="St. Louis" ~ 83,
                          team_name=="Tampa Bay" ~ 80,
                          team_name=="Texas" ~ 78,
                          team_name=="Toronto" ~ 76,
                          team_name=="Washington" ~ 97))

cor(relief_pitchers_team_sums$era, relief_pitchers_team_sums$wins)
#-0.5789
i <- lm(relief_pitchers_team_sums$era ~ relief_pitchers_team_sums$wins)
anova(i)
#13.769

cor(relief_pitchers_team_sums$whip, relief_pitchers_team_sums$wins)
#-0.6317
j <- lm(relief_pitchers_team_sums$whip ~ relief_pitchers_team_sums$wins)
anova(j)
#19.297

cor(relief_pitchers_team_sums$strikeouts_per_nine, relief_pitchers_team_sums$wins)
#0.5685
k <- lm(relief_pitchers_team_sums$strikeouts_per_nine ~ relief_pitchers_team_sums$wins)
anova(k)
#12.168

cor(relief_pitchers_team_sums$holds, relief_pitchers_team_sums$wins)
#0.6295
l <- lm(relief_pitchers_team_sums$holds ~ relief_pitchers_team_sums$wins)
anova(l)
#16.113


closers_team_sums <- closers_rosters %>% 
  group_by(team_name) %>% 
  summarise(era = mean(era, na.rm = TRUE)
            , whip = mean(whip, na.rm = TRUE)
            , strikeouts_per_nine = mean(strikeouts_per_nine, na.rm = TRUE)
            , saves = sum(saves, na.rm = TRUE)) %>% 
  mutate(wins = case_when(team_name=="Arizona" ~ 93,
                          team_name=="Atlanta" ~ 72,
                          team_name=="Baltimore" ~ 75,
                          team_name=="Boston" ~ 93,
                          team_name=="Chi. Cubs" ~ 92,
                          team_name=="Chi. White Sox" ~ 67,
                          team_name=="Cincinnati" ~ 68,
                          team_name=="Cleveland" ~ 102,
                          team_name=="Colorado" ~ 87,
                          team_name=="Detroit" ~ 64,
                          team_name=="Houston" ~ 101,
                          team_name=="Kansas City" ~ 80,
                          team_name=="LA Angels" ~ 80,
                          team_name=="LA Dodgers" ~ 104,
                          team_name=="Miami" ~ 77,
                          team_name=="Milwaukee" ~ 86,
                          team_name=="Minnesota" ~ 85,
                          team_name=="NY Mets" ~ 70,
                          team_name=="NY Yankees" ~ 91,
                          team_name=="Oakland" ~ 75,
                          team_name=="Philadelphia" ~ 66,
                          team_name=="Pittsburgh" ~ 75,
                          team_name=="San Diego" ~ 71,
                          team_name=="San Francisco" ~ 64,
                          team_name=="Seattle" ~ 78,
                          team_name=="St. Louis" ~ 83,
                          team_name=="Tampa Bay" ~ 80,
                          team_name=="Texas" ~ 78,
                          team_name=="Toronto" ~ 76,
                          team_name=="Washington" ~ 97))

cor(closers_team_sums$era, closers_team_sums$wins)
#-0.4866
m <- lm(closers_team_sums$era ~ closers_team_sums$wins)
anova(m)
#8.6861

cor(closers_team_sums$whip, closers_team_sums$wins)
#-0.5774
n <- lm(closers_team_sums$whip ~ closers_team_sums$wins)
anova(n)
#14.001

cor(closers_team_sums$strikeouts_per_nine, closers_team_sums$wins)
#0.3306
o <- lm(closers_team_sums$strikeouts_per_nine ~ closers_team_sums$wins)
anova(o)
#3.4365

cor(closers_team_sums$saves, closers_team_sums$wins)
#0.5589
p <- lm(closers_team_sums$saves ~ closers_team_sums$wins)
anova(p)
#12.716

#Normalize all stats
normalized_hitters_rosters <- hitters_rosters
normalized_hitters_rosters$wOBA <- rescale(hitters_rosters$wOBA, to = c(0,10))
normalized_hitters_rosters$OPS <- rescale(hitters_rosters$OPS, to = c(0,10))
normalized_hitters_rosters$small_ball <- rescale(hitters_rosters$small_ball, to = c(0,10))
normalized_hitters_rosters$runs_batted_in <- rescale(hitters_rosters$runs_batted_in, to = c(0,10))

normalized_starting_pitchers_rosters <- starting_pitchers_rosters
normalized_starting_pitchers_rosters$era <- rescale(starting_pitchers_rosters$era, to = c(0,10))
normalized_starting_pitchers_rosters$whip <- rescale(starting_pitchers_rosters$whip, to = c(0,10))
normalized_starting_pitchers_rosters$strikeouts_per_nine <- rescale(starting_pitchers_rosters$strikeouts_per_nine, to = c(0,10))
normalized_starting_pitchers_rosters$my_metric <- rescale(starting_pitchers_rosters$era, to = c(0,10))


normalized_relief_pitchers_rosters <- relief_pitchers_rosters
normalized_relief_pitchers_rosters$era <- rescale(relief_pitchers_rosters$era, to = c(0,10))
normalized_relief_pitchers_rosters$whip <- rescale(relief_pitchers_rosters$whip, to = c(0,10))
normalized_relief_pitchers_rosters$strikeouts_per_nine <- rescale(relief_pitchers_rosters$strikeouts_per_nine, to = c(0,10))
normalized_relief_pitchers_rosters$holds <- rescale(relief_pitchers_rosters$holds, to = c(0,10))

normalized_closers_rosters <- closers_rosters
normalized_closers_rosters$era <- rescale(closers_rosters$era, to = c(0,10))
normalized_closers_rosters$whip <- rescale(closers_rosters$whip, to = c(0,10))
normalized_closers_rosters$strikeouts_per_nine <- rescale(closers_rosters$strikeouts_per_nine, to = c(0,10))
normalized_closers_rosters$saves <- rescale(closers_rosters$saves, to = c(0,10))

#F-values and stat weights
f_values <- tibble("Stat" = c("wOBA", "OPS", "SmallB", "RBIs", "S.ERA", "S.WHIP", "S.K/9", "my_metric", "R.ERA", "R.WHIP", "R.K/9", "Holds", "C.ERA", "C.WHIP", "C.K/9", "Saves"),
                   "f-value" = c(31.086,24.227,5.2773,23.882,46.038,46.635,29.759,19.114,13.769,19.297,12.168,16.113,8.6861,14.001,3.4365,12.716))

f_values$`f-value` <- rescale(f_values$`f-value`, to = c(1,10))

f_values$Stat <- factor(f_values$Stat, levels = f_values$Stat[order(desc(f_values$`f-value`))])

ggplot(f_values, aes(x=Stat, y=`f-value`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Stat Weights") +
  theme(plot.title = element_text(hjust = 0.5))

barplot(f_values, space=1, names = c("wOBA", "OPS", "SmallB", "RBIs", "S.ERA", "S.WHIP", "S.K/9", "my_metric", "R.ERA", "R.WHIP", "R.K/9", "Holds", "C.ERA", "C.WHIP", "C.K/9", "Saves"), cex.names=0.75, ylab = "F-value", las=2)
weights <- rescale(fvalues, to = c(1,10))
weights <- f_values$`f-value`

#Hitters player values
hitters_player_values <-normalized_hitters_rosters[,c(1,2,3,4,5,6,7,8,9)]
hitters_player_values$wOBA <- normalized_hitters_rosters$wOBA*weights[1]
hitters_player_values$OPS <- normalized_hitters_rosters$OPS*weights[2]
hitters_player_values$small_ball <- normalized_hitters_rosters$small_ball*weights[3]
hitters_player_values$runs_batted_in <- normalized_hitters_rosters$runs_batted_in*weights[4]
hitters_player_values <- hitters_player_values %>%
  group_by(id) %>% 
  mutate(player_value = sum(wOBA, OPS, small_ball, runs_batted_in))

hitters_player_values <- hitters_player_values[,c(1,2,3,4,5,10)]

#Starting pitchers player values
starting_pitchers_player_values <- normalized_starting_pitchers_rosters[,c(1,2,3,4,5,6,7,8,9)]
starting_pitchers_player_values$era <- (10-normalized_starting_pitchers_rosters$era)*weights[5]
starting_pitchers_player_values$whip <- (10-normalized_starting_pitchers_rosters$whip)*weights[6]
starting_pitchers_player_values$strikeouts_per_nine <- normalized_starting_pitchers_rosters$strikeouts_per_nine*weights[7]
starting_pitchers_player_values$my_metric <- normalized_starting_pitchers_rosters$my_metric*weights[8]
starting_pitchers_player_values <- starting_pitchers_player_values %>% 
  group_by(id) %>% 
  mutate(player_value = sum(era, whip, strikeouts_per_nine, my_metric))

starting_pitchers_player_values <- starting_pitchers_player_values %>% 
  mutate(position_abbreviation=replace(position_abbreviation, position_abbreviation=="P", "SP"))

starting_pitchers_player_values <- starting_pitchers_player_values[,c(1,2,3,4,5,10)]

#Relief pitchers player values
relief_pitchers_player_values <- normalized_relief_pitchers_rosters[,c(1,2,3,4,5,6,7,8,12)]
relief_pitchers_player_values$era <- (10-normalized_relief_pitchers_rosters$era)*weights[9]
relief_pitchers_player_values$whip <- (10-normalized_relief_pitchers_rosters$whip)*weights[10]
relief_pitchers_player_values$strikeouts_per_nine <- normalized_relief_pitchers_rosters$strikeouts_per_nine*weights[11]
relief_pitchers_player_values$holds <- normalized_relief_pitchers_rosters$holds*weights[12]
relief_pitchers_player_values <- relief_pitchers_player_values %>% 
  group_by(id) %>% 
  mutate(player_value = sum(era, whip, strikeouts_per_nine, holds))

relief_pitchers_player_values <- relief_pitchers_player_values %>% 
  mutate(position_abbreviation=replace(position_abbreviation, position_abbreviation=="P", "RP"))

relief_pitchers_player_values <- relief_pitchers_player_values[,c(1,2,3,4,5,10)]

#Closers player values
closers_player_values <- normalized_closers_rosters[,c(1,2,3,4,5,6,7,8,13)]
closers_player_values$era <- (10-normalized_closers_rosters$era)*weights[13]
closers_player_values$whip <- (10-normalized_closers_rosters$whip)*weights[14]
closers_player_values$strikeouts_per_nine <- normalized_closers_rosters$strikeouts_per_nine*weights[15]
closers_player_values$saves <- normalized_closers_rosters$saves*weights[16]
closers_player_values <- closers_player_values %>% 
  group_by(id) %>% 
  mutate(player_value = sum(era, whip, strikeouts_per_nine, saves))

closers_player_values <- closers_player_values %>% 
  mutate(position_abbreviation=replace(position_abbreviation, position_abbreviation=="RP", "CL"))

closers_player_values <- closers_player_values[,c(1,2,3,4,5,10)]

#Table of all players with player values
player_values <- rbind(hitters_player_values, starting_pitchers_player_values, relief_pitchers_player_values, closers_player_values)
player_values$player_value <- rescale(player_values$player_value, to = c(0,100))

#Hitters bar chart
hitters_graph <- player_values %>%
  filter(position_abbreviation!="SP", position_abbreviation!="P", position_abbreviation!="RP") %>% 
  arrange(desc(player_value)) %>% 
  head(12)

hitters_graph$last_name <- factor(hitters_graph$last_name, levels = hitters_graph$last_name[order(desc(hitters_graph$player_value))])

ggplot(hitters_graph, aes(x=last_name, y=player_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Best Hitters") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(50,65)) +
  xlab("Last Name") +
  ylab("Player Value")


#Starting pitchers bar chart
SP_graph <- player_values %>% 
       filter(position_abbreviation=="SP") %>% 
       arrange(desc(player_value)) %>% 
  head(12)

SP_graph$last_name <- factor(SP_graph$last_name, levels = SP_graph$last_name[order(desc(SP_graph$player_value))])

ggplot(SP_graph, aes(x=last_name, y=player_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Best Starting Pitchers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(80,100)) +
  xlab("Last Name") +
  ylab("Player Value")


#Relief pitchers bar chart
RP_graph <- player_values %>% 
       filter(position_abbreviation=="RP") %>% 
       arrange(desc(player_value)) %>% 
  head(12)

RP_graph$last_name <- factor(RP_graph$last_name, levels = RP_graph$last_name[order(desc(RP_graph$player_value))])

ggplot(RP_graph, aes(x=last_name, y=player_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Best Relief Pitchers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(35,50)) +
  xlab("Last Name") +
  ylab("Player Value")

#Closers bar chart
CL_graph <- player_values %>% 
       filter(position_abbreviation=="CL") %>% 
       arrange(desc(player_value)) %>% 
  head(12)

CL_graph$last_name <- factor(CL_graph$last_name, levels = CL_graph$last_name[order(desc(CL_graph$player_value))])

ggplot(CL_graph, aes(x=last_name, y=player_value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Best") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(15,30)) +
  xlab("Last Name") +
  ylab("Player Value")


#Team rankings
team_values <- player_values %>% 
  group_by(team_name) %>% 
  summarise(team_value = sum(player_value, na.rm = TRUE)) %>% 
  arrange(desc(team_value))

team_values$team_value <- rescale(team_values$team_value, to = c(0,100))


#Compare salaries
player_values_with_salary <- merge(player_values, players_with_salary, by.x = "id", by.y = "id")

player_values_with_salary <- player_values_with_salary[,c(1,2,3,4,5,6,10)]

player_values_with_salary %>% 
  arrange(desc(salary)) %>% 
  select(first_name.x, last_name.x, position_abbreviation.x, team_name, player_value, salary)

rescale(player_values_with_salary$player_value, to = c(0,35000000))

player_values_with_salary$deserving_salary <- NA
player_values_with_salary$deserving_salary <- rescale(player_values_with_salary$player_value, to = c(0,35000000))
player_values_with_salary$sal_difference <- NA
player_values_with_salary$sal_difference <- player_values_with_salary$salary - player_values_with_salary$deserving_salary

player_values_with_salary <- player_values_with_salary %>% 
  rename(first_name=first_name.x,
         last_name=last_name.x,
         position_abbreviation=position_abbreviation.x)


#All player's salaries
player_values_with_salary_graph <- ggplot(player_values_with_salary, aes(x=salary, y=deserving_salary, label = first_name, label2 = last_name)) +
  geom_point()

ggplotly(player_values_with_salary_graph)

#Hitters salaries
hitters_salaries <- player_values_with_salary %>% 
  filter(position_abbreviation=="IF" 
         | position_abbreviation=="SS" 
         | position_abbreviation=="C" 
         | position_abbreviation=="1B" 
         | position_abbreviation=="2B" 
         | position_abbreviation=="LF" 
         | position_abbreviation=="CF" 
         | position_abbreviation=="OF" 
         | position_abbreviation=="3B" 
         | position_abbreviation=="DH" 
         | position_abbreviation=="RF") %>% 
  arrange(sal_difference)

hitters_salaries_graph <- ggplot(hitters_salaries, aes(x=salary, y=deserving_salary, label=first_name, label2=last_name, label3=team_name)) +
  geom_point() + axis.text.x("")

ggplotly(hitters_salaries_graph)

#Starting pitchers salaries
starting_pitcher_salaries <- player_values_with_salary %>% 
  filter(position_abbreviation=="SP") %>% 
  arrange(sal_difference)

starting_pitcher_salaries_graph <- ggplot(starting_pitcher_salaries, aes(x=salary, y=deserving_salary, label = first_name, label2 = last_name)) +
  geom_point()

ggplotly(starting_pitcher_salaries_graph)

#Relief pitchers salaries
relief_pitcher_salaries <- player_values_with_salary %>% 
  filter(position_abbreviation=="RP") %>% 
  arrange(sal_difference)

relief_pitcher_salaries_graph <- ggplot(relief_pitcher_salaries, aes(x=salary, y=deserving_salary, label = first_name, label2 = last_name)) +
  geom_point()

ggplotly(relief_pitcher_salaries_graph)


#Closers salaries
closers_salaries <- player_values_with_salary %>% 
  filter(position_abbreviation=="CL") %>% 
  arrange(sal_difference)

closers_salaries_graph <- ggplot(closers_salaries, aes(x=salary, y=deserving_salary, label = first_name, label2 = last_name)) +
  geom_point()

ggplotly(closers_salaries_graph)

most_overpaid <- player_values_with_salary %>% 
  arrange(desc(sal_difference)) %>% 
  select(first_name, last_name, position_abbreviation, team_name, player_value, salary, deserving_salary, sal_difference) %>% 
  head(12)

most_underpaid <- player_values_with_salary %>% 
  arrange(sal_difference) %>% 
  select(first_name, last_name, position_abbreviation, team_name, player_value, salary, deserving_salary, sal_difference) %>% 
  head(12)

most_overpaid$last_name <- factor(most_overpaid$last_name, levels = most_overpaid$last_name[order(desc(most_overpaid$sal_difference))])

ggplot(most_overpaid, aes(x=last_name, y=sal_difference)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Most Overpaid Players") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Last Name") +
  ylab("Amount Overpaid By") +
  scale_y_continuous(name="Amount Overpaid By", labels = comma)

ggplot(most_overpaid, aes(x=salary, y=deserving_salary, label=last_name)) +
  geom_point() +
  geom_text(aes(label=last_name),hjust=0.5, vjust=-0.5,size=2.4) +
  scale_x_continuous(name="Salary", labels = comma) +
  scale_y_continuous(name="Deserving Salary", labels = comma) +
  theme(axis.text = element_text(size=6)) +
  ggtitle("Most Overpaid Players") +
  theme(plot.title = element_text(hjust = 0.5))

most_underpaid$sal_difference <- most_underpaid$sal_difference*-1
most_underpaid$last_name <- factor(most_underpaid$last_name, levels = most_underpaid$last_name[order(desc(most_underpaid$sal_difference))])

ggplot(most_underpaid, aes(x=last_name, y=sal_difference)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Most Underpaid Players") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Last Name") +
  ylab("Amount Underpaid By") +
  scale_y_continuous(name="Amount Underpaid By", labels = comma)

ggplot(most_underpaid, aes(x=salary, y=deserving_salary, label=last_name)) +
  geom_point() +
  geom_text(aes(label=last_name),hjust=0.5, vjust=-0.5,size=2.4) +
  scale_x_continuous(name="Salary", labels = comma) +
  scale_y_continuous(name="Deserving Salary", labels = comma) +
  theme(axis.text = element_text(size=6)) +
  ggtitle("Most Underpaid Players") +
  theme(plot.title = element_text(hjust = 0.5))

most_overpaid1 <- most_overpaid[,c(1,2,3,4,5,8)]
most_overpaid1 <- most_overpaid1 %>% 
  rename(amt_overpaid_by = sal_difference)

most_underpaid1 <- most_underpaid[,c(1,2,3,4,5,8)]
most_underpaid1 <- most_underpaid1 %>% 
  rename(amt_underpaid_by = sal_difference)
