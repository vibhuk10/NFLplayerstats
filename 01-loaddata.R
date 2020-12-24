library(tidyverse)

#NFLFastR Data
fastroster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")
fastpbp_20 <- read_csv("data-raw/NFLfastR_pbp_2020.csv")

library(purrr)
library(nflfastR)
fastpbp_20 <- read_csv("data-raw/NFLfastR_pbp_2020.csv")
fastpbp_20 <- decode_player_ids(fastpbp_20, fast = FALSE) 
fastpbp_20 %>% write_csv("data-raw/NFLfastR_pbp_2020.csv")

# main
pbp_19 <- read_csv("data-raw/NFL_pbp_2019.csv")
full_roster_19 <- read_csv("data-raw/NFL_full_roster_2019.csv")


#skill
roster_19 <-  read_csv("data-raw/NFL_roster_2019.csv")

#player stats
stats_skill_19 <- read_csv("data-clean/NFL_player_stats_skill_2019.csv")

skill_roster_19 <- read_csv("data-clean/NFL_player_stats_skill_roster_2019.csv")


ros_19 <- read_csv("data-raw/NFL_combined_roster_2019.csv")

pbp_full <- read_csv("data-raw/NFL_pbp_2009-2019.csv")
full_roster <- reac_csv("data-raw/NFL_full_roster_1999_2019.csv")

pbp_19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")
pbp_19 %>% write_csv("data-raw/NFL_pbp_2019.csv")

roster_19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2019.csv")
roster_19 %>% write_csv("data-raw/NFL_roster_2019.csv")

pbp_19_2 <- read_rds("data-raw/play_by_play_2019.rds")

full_roster <- read_csv("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.csv")
full_roster %>% write_csv("data-raw/NFL_full_roster_1999_2019.csv")

final %>% write_csv("data-clean/NFL_player_stats_skill_2019.csv")

full_roster_19 <- 
  full_roster %>% 
  filter(team.season == 2019) %>% 
  select(team.season, teamPlayers.displayName, teamPlayers.firstName, teamPlayers.middleName, teamPlayers.lastName,     
        teamPlayers.suffix, teamPlayers.status, teamPlayers.positionGroup, teamPlayers.position,teamPlayers.nflId,  
        teamPlayers.esbId, teamPlayers.gsisId, teamPlayers.birthDate, teamPlayers.homeTown, teamPlayers.collegeId,   
        teamPlayers.collegeName, teamPlayers.jerseyNumber, teamPlayers.height, teamPlayers.weight, team.teamId,        
        team.abbr, team.cityState,team.fullName, team.nick, team.conferenceAbbr,
        team.divisionAbbr, teamPlayers.headshot_url, teamPlayers.profile_url )

names(full_roster_19)

full_roster_19 %>% write_csv("data-raw/NFL_full_roster_2019.csv")

rosters <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/rosters.csv")
roster_2019 <- 
  rosters %>% 
  filter(season == 2019) %>% 
  select(season:position, years)

install.packages("editData")
devtools::install_github("cardiomoon/editData")

ros_19 <- 
  full_roster_19 %>% 
  inner_join(roster_2019, by = "name")
ros_19 %>% write_csv("data-raw/NFL_combined_roster_2019.csv")

install.packages("nflfastR")
library(nflfastR)

