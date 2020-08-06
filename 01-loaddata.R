library(tidyverse)

pbp_19 <- read_csv("data-raw/NFL_pbp_2019.csv")
full_roster_19 <- read_csv("data-raw/NFL_full_roster_2019.csv")

roster_19 <-  read_csv("data-raw/NFL_roster_2019.csv")

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

full_roster_19 <- 
  full_roster %>% 
  filter(team.season == 2019)
full_roster_19 %>% write_csv("data-raw/NFL_full_roster_2019.csv")

library(nflscrapR)

install.packages("devtools")
devtools::install_github(repo = "maksimhorowitz/nflscrapR")
devtools::install_github(repo = "ryurko/nflscrapR")

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite", type = "source")

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


