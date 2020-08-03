library(tidyverse)

pbp_full <- read_csv("data-raw/NFL_pbp_2009-2019.csv")

pbp_19 <-  read_csv("data-raw/NFL_pbp_2019.csv")
roster_19 <-  read_csv("data-raw/NFL_roster_2019.csv")

pbp_19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")
pbp_19 %>% write_csv("data-raw/NFL_pbp_2019.csv")

roster_19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2019.csv")
roster_19 %>% write_csv("data-raw/NFL_roster_2019.csv")


