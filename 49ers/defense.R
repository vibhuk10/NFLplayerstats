library(tidyverse)
library(purrr)
library(nflfastR)

seasons <- 2020:2020
fastpbp_20 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
fastpbp_20 <- decode_player_ids(fastpbp_20, fast = TRUE)
fastpbp_20 <- 
  fastpbp_20 %>%
  mutate(year = str_sub(game_id,1,4))
fastpbp_20$year <- as.numeric(fastpbp_20$year)
fastroster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")

defense_20 <- 
  fastpbp_20 %>% 
  group_by(defteam) %>%
  filter(!is.na(epa)) %>%
  summarize(
    epa = -mean(epa),
  ) %>%
  ungroup() %>%
  select(defteam, epa)

rushing_20 <- 
  fastpbp_20 %>% 
  group_by(posteam) %>%
  filter(!is.na(epa), !is.na(down), (play_type == "run" & qb_scramble == 0)) %>%
  summarize(
    epa = mean(epa),
  ) %>%
  ungroup() %>%
  select(posteam, epa)