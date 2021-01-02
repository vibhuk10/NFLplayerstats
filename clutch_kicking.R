library(tidyverse)
library(nflfastR)
library(purrr)

seasons <- 2017:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
pbp<- decode_player_ids(pbp, fast = TRUE)
pbp <- 
  pbp %>%
  mutate(year = str_sub(game_id,1,4))
pbp$year <- as.numeric(pbp$year)

gould <- 
  pbp %>% 
  filter(kicker_player_name == "R.Gould")

clutch_gould <- 
  gould %>% 
  filter(play_type == "field_goal") %>% 
  filter((quarter_seconds_remaining <= 200 & qtr == 4) | (qtr == 5))

clutch_gould <- 
  count(field_goal_result)
