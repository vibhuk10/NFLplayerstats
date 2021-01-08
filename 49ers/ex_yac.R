#libraries
library(tidyverse)
library(purrr)
library(nflfastR)
# get PFR QB Data
advRecv_20 <- read_csv('data-raw/advRecv_2020.csv')
advRecv_20 <- cleanPFRData(advRecv_20, 2020)

#get pbp data
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

fastpbp_20 %>% filter(!is.na(cpoe)) %>% group_by(passer_player_name) %>%
  summarize(cpoe = mean(cpoe), Atts=n()) %>%
  filter(Atts > 200) %>%
  arrange(-cpoe) %>%
  head(5) %>% 
  knitr::kable(digits = 1)

# get expected yac per play, yac per play, yards over expecred yac per play
yac_over_ex_20 <- 
  fastpbp_20 %>% 
  group_by(receiver, receiver_id, posteam) %>%
  mutate(tgt = sum(complete_pass + incomplete_pass)) %>%
  filter(posteam == "SF") %>%
  filter(complete_pass == 1, air_yards < yardline_100, !is.na(xyac_mean_yardage)) %>%
  summarize(
    yac_oe = mean(yards_after_catch - xyac_mean_yardage),
    actual_yac = mean(yards_after_catch),
    expected_yac = mean(xyac_mean_yardage),
    rec = n()
  ) %>%
  ungroup() %>%
  filter(rec > 4) %>% 
  select(receiver, receiver_id, posteam, actual_yac, expected_yac, yac_oe, rec) %>%
  arrange(-yac_oe)

yac_20 <- 
  fastpbp_20 %>% 
  group_by(receiver, receiver_id, posteam) %>%
  mutate(tgt = sum(complete_pass + incomplete_pass)) %>%
  filter(complete_pass == 1, air_yards < yardline_100, !is.na(xyac_mean_yardage)) %>%
  summarize(
    yac_oe = mean(yards_after_catch - xyac_mean_yardage),
    actual_yac = mean(yards_after_catch),
    expected_yac = mean(xyac_mean_yardage),
    rec = n()
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  left_join(fastroster_20, by = c("receiver_id" = "gsis_id")) %>% 
  filter(position == "WR" | position == "TE") %>% 
  filter(rec>30) %>% 
  select(receiver, position, full_name, receiver_id, posteam, actual_yac, expected_yac, yac_oe, rec) %>%
  arrange(-yac_oe) %>% 
  left_join(advRecv_20, by = c("full_name" = "Player")) %>% 
  select(receiver:rec, `ADOT?`) %>% 
  mutate(niner = ifelse(posteam == "SF", "y", "n")) %>% 
  na.omit()

chart_data <- 
  yac_20 %>% 
  filter(rec > 45 | niner == "y") %>% 
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))

chart_data %>%
  ggplot(aes(x = actual_yac, y = `ADOT?`)) +
  geom_point(color = chart_data$team_color, aes(cex = rec), alpha = 1 / 4) +
  ggrepel::geom_text_repel(aes(label = receiver, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Average YAC per play",
    y = "Average Depth of Target in Yards",
    title = "Deebo YAC Analysis in 2020"
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, angle = 0),
    axis.text.x = element_text(size = 10, angle = 0),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL

  