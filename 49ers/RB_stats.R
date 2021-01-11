library(nflfastR)
library(tidyverse)
library(purrr)
#get pbp data
seasons <- 2019:2020
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

rbs_19_20 <- 
  fastpbp_20 %>% 
  group_by(rusher, rusher_id, posteam, year) %>%
  filter(posteam == "SF") %>%
  filter(play_type == "run", !is.na(epa)) %>%
  summarize(
    rush_epa = mean(epa),
    ypc = mean(yards_gained),
    carries = n()
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  select(rusher, rusher_id, posteam, rush_epa, ypc, carries, year) %>% 
  left_join(fastroster_20, by = c("rusher_id" = "gsis_id")) %>% 
  filter(position == "RB") %>% 
  filter(!is.na(rusher), carries > 20)

chart_data <- 
  rbs_19_20 %>% 
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  mutate(year = as.character(year))

chart_data %>%
  ggplot(aes(x = rush_epa, y = ypc)) +
  geom_point(aes(cex = carries), alpha = 1 / 4) +
  ggrepel::geom_text_repel(aes(label = rusher, color = year), force = 1, point.padding = 0, segment.size = 0.1, size = 5.5) +
  scale_colour_manual(values = c("2020" = "#AA0000", "2019" = "#B3995D")) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Rush EPA per play",
    y = "Yards Per Carry",
    title = "49ers RBs in 2019 vs. 2020"
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(size = 12,face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, angle = 0),
    axis.text.x = element_text(size = 10, angle = 0),
    legend.title = element_text(size = 12, hjust = 0, vjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
    legend.position = c(0.8, 0.15),
    legend.box = "vertical",
    legend.direction = "horizontal",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL
