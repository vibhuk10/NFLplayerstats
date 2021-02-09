library(nflfastR)
library(tidyverse)
library(purrr)
library(ggimage)
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
  fastpbp_20$year <- as.numeric(fastpbp_20$year)
fastroster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")

rushing_20 <- 
  fastpbp_20 %>% 
  group_by(posteam,  defteam, week,) %>%
  filter(posteam == "SF") %>%
  filter(play_type == "run", !is.na(epa)) %>%
  summarize(
    rush_epa = mean(epa),
    rush_attempts = n()
  ) %>%
  ungroup() %>%
  select(posteam, defteam, week, rush_epa, rush_attempts)

chart_data <- 
  rushing_20 %>% 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

chart_data %>%
  ggplot(aes(x = week, y = rush_epa)) +
  geom_line(color = "red", size = 1) +
  geom_image(aes(image = team_logo_espn), size = .09) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Week",
    y = "Rush EPA per play",
    title = "49ers Rushing EPA per week in 2020",
    caption = "Figure: @49ersAnalytics | Data: @nflfastR."
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.text.x = element_text(size = 12, angle = 0),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL
