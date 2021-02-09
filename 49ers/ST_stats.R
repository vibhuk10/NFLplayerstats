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
  mutate(year = str_sub(game_id,1,4))
fastpbp_20$year <- as.numeric(fastpbp_20$year)
fastroster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")

ST_punt_20 <- 
  fastpbp_20 %>% 
  group_by(defteam) %>%
  filter(play_type == "punt", !is.na(epa)) %>%
  summarize(
    punt_epa = -mean(epa),
  ) %>%
  ungroup() %>%
  select(defteam, punt_epa) %>% 
  rename(team = defteam)

ST_kickoff_20 <- 
  fastpbp_20 %>% 
  group_by(posteam) %>%
  filter(play_type == "kickoff", !is.na(epa)) %>%
  summarize(
    kickoff_epa = mean(epa),
  ) %>%
  ungroup() %>%
  select(posteam, kickoff_epa) %>% 
  rename(team = posteam)

ST_20 <- 
  ST_punt_20 %>% 
  left_join(ST_kickoff_20, by = "team") %>% 
  mutate(epa = (punt_epa + kickoff_epa)/2)

chart_data <- 
  ST_20 %>% 
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

chart_data %>%
  ggplot(aes(x = punt_epa, y = kickoff_epa)) +
  #geom_point(color = chart_data$team_color, aes(cex = rec), alpha = 1 / 4) +
  #ggrepel::geom_text_repel(aes(label = receiver, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  #scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  geom_image(aes(image = team_logo_espn), size = .08) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Receiving Punt EPA per play",
    y = "Receiving Kickoff EPA per play",
    title = "49ers Special Teams EPA in 2020",
    caption = "Figure: @49ersAnalytics | Data: @nflfastR."
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.text.x = element_text(size = 12, angle = 0),
    legend.title = element_text(size = 12, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "top",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL

taybor <- 
  fastpbp_20 %>% 
  filter(posteam == "SF", play_type == "punt") %>% 
  select(posteam, defteam, week, desc, epa, year)



