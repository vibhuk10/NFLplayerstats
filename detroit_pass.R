#libraries
library(tidyverse)
library(purrr)
library(nflfastR)
library(ggimage)
#get pbp data
seasons <- 2009:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
pbp <- decode_player_ids(pbp, fast = TRUE)
pbp <- 
  pbp %>%
  mutate(year = str_sub(game_id,1,4))
pbp$year <- as.numeric(pbp$year)
fastroster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")

det_def <-   
  pbp %>%
  filter(!is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(
    def_epa = -mean(epa)
  ) %>%
  select(defteam, def_epa)

det_rush <-   
  pbp %>%
  filter((play_type == "run" & qb_scramble == 0),!is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(
    rush_epa = mean(epa)
  ) %>%
  select(posteam, rush_epa)

chart_data <- 
  det_def %>% 
  left_join(det_rush, by = c("defteam" = "posteam")) %>% 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr')) %>% 
  mutate(detroit = ifelse(defteam == "DET", "y", "n"))

chart_data %>%
  ggplot(aes(x = rush_epa, y = def_epa)) +
  geom_hline(aes(yintercept = mean(def_epa)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(rush_epa, na.rm = TRUE)), color = "red", linetype = "dotted") +
  geom_point(color = chart_data$team_color, alpha = 1 / 4, size = 4) +
  ggrepel::geom_text_repel(aes(label = defteam, color = detroit), force = 1, point.padding = 0, segment.size = 0.1, size = 5) +
  scale_colour_manual(values = c("y" = "blue", "n" = "black"), guide = FALSE) +
  #geom_image(aes(image = team_logo_espn), size = .06, by = "width", asp = 1.618) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Rushing EPA per play",
    y = "Defensive EPA per play",
    title = "QB help from rushing and defense from 2009-2020",
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
