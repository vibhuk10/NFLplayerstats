library(nflfastR)
library(tidyverse)
library(purrr)
library(ggimage)
library(ggtext)
PFFrecv_20 <-  nflfastR:::load_ngs(2020, 'receiving')
PFFrush_20 <- nflfastR:::load_ngs(2020, 'rushing')
PFFpass_20 <- nflfastR:::load_ngs(2020, 'passing')
seasons <- 2020:2020
fastpbp_20 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
fastpbp_20 <- decode_player_ids(fastpbp_20, fast = TRUE)
roster_20 <- read_csv("https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_2020.csv")

totPFFrush_20 <- 
  PFFrush_20 %>% 
  filter(week == 0) %>% 
  left_join(roster_20, by = c("player_gsis_id" = "gsis_id")) %>%  
  select((season_type:player_short_name), headshot_url)

PFF49ers_20 <- 
  PFFrush_20 %>% 
  filter(team_abbr == "SF" & week != 0) %>% 
  left_join(roster_20, by = c("player_gsis_id" = "gsis_id"))  %>% 
  select((season_type:player_short_name), headshot_url)

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
  select(posteam, defteam, week)

chart_data2 <- 
  PFF49ers_20 %>% 
  left_join(rushing_20, by = "week") %>% 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

chart_data2 %>%
  ggplot(aes(x = week, y = percent_attempts_gte_eight_defenders)) +
  geom_image(aes(image = headshot_url), size = .06, by = "width", asp = 1.618) +
  scale_x_continuous(breaks = round(seq(min(chart_data2$week), max(chart_data2$week), by = 1),1)) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Week",
    y = "% of Attempts vs. 8+ Defenders in the Box",
    title = "49ers Stacked Boxes each Week in 2020",
    caption = "Figure: @49ersAnalytics | Data: Next Gen Stats."
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

chart_data <- 
  totPFFrush_20 %>% 
  left_join(teams_colors_logos, by = c('team_abbr' = 'team_abbr')) %>% 
  mutate(niner = ifelse(team_abbr == "SF", "y", "n")) %>% 
  filter(rush_attempts>100)

chart_data %>%
  ggplot(aes(x = rush_yards_over_expected_per_att, y = percent_attempts_gte_eight_defenders)) +
  geom_hline(aes(yintercept = mean(percent_attempts_gte_eight_defenders)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(rush_yards_over_expected_per_att)), color = "red", linetype = "dotted") +
  geom_point(color = chart_data$team_color, aes(cex = rush_attempts), alpha = 1 / 4) +
  ggrepel::geom_text_repel(aes(label = player_short_name, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  #geom_image(aes(image = headshot_url), size = .05, by = "width", asp = 1.618) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Rushing Yards Over Expected Per Attempt",
    y = "% of Attempts vs. 8+ Defenders in the Box",
    title = "49ers NGS rushing in 2020",
    caption = "Figure: @49ersAnalytics | Data: Next Gen Stats."
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.text.x = element_text(size = 12, angle = 0),
    legend.title = element_text(size = 12, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = "none",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL


