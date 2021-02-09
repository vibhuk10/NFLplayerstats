library(nflfastR)
library(tidyverse)
library(purrr)
library(ggimage)
library(ggtext)
library(gt)
NGSrecv <-  nflfastR:::load_ngs(2020, 'receiving')
test <- 
  NGSrecv %>% 
  filter(player_display_name == "Deebo Samuel")
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
advRecv_20 <- read_csv("data-raw/advRecv_2020.csv")
NGSrecv_20 <- NGSrecv %>% filter(week == 0)

chart_data <- 
  NGSrecv_20  %>% 
  left_join(teams_colors_logos, by = c('team_abbr' = 'team_abbr')) %>% 
  mutate(niner = ifelse(team_abbr == "SF", "y", "n")) %>% 
  filter(receptions > 47 | player_display_name == "Deebo Samuel")

chart_data %>%
  ggplot(aes(x = percent_share_of_intended_air_yards, y = avg_yac_above_expectation)) +
  geom_hline(aes(yintercept = mean(avg_yac_above_expectation)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(percent_share_of_intended_air_yards)), color = "red", linetype = "dotted") +
  geom_point(color = chart_data$team_color, size = 5, alpha = 1/4) +
  ggrepel::geom_text_repel(aes(label = player_display_name, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  #geom_image(aes(image = headshot_url), size = .05, by = "width", asp = 1.618) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Average Yac Above Expectation",
    y = "Percent Share of Intended Air Yards",
    title = "Deebo Receiving Comparison 2020",
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

fast_pass_wr3 <- 
  fastpbp_20 %>% 
  group_by(receiver, receiver_id, posteam) %>%
  mutate(tgt = sum(complete_pass + incomplete_pass)) %>%
  filter(!is.na(epa)) %>%
  summarize(
    recv_epa = mean(epa),
    tgt = sum(complete_pass + incomplete_pass),
    rec =  sum(complete_pass)
  ) %>%
  ungroup() %>%
  select(receiver, receiver_id, posteam, recv_epa, tgt, rec) %>%
  filter(!(receiver == "D.Moore" & posteam == "CAR"))

fast_run_wr3 <- 
  fastpbp_20 %>% 
  group_by(rusher, rusher_id, posteam) %>%
  filter(!is.na(epa), penalty != 1) %>%
  summarize(
    rush_epa = mean(epa),
    carries = n()
  ) %>%
  ungroup() %>%
  filter(!(rusher == "D.Moore" & posteam == "CAR")) %>% 
  select(rusher, rusher_id, rush_epa, carries)

wr3 <- 
  tibble(
    name = c("K.Bourne", "C.Samuel", "D.Moore", "M.Hardman", "J.Guyton", "R.Gage", "J.Reynolds", "T.Smith"),
    ypg = c(44.5, 56.7, 26.1, 35.0, 31.9, 49.1, 38.6, 32.0),
    drop_pct = c(8.1, 5.2, 6.4, 12.9, 9.1, 6.4, 3.7, 8.0)
  )

wr3 <- 
  wr3 %>% 
  left_join(fast_pass_wr3, by = c("name" = "receiver")) %>% 
  left_join(fast_run_wr3, by = c("name" = "rusher"))

wr3 <- 
  wr3 %>% 
  mutate(epa = ifelse(is.na(rush_epa) == TRUE, recv_epa, (rush_epa+recv_epa)/2))

wr3NGSrecv_20 <- 
  NGSrecv_20 %>% 
  select(avg_intended_air_yards, avg_yac_above_expectation, player_gsis_id)
wr3 <- 
  wr3 %>% 
  left_join(wr3NGSrecv_20, by = c("receiver_id" = "player_gsis_id"))

wr3 <- 
  wr3 %>% 
  mutate(epa = round(epa, digits = 3),
         avg_intended_air_yards = round(avg_intended_air_yards, digits = 2),
         avg_yac_above_expectation = round(avg_yac_above_expectation, digits = 3))

wr3$carries[is.na(wr3$carries)] <- 0
wr3 %>%
  select(name, posteam, tgt, rec, carries, ypg, epa, avg_intended_air_yards, avg_yac_above_expectation) %>% 
  gt(rowname_col = "name") %>%
  tab_header(
    title = "Kendrick Bourne vs other WR3s"
  ) %>%
  tab_source_note(
    source_note = "Figure: @49ersAnalytics, Data: @nflfastR, @NextGenStats"
  ) %>% 
  tab_stubhead(label = "Name") %>% 
  cols_label(
    posteam = html("Team"),
    tgt = html("Targets"),
    rec = html("Receptions"),
    carries = html("Carries"),
    ypg = html("Yards per game"),
    epa = html("EPA per play"),
    avg_intended_air_yards = html("ADOT"),
    avg_yac_above_expectation = html("YAC over expected"),
  )
