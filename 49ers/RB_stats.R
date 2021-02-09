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
rbs_19_20 <-   
  rbs_19_20 %>% 
  mutate(full_name = ifelse(full_name == "Jeffery Wilson", "Jeff Wilson", full_name))

rbs2_19_20 <- 
  fastpbp_20 %>% 
  group_by(rusher, rusher_id, posteam, year) %>%
  filter(posteam == "SF") %>%
  filter(play_type == "run", (down == 1 | down == 2), !is.na(epa)) %>%
  summarize(
    early_rush_epa = mean(epa),
    early_ypc = mean(yards_gained),
    early_carries = n()
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  select(rusher, rusher_id, posteam, early_rush_epa, early_ypc, early_carries, year) %>% 
  left_join(fastroster_20, by = c("rusher_id" = "gsis_id")) %>% 
  filter(position == "RB") %>% 
  filter(!is.na(rusher), early_carries > 20)
rbs2_19_20 <-   
  rbs2_19_20 %>% 
  mutate(full_name = ifelse(full_name == "Jeffery Wilson", "Jeff Wilson", full_name))

rbs3_19_20 <- 
  fastpbp_20 %>% 
  group_by(rusher, rusher_id, posteam, year) %>%
  filter(posteam == "SF") %>%
  filter(play_type == "run", (down == 3 | down == 4), !is.na(epa)) %>%
  summarize(
    late_rush_epa = mean(epa),
    late_ypc = mean(yards_gained),
    late_carries = n()
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  select(rusher, rusher_id, posteam, late_rush_epa, late_ypc, late_carries, year) %>% 
  left_join(fastroster_20, by = c("rusher_id" = "gsis_id")) %>% 
  filter(position == "RB") %>% 
  filter(!is.na(rusher), late_carries > 20)
rbs3_19_20 <-   
  rbs3_19_20 %>% 
  mutate(full_name = ifelse(full_name == "Jeffery Wilson", "Jeff Wilson", full_name))

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

# get PFR RB Data
source("./scraping/PFRScraping.R")
advRush_19 <- read_csv('data-raw/advRush_2019.csv') %>% cleanPFRData(2019)
advRush_20 <- read_csv('data-raw/advRush_2020.csv') %>% cleanPFRData(2020)
advRush_19_20 <- rbind(advRush_19, advRush_20)

rbs2_19_20 <- 
  advRush_19_20 %>% 
  filter(Tm == "SFO")

rb_data <- 
  rbs_19_20 %>% 
  left_join(rbs2_19_20, by = c("full_name" = "Player", "year" = "Year")) %>% 
  select(-(season:status), -(first_name:update_dt))

rbs_rush_20 <- 
  fastpbp_20 %>% 
  filter(year == 2020) %>% 
  group_by(rusher_player_name, rusher_player_id, posteam, week) %>%
  #filter(posteam == "SF") %>%
  filter(play_type == "run", !is.na(epa)) %>%
  summarize(
    rush_epa = mean(epa),
    atts = n(),
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  select(rusher_player_name, rusher_player_id, posteam, week, rush_epa, atts) %>% 
  left_join(fastroster_20, by = c("rusher_player_id" = "gsis_id")) %>% 
  filter(position == "RB")
  # filter(!is.na(rusher), carries > 20)
  

rbs_pass_20 <- 
  fastpbp_20 %>% 
  filter(year == 2020) %>% 
  filter(play_type == "pass", !is.na(epa)) %>%
  group_by(receiver_player_name, receiver_player_id) %>%
  #filter(posteam == "SF") %>%
  summarize(
    recv_epa = mean(epa),
    tgts = n(),
    recs = sum(complete_pass)
  ) %>%
  ungroup() %>%
  #filter(rec > 4) %>% 
  select(receiver_player_name, receiver_player_id, recv_epa, tgts, recs)
  #left_join(fastroster_20, by = c("rusher_id" = "gsis_id")) %>% 
  #filter(position == "RB") %>% 
 # filter(!is.na(rusher), carries > 20)

rbs_20 <- 
  rbs_rush_20 %>% 
  left_join(rbs_pass_20, by = c("rusher_player_name" = "receiver_player_name", "rusher_player_id" = "receiver_player_id")) %>% 
  left_join(fastroster_20, by = c("rusher_player_id" = "gsis_id")) %>% 
  filter(position == "RB") %>% 
  filter(!is.na(rusher_player_name), atts > 100)

rbs_20 <-   
  rbs_20 %>% 
  mutate(full_name = ifelse(full_name == "Jeffery Wilson", "Jeff Wilson", full_name))

chart_data <- 
  rbs_20 %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) %>% 
  mutate(niner = ifelse(rusher_player_name == "J.Wilson", "y", "n")) %>% 
  filter(rusher_player_name != "B.Snell") %>% 
  filter(rusher_player_name != "D.Harris") %>% 
  filter(recs > 12)

chart_data %>%
  ggplot(aes(x = rush_epa, y = recv_epa)) +
  geom_hline(aes(yintercept = mean(recv_epa)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(rush_epa)), color = "red", linetype = "dotted") +
  geom_point(color = chart_data$team_color, size = 5, alpha = 1/4) +
  ggrepel::geom_text_repel(aes(label = rusher_player_name, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  #geom_image(aes(image = headshot_url), size = .05, by = "width", asp = 1.618) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Rushing EPA",
    y = "Receiving EPA",
    title = "Jeff Wilson in 2020",
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

