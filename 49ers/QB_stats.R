#libraries
library(tidyverse)
library(purrr)
library(nflfastR)
# get PFR QB Data
source("./scraping/PFRScraping.R")
PFRDataNames <- 
  tibble(
    varName = c('stanPass_20', 'advPassAY_20', 'advPassAcc_20', 'advPassPressure_20'),
    urlend = c('/passing.htm', '/passing_advanced.htm', '/passing_advanced.htm', '/passing_advanced.htm'),
    startyr = c(2020, 2020, 2020, 2020),
    endyr = c(2020, 2020, 2020, 2020),
    numTable = c(1, 1, 2, 3)
  )

# for loop to scrape each data set in PFRDataNames through ScrapePFRData
for(i in 1:nrow(PFRDataNames)) {
  assign(getVar(PFRDataNames, i, 1), scrapePFRData(getVar(PFRDataNames, i, 2), getVar(PFRDataNames, i, 3), getVar(PFRDataNames, i, 4), getVar(PFRDataNames, i, 5)))
}

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

qb_cpoe_20 <-   
  fastpbp_20 %>%
  filter(!is.na(cpoe)) %>%
  group_by(passer_player_name, passer_player_id) %>%
  summarize(
    cpoe = mean(cpoe),
    Atts=n()
  ) %>%
  filter(Atts > 100) %>% 
  select(passer_player_name, passer_player_id, cpoe)

# get expected yac per play, yac per play, yards over expecred yac per play
qb_epa_20 <- 
  fastpbp_20 %>% 
  group_by(name, id, posteam) %>%
  filter(!is.na(qb_epa), !is.na(down)) %>%
  summarize(
    qb_epa = mean(qb_epa),
    Atts = sum(complete_pass + incomplete_pass)
  ) %>%
  ungroup() %>%
  filter(Atts>100) %>% 
  select(name, id, posteam, Atts, qb_epa)

qb_efficiency_20 <- 
  qb_cpoe_20 %>% 
  left_join(qb_epa_20, by = c("passer_player_id" = "id")) %>% 
  select(name, passer_player_id, posteam, Atts, cpoe, qb_epa)

chart_data <- 
  qb_efficiency_20 %>% 
  filter(name != "J.Luton") %>% 
  filter(name != "T.Hill") %>% 
  filter(name != "G.Minshew II") %>% 
  mutate(niner = ifelse(posteam == "HOU", "y", "n")) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))

chart_data %>%
  ggplot(aes(x = qb_epa, y = cpoe)) +
  geom_point(color = chart_data$team_color, aes(cex = Atts), alpha = 1 / 4) +
  ggrepel::geom_text_repel(aes(label = name, color = niner), force = 1, point.padding = 0, segment.size = 0.1, size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black"), guide = FALSE) +
  scale_size_area(max_size = 6) +
  labs(
    x = "Average EPA per play",
    y = "Completion Percentage over Expected",
    title = "Deshaun Watson CPOE vs. EPA in 2020"
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 8) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, angle = 0),
    axis.text.x = element_text(size = 10, angle = 0),
    legend.title = element_text(size = 8, hjust = 0, vjust = 0.5, face = "bold"),
    legend.position = c(0.8, 0.1),
    legend.box = "vertical",
    legend.direction = "horizontal",
    aspect.ratio = 1 / 1.618,
  ) +
  NULL
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
#                "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
#                "#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
#                "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
#                "#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
#                "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2")
# ninerQBData <- 
#   QBRankingData %>% 
#   mutate(niner = ifelse(team == "SF", "y", "n"))
# ninerQBData %>% 
#   filter(Att >= 60, name != "Jake Luton") %>% 
#   ggplot(mapping = aes(x = avgEpa, y = Rate, color = niner)) +
#   geom_point(size = 4) +
#   scale_colour_manual(values = c("y" = "red", "n" = "black")) +
#   geom_text_repel(aes(label = name),
#                    box.padding   = 0.35, 
#                    point.padding = 0.5,
#                    size = 5.5,
#                    segment.color = 'black') +
#   theme_classic() +
#   guides(color = FALSE, size = FALSE) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(x = "Average EPA per Play", y = "Passer Rating", title = "49ers QBs in 2020")
# 
#         