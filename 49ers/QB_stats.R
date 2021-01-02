library(ggrepel)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
               "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
               "#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
               "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
               "#999999", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2", "#D55E00", "#CC79A7",
               "#000000", "#E69F00", "#56B4E9", "#009E73", "#00AFBB", "#0072B2")
ninerQBData <- 
  QBRankingData %>% 
  mutate(niner = ifelse(team == "SF", "y", "n"))
ninerQBData %>% 
  filter(Att >= 60, name != "Jake Luton") %>% 
  ggplot(mapping = aes(x = avgEpa, y = Rate, color = niner)) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("y" = "red", "n" = "black")) +
  geom_text_repel(aes(label = name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   size = 5.5,
                   segment.color = 'black') +
  theme_classic() +
  guides(color = FALSE, size = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Average EPA per Play", y = "Passer Rating", title = "49ers QBs in 2020")

        