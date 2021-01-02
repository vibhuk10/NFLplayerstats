library(tidyverse)
source("./scraping/PFRScraping.R")
PFRDataNames <- 
  tibble(
    varName = c('kick_punt_20'),
    urlend = c('/kicking.htm'),
    startyr = c(2017),
    endyr = c(2020),
    numTable = c(1)
  )

# for loop to scrape each data set in PFRDataNames through ScrapePFRData
for(i in 1:nrow(PFRDataNames)) {
  assign(getVar(PFRDataNames, i, 1), scrapePFRData(getVar(PFRDataNames, i, 2), getVar(PFRDataNames, i, 3), getVar(PFRDataNames, i, 4), getVar(PFRDataNames, i, 5)))
}

colnames(kick_punt_20)[8] <- "FGA_0_19"
colnames(kick_punt_20)[9] <- "FGM_0_19"
colnames(kick_punt_20)[10] <- "FGA_20_29"
colnames(kick_punt_20)[11] <- "FGM_20_29"
colnames(kick_punt_20)[12] <- "FGA_30_39"
colnames(kick_punt_20)[13] <- "FGM_30_39"
colnames(kick_punt_20)[14] <- "FGA_40_49"
colnames(kick_punt_20)[15] <- "FGM_40_49"
colnames(kick_punt_20)[16] <- "FGA_50"
colnames(kick_punt_20)[17] <- "FGM_50"
colnames(kick_punt_20)[32] <- "Punt_Lng"

colnames(kick_punt_20)

kick_punt_20 <- 
  kick_punt_20 %>% 
  mutate(Pos = ifelse(FGA>0, "k", "p"))

kick_20 <- 
  kick_punt_20 %>% 
  filter(Pos == "k") %>% 
  select(-(Pnt:`Y/P`))

kick_20[is.na(kick_20)] <- 0

kick_20 <- 
  kick_20 %>% 
  filter(FGA >= 10) %>% 
  mutate(FG_pct_under_40 = (FGM_0_19+FGM_20_29+FGM_30_39)/(FGA_0_19+FGA_20_29+FGA_30_39))

test <- 
  kick_20 %>% 
  group_by(Player)

gould <- 
  kick_20 %>% 
  filter(Player == "Robbie Gould")
library(scales)

cbPalette <- c("#b3995d", "#b3995d", "#b3995d", "#b3995d")
gould %>% 
  ggplot() +
  geom_col(aes(x=Year, y = `FG%`, fill = as.factor(Year)), width = .5) +
  guides(fill = FALSE) +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Season",
       y = "Field Goal Percentage",
       title = "Robbie Gould's Field Goal Percentage on the 49ers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = percent) + 
  theme_classic(base_size = 20) +
  geom_text(size = 6, aes(x=Year, y = `FG%` + 0.05, label = paste0(round(`FG%`, 3) * 100, "% ")))

  