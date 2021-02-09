library(tidyverse) # all the things
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)    # helper functions from ggplot2
souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

pass_map_df <- 
  read_csv(souce_url) %>% 
  rename(x_coord = x, y_coord = y)

hex_plot <- 
  pass_map_df %>%
  filter(name == "Jimmy Garoppolo") %>% 
  ggplot(aes(x = x_coord, y = y_coord)) +
  geom_hex(
    binwidth = c(1, 1)
  ) +
  scale_fill_gradient(low = "red", high = "yellow") +
  geom_hline(yintercept = c(2, 7), color = "grey") +
  scale_y_continuous(breaks = seq(-10, 60, 5))

hex_plot