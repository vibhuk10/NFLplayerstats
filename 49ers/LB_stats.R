#libraries
library(tidyverse)
library(purrr)
library(nflfastR)
# get PFR Defense Data
advDefense_20 <- read_csv('data-raw/advDefense_2020.csv')
advDefense_20 <- cleanPFRData(advDefense_20, 2020)

LB_20 <- 
  advDefense_20 %>% 
  filter(Pos == "ILB" | Pos == "MLB")

test <- 
  advDefense_20 %>% 
  filter(Player == "Roquan Smith" |Player == "Fred Warner" | Player == "Darius Leonard" | Player == "Bobby Wagner")
