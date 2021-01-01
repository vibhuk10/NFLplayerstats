# QB ranking formula
# factors: on target %, passer rating, Adjusted net yards per pass attempt, drop%, pressure %, EPA
# formula: rating = ((on target%/a) + (passer rating/b) + (Adjusted net yards per pass attempt/c)
#                    + (drop%/d) + (pressure%/e) + (EPA/f))*x
#libraries
library(tidyverse)
library(purrr)
library(nflfastR)
# get PFR QB Data
source("./PFRScraping.R")
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
#get qb epa
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

na_to_zero <- function(x) {
  ifelse(any(is.na(x)) == TRUE, 0, x)
}
add_stats <- function(total_stat, column, row, data) {
  temp_stat <- data[row,column]
  temp_stat <- temp_stat[[1]]
  temp_stat <- na_to_zero(temp_stat)
  total_stat <- total_stat + temp_stat
  total_stat
}
get_var <- function(row, column, data) {
  x <- data[row, column]
  x <- x[[1]]
}
QBStats <- 
  tribble(
    ~name, ~id, ~position, ~team, ~avgEpa, ~avgQBEpa
  )
fastroster_20_QB <- 
  fastroster_20 %>% 
  filter(position == "QB",
         !is.na(team),
         !is.na(gsis_id))
for (i in 1:nrow(fastroster_20_QB)) {
  print(paste0(i,"/",nrow(fastroster_20_QB)))
  nameTemp <- get_var(i, 7, fastroster_20_QB)
  idTemp <- get_var(i, 15, fastroster_20_QB)
  positionTemp <- get_var(i, 3, fastroster_20_QB)
  teamTemp <- get_var(i, 2, fastroster_20_QB)
  
  total_epa <- 0
  total_qbepa <- 0
  plays <- 0
  passingData <- 
    fastpbp_20 %>% 
    filter(passer_player_id == idTemp)
  
  for (a in 1:nrow(passingData)) {
    total_epa <- add_stats(total_epa, 72, a, passingData)
    total_qbepa <- add_stats(total_qbepa, 335, a, passingData)
    plays <- plays + 1
  }
  avgEpa <- total_epa/plays
  avgQBEpa <- total_epa/plays
 
  tempQBStats <- 
    tibble(
      name = c(nameTemp),
      id = c(idTemp),
      position = c(positionTemp),
      team = c(teamTemp),
      avgEpa = c(avgEpa),
      avgQBEpa = c(avgQBEpa)
    )
  QBStats <- 
    rbind(QBStats, tempQBStats)
}
QBStats[is.na(QBStats)] <- 0
QBStats %>% count(name)

QBRankingData <- left_join(QBStats, 
                           stanPass_20 %>% 
                             select(Player, Age, Att, Rate, `ANY/A`, `Sk%`) %>% 
                             rename(name = Player),
                           by = "name"
                           ) %>% 
                 left_join(advPassAcc_20 %>% 
                             select(Player, `Drop%`, `OnTgt%`) %>% 
                             rename(name = Player),
                           by = "name"
                           ) %>% 
                 left_join(advPassPressure_20 %>% 
                             select(Player, `Prss%`) %>% 
                             rename(name = Player),
                           by = "name"
                           ) %>% 
                 na.omit()
#formula: rating = ((on target%/0.5) + (passer rating/120) + (Adjusted net yards per pass attempt/10)
                   #                    + (drop%/5) + (pressure%-sack%/1) + ((EPA+2)/2))*20
QBRankings <- 
  QBRankingData %>%
  mutate(QBrating = ((`OnTgt%`/.75) + (Rate/100) + (`ANY/A`/10)
                     + (`Drop%`/2.5) + (`Prss%`-`Sk%`/1) + ((avgEpa+2)/2))*22
         ) %>%
  filter(Att >= 25) %>% 
  select(name:team, QBrating)
