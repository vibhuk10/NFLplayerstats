# passingDataTest <- 
#   fastpbp_20 %>% 
#   filter(passer_player_id == "00-0035232",
#          sack == 0)
# names(fastpbp_20)

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
    ~name, ~id, ~position, ~team,
    ~total_yards, ~total_touchowns,
    ~rushing_yards, ~rushing_touchdowns,
    ~passing_yards, ~passing_touchdowns, ~air_yards, ~interceptions,
    ~attempts, ~ypa, ~completions, ~cmp_percentage, ~passer_rating,
    ~sacks, ~sack_rate, ~qb_hits, ~qb_hit_rate,
    ~qb_scrambles, ~fumbles
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
  
  # rushing
  rushing_yards <- 0
  rushing_tds <- 0
  qb_scrambles <- 0
  
  rushingData <- 
    fastpbp_20 %>% 
      filter(rusher_player_id == idTemp,
             play_type != "qb_kneel")
  
  for (a in 1:nrow(rushingData)) {
    rushing_yards <- add_stats(rushing_yards, 30, a, rushingData)
    rushing_tds <- add_stats(rushing_tds, 153, a, rushingData)
    qb_scrambles <- add_stats(qb_scrambles, 36, a, rushingData)
  }
  
  # sack and pressures
  sacks <- 0
  qb_hits <- 0
  
  sackData <- 
    fastpbp_20 %>% 
    filter(passer_player_id == idTemp)
  
  for (b in 1:nrow(sackData)) {
    sacks <- add_stats(sacks, 150, b, sackData)
    qb_hits <- add_stats(qb_hits, 147, b, sackData)
  }
  
  # sack rate
  sack_rate <- round(sacks/nrow(sackData), digits = 3)
  
  # QB hit rate
  qb_hit_rate <- round(qb_hits/nrow(sackData), digits = 3)
  
  # passing
  passing_yards <- 0
  passing_tds <- 0
  air_yards <- 0
  attempts <- 0
  completions <- 0
  interceptions <- 0
  
  passingData <- 
    fastpbp_20 %>% 
    filter(passer_player_id == idTemp,
           sack == 0)
  
  for (c in 1:nrow(passingData)) {
    passing_yards <- add_stats(passing_yards, 30, c, passingData)
    passing_tds <- add_stats(passing_tds, 152, c, passingData)
    air_yards <- add_stats(air_yards, 39, c, passingData)
    attempts <- attempts + 1
    completions <- add_stats(completions, 161, c, passingData)
    interceptions <- add_stats(interceptions, 126, c, passingData)
  }
  
  # completion percentage
  cmp_percentage <- round(completions/attempts, digits = 3)
  
  # yards per attempt
  ypa <- round(passing_yards/attempts, digits=1)
  
  # passer rating
  passer_rating <- 
    round(
    ((((completions/attempts-.3)*5) + 
    ((passing_yards/attempts-3)*.25) +
    ((passing_tds/attempts)*20) +
    (2.375-(interceptions/attempts*25)))/6)*100,
    digits = 1)
  
  # fumbles
  fumbles <- 0
  
  fumbleData <- 
    fastpbp_20 %>% 
    filter(fumbled_1_player_id == idTemp)
  
  for (d in 1:nrow(fumbleData)) {
    fumbles <- fumbles + 1
  }
  
  
  # totals
  total_touchdowns <- na_to_zero(rushing_tds) + na_to_zero(passing_tds)
  total_yards <-  na_to_zero(rushing_yards) + na_to_zero(passing_yards)
  
  tempQBStats <- 
    tibble(
      name = c(nameTemp),
      id = c(idTemp),
      position = c(positionTemp),
      team = c(teamTemp),
      total_yards = c(total_yards),
      total_touchdowns = c(total_touchdowns),
      rushing_yards = c(rushing_yards),
      rushing_touchdowns = c(rushing_tds),
      passing_yards = c(passing_yards),
      passing_touchdowns = c(passing_tds),
      air_yards = c(air_yards),
      interceptions = c(interceptions),
      attempts = c(attempts),
      ypa = c(ypa),
      completions = c(completions),
      cmp_percentage = c(cmp_percentage),
      passer_rating = c(passer_rating),
      sacks = c(sacks),
      sack_rate = c(sack_rate),
      qb_hits = c(qb_hits),
      qb_hit_rate = c(qb_hit_rate),
      qb_scrambles = c(qb_scrambles),
      fumbles = c(fumbles)
    )
  
  QBStats <- 
    rbind(QBStats, tempQBStats)
  
}

QBStats[is.na(QBStats)] <- 0
