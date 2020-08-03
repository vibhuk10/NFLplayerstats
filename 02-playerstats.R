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


final <- 
  tribble(
    ~name, ~id, ~position, ~team,
    ~total_yards, ~total_touchowns,
    ~receiving_yards, ~yards_after_catch, ~receiving_touchdowns,
    ~rushing_yards, ~rushing_touchdowns,
    ~passing_yards, ~passing_touchdowns, ~air_yards, ~qb_scrambles,
    ~fumbles
  )
for (i in 1:nrow(roster_19)) {            
  nameTemp <- get_var(i, 3, roster_19)
  idTemp <- get_var(i, 7, roster_19)
  positionTemp <- get_var(i, 6, roster_19)
  teamTemp <- get_var(i, 5, roster_19)
  
  # receiving
  receiving_yards <- 0
  yards_after_catch <-  0
  receiving_tds <- 0
  
  receivingData <- 
    pbp_19 %>% 
    filter(receiver_player_id == idTemp)
  
  for (a in 1:nrow(receivingData)) {
    receiving_yards <- add_stats(receiving_yards, 27, a, receivingData)
    yards_after_catch <- add_stats(yards_after_catch, 37, a, receivingData)
    receiving_tds <- add_stats(receiving_tds, 146, a, receivingData)
  }
  
  # rushing
  rushing_yards <- 0
  rushing_tds <- 0
  qb_scrambles <- 0
  
  rushingData <- 
    pbp_19 %>% 
    filter(rusher_player_id == idTemp)
  
  for (b in 1:nrow(rushingData)) {
    rushing_yards <- add_stats(rushing_yards, 27, b, rushingData)
    rushing_tds <- add_stats(rushing_tds, 146, b, rushingData)
    qb_scrambles <- add_stats(qb_scrambles, 33, b, rushingData)
  }
  
  # passing
  passing_yards <- 0
  passing_tds <- 0
  air_yards <- 0
  
  passingData <- 
    pbp_19 %>% 
    filter(passer_player_id == idTemp)
  
  for (c in 1:nrow(passingData)) {
    passing_yards <- add_stats(passing_yards, 27, c, passingData)
    passing_tds <- add_stats(passing_tds, 146, c, passingData)
    air_yards <- add_stats(air_yards, 36, c, passingData)
  }
  
  # fumbles
  fumbles <- 0
  
  fumbleData <- 
    pbp_19 %>% 
    filter(fumbled_1_player_id == idTemp)
  
  for (d in 1:nrow(fumbleData)) {
    fumbles <- fumbles + 1
  }
  
  
  # totals
  total_touchdowns <- na_to_zero(receiving_tds) + na_to_zero(rushing_tds) + na_to_zero(passing_tds)
  total_yards <-  na_to_zero(receiving_yards) +  na_to_zero(rushing_yards) + na_to_zero(passing_yards)
  
  temp <- 
    tibble(
      name = c(nameTemp),
      id = c(idTemp),
      position = c(positionTemp),
      team = c(teamTemp),
      total_yards = c(total_yards),
      total_touchdowns = c(total_touchdowns),
      receiving_yards = c(receiving_yards),
      yards_after_catch = c(yards_after_catch),
      receiving_touchdowns = c(receiving_tds),
      rushing_yards = c(rushing_yards),
      rushing_touchdowns = c(rushing_tds),
      passing_yards = c(passing_yards),
      passing_touchdowns = c(passing_tds),
      air_yards = c(air_yards),
      qb_scrambles = c(qb_scrambles),
      fumbles = c(fumbles)
    )
  
  final <- 
    rbind(final, temp)
  
}

