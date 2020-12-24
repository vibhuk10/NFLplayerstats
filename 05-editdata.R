stats_skill_19 <- read_csv("data-clean/NFL_player_stats_skill_2019.csv")

convertName <- function(name) {
  name <- str_trim(name)
  name <-  tolower(name)
  first <-
    name %>% 
    str_sub(0, str_locate(name," ")[1, 1]-1)
  last <- 
    name %>% 
    str_sub(str_locate(name, " ")[1, 1] + 1)
  webName <- paste0(first,"-",last)
  webName
}

convertName(stats_skill_19[2,1])

final <- 
  tribble(
    ~id, ~webName
  )

for (i in 1:nrow(stats_skill_19)) {
  nameTemp <- stats_skill_19[i,1]
  nameTemp <- nameTemp[[1]]
  idTemp <- stats_skill_19[i,2]
  idTemp <- idTemp[[1]]
  webNameTemp <- convertName(nameTemp)
  temp <- 
    tibble(
      id = c(idTemp),
      webName = c(webNameTemp)
    )
  
  final <- 
    rbind(final, temp)
}

final2 <- 
  stats_skill_19 %>% 
  left_join(final, by = "id")

temp <- 
  full_roster_19 %>% 
  select(teamPlayers.gsisId, teamPlayers.headshot_url) %>% 
  rename(id = teamPlayers.gsisId, headshot = teamPlayers.headshot_url)

final2 <- 
  stats_skill_19 %>% 
  left_join(temp, by = "id")

final2[is.na(final2)] <- 0

final2 <- 
  full_roster_19 %>% 
  select(teamPlayers.gsisId, teamPlayers.lastName) %>% 
  rename(id = teamPlayers.gsisId, lastName = teamPlayers.lastName)

final3 <- 
  stats_skill_19 %>% 
  left_join(final2, by = "id")

final2 <- 
  full_roster_19 %>% 
  select(teamPlayers.gsisId, teamPlayers.birthDate, teamPlayers.collegeName, teamPlayers.jerseyNumber, teamPlayers.height, teamPlayers.weight, team.fullName) %>% 
  rename(id = teamPlayers.gsisId, birthDate= teamPlayers.birthDate, collegeNmae = teamPlayers.collegeName, jerseyNumber = teamPlayers.jerseyNumber, height = teamPlayers.height, weight = teamPlayers.weight, fullName = team.fullName)

final3 <- 
  stats_skill_19 %>% 
  left_join(final2, by = "id")
  

final3 %>% write_csv("data-clean/NFL_player_stats_skill_2019.csv")

skill_roster_19 <-
  stats_skill_19 %>% 
  select(name, position, fullName, webName, headshot, lastName)

skill_roster_19 %>% write_csv("data-clean/NFL_player_stats_skill_roster_2019.csv")
