roster01 <- 
  pbp_19 %>% 
  select(passer_player_id, passer_player_name) %>% 
  na.omit() %>% 
  distinct(passer_player_id, .keep_all = TRUE) %>% 
  rename(name = passer_player_name, id = passer_player_id)

roster02 <- 
  pbp_19 %>% 
  select(rusher_player_id, rusher_player_name) %>% 
  na.omit() %>% 
  distinct(rusher_player_id, .keep_all = TRUE) %>% 
  rename(name = rusher_player_name, id = rusher_player_id)

roster03 <- 
  pbp_19 %>% 
  select(receiver_player_id, receiver_player_name) %>% 
  na.omit() %>% 
  distinct(receiver_player_id, .keep_all = TRUE) %>% 
  rename(name = receiver_player_name, id = receiver_player_id)

roster04 <- 
  pbp_19 %>% 
  select(lateral_receiver_player_id, lateral_receiver_player_name) %>% 
  na.omit() %>% 
  distinct(lateral_receiver_player_id, .keep_all = TRUE) %>% 
  rename(name = lateral_receiver_player_id, id = lateral_receiver_player_name)

roster05 <- 
  pbp_19 %>% 
  select(interception_player_id, interception_player_name) %>% 
  na.omit() %>% 
  distinct(interception_player_id, .keep_all = TRUE) %>% 
  rename(name = interception_player_id, id = interception_player_name)

roster06 <- 
  pbp_19 %>% 
  select(punt_returner_player_id, punt_returner_player_name) %>% 
  na.omit() %>% 
  distinct(punt_returner_player_id, .keep_all = TRUE) %>% 
  rename(name = punt_returner_player_id, id = punt_returner_player_name)

roster07 <- 
  pbp_19 %>% 
  select(kickoff_returner_player_id, kickoff_returner_player_name) %>% 
  na.omit() %>% 
  distinct(kickoff_returner_player_id, .keep_all = TRUE) %>% 
  rename(name = kickoff_returner_player_id, id = kickoff_returner_player_name)

roster08 <- 
  pbp_19 %>% 
  select(punter_player_id, punter_player_name) %>% 
  na.omit() %>% 
  distinct(punter_player_id, .keep_all = TRUE) %>% 
  rename(name = punter_player_id, id = punter_player_name)

roster09 <- 
  pbp_19 %>% 
  select(kicker_player_id, kicker_player_name) %>% 
  na.omit() %>% 
  distinct(kicker_player_id, .keep_all = TRUE) %>% 
  rename(name = kicker_player_id, id = kicker_player_name)

roster10 <- 
  pbp_19 %>% 
  select(blocked_player_id, blocked_player_name) %>% 
  na.omit() %>% 
  distinct(blocked_player_id, .keep_all = TRUE) %>% 
  rename(name = blocked_player_id, id = blocked_player_name)

roster11 <- 
  pbp_19 %>% 
  select(tackle_for_loss_1_player_id, tackle_for_loss_1_player_name) %>% 
  na.omit() %>% 
  distinct(tackle_for_loss_1_player_id, .keep_all = TRUE) %>% 
  rename(name = tackle_for_loss_1_player_id, id = tackle_for_loss_1_player_name)

roster12 <- 
  pbp_19 %>% 
  select(qb_hit_1_player_id, qb_hit_1_player_name) %>% 
  na.omit() %>% 
  distinct(qb_hit_1_player_id, .keep_all = TRUE) %>% 
  rename(name = qb_hit_1_player_id, id = qb_hit_1_player_name)

roster13 <- 
  pbp_19 %>% 
  select(qb_hit_2_player_id, qb_hit_2_player_name) %>% 
  na.omit() %>% 
  distinct(qb_hit_2_player_id, .keep_all = TRUE) %>% 
  rename(name = qb_hit_2_player_id, id = qb_hit_2_player_name)

roster14 <- 
  pbp_19 %>% 
  select(forced_fumble_player_1_player_id, forced_fumble_player_1_player_name) %>% 
  na.omit() %>% 
  distinct(forced_fumble_player_1_player_id, .keep_all = TRUE) %>% 
  rename(name = forced_fumble_player_1_player_id, id = forced_fumble_player_1_player_name)

roster15 <- 
  pbp_19 %>% 
  select(solo_tackle_1_player_id, solo_tackle_1_player_name) %>% 
  na.omit() %>% 
  distinct(solo_tackle_1_player_id, .keep_all = TRUE) %>% 
  rename(name = solo_tackle_1_player_id, id = solo_tackle_1_player_name)

roster16 <- 
  pbp_19 %>% 
  select(solo_tackle_2_player_id, solo_tackle_2_player_name) %>% 
  na.omit() %>% 
  distinct(solo_tackle_2_player_id, .keep_all = TRUE) %>% 
  rename(name = solo_tackle_2_player_id, id = solo_tackle_2_player_name)

roster17 <- 
  pbp_19 %>% 
  select(assist_tackle_1_player_id, assist_tackle_1_player_name) %>% 
  na.omit() %>% 
  distinct(assist_tackle_1_player_id, .keep_all = TRUE) %>% 
  rename(name = assist_tackle_1_player_id, id = assist_tackle_1_player_name)

roster18 <- 
  pbp_19 %>% 
  select(assist_tackle_2_player_id, assist_tackle_2_player_name) %>% 
  na.omit() %>% 
  distinct(assist_tackle_2_player_id, .keep_all = TRUE) %>% 
  rename(name = assist_tackle_2_player_id, id = assist_tackle_2_player_name)

roster19 <- 
  pbp_19 %>% 
  select(pass_defense_1_player_id, pass_defense_1_player_name) %>% 
  na.omit() %>% 
  distinct(pass_defense_1_player_id, .keep_all = TRUE) %>% 
  rename(name = pass_defense_1_player_id, id = pass_defense_1_player_name)

roster20 <- 
  pbp_19 %>% 
  select(pass_defense_2_player_id, pass_defense_2_player_name) %>% 
  na.omit() %>% 
  distinct(pass_defense_2_player_id, .keep_all = TRUE) %>% 
  rename(name = pass_defense_2_player_id, id = pass_defense_2_player_name)

roster21 <- 
  pbp_19 %>% 
  select(fumbled_1_player_id, fumbled_1_player_name) %>% 
  na.omit() %>% 
  distinct(fumbled_1_player_id, .keep_all = TRUE) %>% 
  rename(name = fumbled_1_player_id, id = fumbled_1_player_name)

roster22 <- 
  pbp_19 %>% 
  select(fumble_recovery_1_player_id, fumble_recovery_1_player_name) %>% 
  na.omit() %>% 
  distinct(fumble_recovery_1_player_id, .keep_all = TRUE) %>% 
  rename(name = fumble_recovery_1_player_id, id = fumble_recovery_1_player_name)

roster23 <- 
  pbp_19 %>% 
  select(penalty_player_id, penalty_player_name) %>% 
  na.omit() %>% 
  distinct(penalty_player_id, .keep_all = TRUE) %>% 
  rename(name = penalty_player_id, id = penalty_player_name)




roster <- 
  rbind(roster01, roster02, roster03, roster04, roster05, roster06, roster07,
        roster08, roster09, roster10, roster11, roster12, roster13, roster14,
        roster15, roster16, roster17, roster18, roster19, roster20, roster21,
        roster22, roster23
  )
full_roster_19 <- 
  roster %>% 
  distinct(id, .keep_all = TRUE)

full_roster_19 %>% write_csv("data-raw/NFL_full_roster_2019.csv")
