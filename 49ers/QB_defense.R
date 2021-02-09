defense <- "SF"
szn <- 2020

opponents <- nflfastR::fast_scraper_schedules(szn) %>%
  dplyr::filter(home_team == defense | away_team == defense) %>%
  dplyr::mutate(
    opp = dplyr::if_else(home_team == defense, away_team, home_team)
  ) %>%
  dplyr::left_join(
    nflfastR::teams_colors_logos,
    by = c("opp" = "team_abbr")
  )