

season_stats |>
  filter(leagueAbbrev == 'NHL') |>
  mutate(across(c(season), ~{as.integer(str_sub(.x, 1,4))}))   |> 
  #filter(playerId == 8448040  & season == 19771978     )
  summarise(gamesPlayed = sum(gamesPlayed), .by = c(playerId, season)) |> 
  arrange(desc(gamesPlayed)) |>
  filter(season  >= 1967) |> 
  ggplot(aes(x = season, y = gamesPlayed, color = season, group = season)) +
  geom_boxplot()
  filter(season == max(   season)) |>
  filter(gamesPlayed  == max(gamesPlayed ,na.rm = TRUE))  |>
  count(season )


xxx |> count(leagueAbbrev   , sort = T)
xxx |> sample_n(5000) |> View()
season_stats <- nhl_player_season_totals(player = 'all', lang = 'en') 

first_last_NHL <- 
  season_stats |> 
  filter(leagueAbbrev == 'NHL') |>
  summarise(season_min = min(season), season_max = max(season), .by = playerId) |>
  arrange(season_min)

first_last_other <- 
  season_stats |> 
  filter(leagueAbbrev != 'NHL') |>
  select(playerId, leagueAbbrev, season) |>
  rename(season_other := season)


p_dat <- 
  first_last_NHL |>
  left_join(first_last_other, by = join_by(playerId)) |>
  mutate(across(c(season_min, season_other), ~{as.integer(str_sub(.x, 1,4))}))   |> 
  mutate(season_diff = season_min - season_other) |>
  filter(season_diff <= 1 & season_other <= season_min ) |>
  slice_min(season_other, n = 1,by = playerId , with_ties = TRUE)  |>
  count(season_min ,  leagueAbbrev ) |>
  filter(season_min >= 1967)


league_keep <-
  p_dat |>
  filter(season_min == max(season_min)) |>
  slice_max(n, n = 7, with_ties = FALSE) |>
  pull(leagueAbbrev) |>
  unique()


# league_keep <- 
#   p_dat |> 
#   slice_max(n, n = 1,by = season_min  , with_ties = FALSE) |>
#   pull(leagueAbbrev) |> 
#   unique()
league_keep <-   
  p_dat |> 
  summarise(n = sum(n), .by = leagueAbbrev) |>
  slice_max(order_by = n, n = 7) |>
  pull(leagueAbbrev) |> 
  unique() |> 
  c(league_keep) |> 
  unique() 




p_dat |>
  mutate(
    leagueAbbrev = case_when(
      leagueAbbrev %in% league_keep ~ leagueAbbrev,
      .default = 'Other'
    )
  ) |> 
  summarise(n = sum(n), .by = c(leagueAbbrev, season_min )) |> 
  mutate(leagueAbbrev = fct_reorder(leagueAbbrev, .x = n, .fun = sum)) |>
  mutate(f = n/sum(n), .by = season_min)  |>
  
  ggplot(aes(x = season_min , y =f , fill = leagueAbbrev  )) +
  geom_col( width = 1) +
  guides(fill = 'none', color = 'none')

