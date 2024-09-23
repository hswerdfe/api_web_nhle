


source(file.path('R', 'source_here.R'))
here_source_dir()

game_ids <- nhl_season_full(team = 'all', season = seq(2008, 2023), lang = 'en') |> 
  #slice_sample(n =25, by = season) |> 
  pull(id) |> 
  unique() 


threeStars <- 
  game_ids |> 
  map_dfr(~{
  
  
  landing <- nhl_get('gamecenter/{.x}/landing')
  
  summary <- landing$summary

  winning_team <- dplyr::case_when(
    summary$linescore$totals$home >  summary$linescore$totals$away ~ landing$homeTeam$abbrev, 
    summary$linescore$totals$home <  summary$linescore$totals$away ~ landing$awayTeam$abbrev, 
    summary$linescore$totals$home ==  summary$linescore$totals$away ~ 'TIE GAME',
    .default = 'unsure who won'
  )
  losing_team <- dplyr::case_when(
    summary$linescore$totals$home >  summary$linescore$totals$away ~ landing$awayTeam$abbrev, 
    summary$linescore$totals$home <  summary$linescore$totals$away ~ landing$homeTeam$abbrev, 
    summary$linescore$totals$home ==  summary$linescore$totals$away ~ 'TIE GAME',
    .default = 'unsure who won'
  )  
  
  if (is.null(summary)){
    return(tibble(game_id = .x))
  }
  threeStars <- summary$threeStars
  if (is.null(summary)){
    return(tibble(game_id = .x))
  }
  if (nrow(threeStars) == 0 ){
    return(tibble(game_id = .x))
  }
  threeStars |> 
    mutate(game_id = .x, 
           is_home = teamAbbrev == landing$homeTeam$abbrev,
           is_away = teamAbbrev == landing$awayTeam$abbrev,
           is_winning = teamAbbrev == winning_team,
           is_losing = teamAbbrev == losing_team,
           dplyr::case_when(
             is_winning ~ 'winning',
             is_losing ~ 'losing',
             .default = 'tie'
           ),
           winner = winning_team,
           gameDate = landing$gameDate,
           season =landing$season
    )
})


threeStars |>
  dplyr::filter(!is.na(star)) |>
  count(season, star, is_winning) |>
  ggplot(aes(x = season, y = n, fill = is_winning)) +
  geom_col() +
  facet_grid(rows = vars(star))



game_id <- game_ids[[300]]
season = seq(2008, 2023)
games_all <- nhl_season_full(team = 'all', season = 'all', lang = 'en')



nhl_roster_names_long <- function(roster = nhl_roster_full(team = 'all', season = 'all', lang = 'en')){
  roster |>
  pivot_longer(cols = matches('^((first|last)Name|(birth(City|StateProvince)))_(default|[a-z][a-z])$'), values_drop_na  = TRUE) |>
    separate(name, into = c('name','lang'), sep = '_') |>
    distinct() |> 
    pivot_wider(names_from = name, values_from = 'value') 
}
x <- nhl_roster_full(team = 'all', season = 'all', lang = 'en')
x$season |> unique() |> sort()
nhl_roster_full(team = 'all', season = 'all', lang = 'en')  |> count(shootsCatches  )
nhl_roster_names_long()
nhl_roster_full(team = 'all', season = 'all', lang = 'en') |> 
  count(season, birthCountry) |>
  mutate(birthCountry = fct_lump(birthCountry, n = 8)) |>
  summarise(n = sum(n), .by = c(season, birthCountry)) |>
  mutate(f = n / sum(n), .by = season) |> 
  filter(season == max(season))
  ggplot(aes(x = season, y = f, fill = birthCountry)) +   geom_col(color = 'black') +
  theme_minimal()
  
  filter(lang != 'default') |> view()
nhl_roster_full(team = 'all', season = 'all', lang = 'en')

nhl_country(lang = 'en')$imageUrl
games_all |>
  pivot_longer()

games_all |> sample_n(3000) |> View()

game_id <- game_ids[[1]]

############################
# Play by play parts
plays <- game_ids |> map_dfr(~{
  nhl_get('gamecenter/{.x}/play-by-play')$plays
})

rosterSpots <- game_ids |> map_dfr(~{
  nhl_get('gamecenter/{.x}/play-by-play')$rosterSpots
})

  
  rosterSpots |>
    pivot_longer(cols = matches('^(first|last)Name_(default|[a-z][a-z])$'), values_drop_na  = TRUE) |>
    separate(name, into = c('name','lang'), sep = 'Name_') |>
    distinct() |>
    pivot_wider(names_from = name, values_from = 'value') |>
    mutate(full = str_squish(paste0(first, ' ', last)), 
           full_2 = str_squish(paste0(last, ', ', first))
    ) |> count(lang, sort = TRUE)
  



teamGameStat <- game_ids |> map_dfr(~{
  nhl_get('gamecenter/{.x}/play-by-play')$summary$teamGameStat
})


seasonSeries <- game_ids |> map_dfr(~{
  nhl_get('gamecenter/{.x}/play-by-play')$summary$seasonSeries
})

.x <- 2009020102

threeStars |> 
  mutate(season_year = as.integer(str_sub(game_id   , 1,4))) |>
  count(season_year) |> 
  arrange(season_year)



.x <- 2024020021
nhl_get('gamecenter/2024020021/landing')$summary$threeStars

nhl_get('gamecenter/{game_id}/play-by-play')



nhl_get('location')


nhl_get('gamecenter/{game_id}/landing')$summary$scoring |> as_tibble()
x <- nhl_get('gamecenter/{game_id}/landing')$summary$threeStars

x$
nhl_get('wsc/game-story/{game_id}')

nhl_get('gamecenter/{game_id}/boxscore')

nhl_get('meta/game/{game_id}')

cache_save()

year = 2023
nhl_get('playoff-bracket/{year}')
