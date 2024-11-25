
source(file.path('R', 'source_here.R'))
library(feather)
here()
file.path(here(), 'data', 'player_awards.feather')  |> read_feather()
here_source()

db <- read_db(file_pattern = '^game_landing_(.*)\\.feather$')

games_dat  <- read_feather_here('game_landing_tombstone.feather')


roster_dat <- 
  read_feather_here('roster_result.feather')  |>
  extract_args() |>
  mutate(season_start_yr = as.integer(str_sub(season,  1,4) ),
         positionCode = case_match(
           positionCode, 
           'C' ~  'Forward',
           'L' ~  'Forward',
           'R' ~  'Forward',
           'D' ~  'Defence',
           'G' ~  'Goalie',
         )) |>
  mutate(season_in_league = season_start_yr - min(season_start_yr), .by = id)  |>
  mutate(age = season_start_yr - year(birthDate))


roster_dat |>
  count(positionCode, season_start_yr, age,  team, sort = T)

all_teams <- 
  games_dat |> 
  select(homeTeam_abbrev, awayTeam_abbrev) |>
  pivot_longer(cols = everything(), values_to = 'team')  |>
  distinct(team) # |>
  #pull()
  
win_or_lose <-
  games_dat |>
  filter(gameType == 2)  |>
  select(id, season, homeTeam_abbrev,  awayTeam_abbrev, homeTeam_score, awayTeam_score ) |>
  rename(homeTeam := homeTeam_abbrev,awayTeam := awayTeam_abbrev) |>
  mutate(season_start_yr = as.integer(str_sub(season,  1,4) )) |>
  mutate(winner = case_when(
    homeTeam_score > awayTeam_score ~  homeTeam ,
    awayTeam_score > homeTeam_score ~  awayTeam ,
    .default =  NA),
    loser = case_when(
      homeTeam_score < awayTeam_score ~  homeTeam ,
      awayTeam_score < homeTeam_score ~  awayTeam ,
      .default = NA)    
  ) |>  distinct(id, season_start_yr, winner, loser,homeTeam, awayTeam) 

games_played <- 
win_or_lose |>
  select(id, season_start_yr, homeTeam, awayTeam) |>
  pivot_longer(cols = c(homeTeam, awayTeam),values_to = 'team')  |>
  summarise(games_played = n(), .by = c(season_start_yr, team)) |>
  arrange(desc(games_played))



bind_rows(
  all_teams |>
    left_join(win_or_lose  |> count(season_start_yr, winner)  |> rename(team   :=  winner),by ='team')  |> mutate(type = 'wins'),  
  
  all_teams |>
    left_join(win_or_lose  |> count(season_start_yr, loser)  |> rename(team   :=  loser),by ='team')  |> mutate(type = 'loses')
)  |>
  pivot_wider(names_from = type, values_from = n)  |>
  left_join(games_played,  by = join_by(team, season_start_yr))  |>
  mutate(f_win = wins/(games_played), 
         f_lose = loses/(games_played)
         ) |>
  arrange(desc(f_win))  |>
  inner_join(roster_dat |> 
               summarise(age = mean(age, na.rm = TRUE), roster_size = n(), .by = c(season_start_yr,  team)) |> filter(roster_size >= 30) ,  
             by = c('team', 'season_start_yr')
  ) |> 
  filter(season_start_yr >= max(season_start_yr)-50 & season_start_yr <= max(season_start_yr)-1) |>  arrange(season_start_yr)  |>
  filter(n() > 15, .by = team) |>
  #filter(season_start_yr == max(season_start_yr) -1 ) |>
  ggplot(aes(x=age, y = f_win))  + 
  geom_jitter()  +
  geom_smooth(method ='lm',  se= FALSE) # +
  geom_path()
  


  
  save_db()
  
games_dat$los
games_dat$homeTeam_abbrev
games_dat$awayTeam_abbrev
games_dat$homeTeam_score
games_dat$awayTeam_score


read_feather('data/player_awards.feather')
