source(file.path('R', 'source_here.R'))

here_source('cache_vec.R')
here_source('season_team_vector.R')

require(glue)
require(purrr)
require(dplyr)


# schedule_all_dates()
# https://api-web.nhle.com/v1/standings/2023-11-10

#' Title
#'
#' @param dt a date formated as a character YYYY-MM-DD
#' @param pattern 
#' @param url_type 
#' @param var_nm 
#'
#' @return
#' @export
#'
#' @examples
#'   standings_BASE('2024-11-22')
#' 
standings_BASE <- function(
    dt =  as.character(Sys.Date()),
    pattern = 'standings/{dt}',
    url_type = 'base',   
    var_nm = 'games'
){
  dt <- as.character(dt)
  response <- get_url_base(pattern = pattern, url_type = url_type)  
  response[['standings']]
}

games_df <- read_feather_here('game_landing_tombstone.feather')
game_dates <- 
  games_df |>
  distinct(gameDate) |>
  rename(dt := gameDate) |>
  mutate(dt = as.Date(dt))

non_game_dates <- 
  tibble(dt = seq(from  = min(game_dates$dt), to = max(game_dates$dt), by = 1))  |>
  anti_join(game_dates)
  

standings_BASE(dt = games |> pull(gameDate) |> sample(1))
############################
# Download all the box-scores for every game
standings_db <- 
  download_data(
  func = standings_BASE,
  all_args = game_dates |> sample_frac(1),
  file_name_prefix = 'standings_'
)

standings <- read_feather_here('standings_result.feather')
standings |> count(seasonId)
p_dat <- 
  standings |> 
  #sample_frac(1) |> 
  select (seasonId, teamAbbrev_default, conferenceAbbrev, divisionAbbrev, gamesPlayed, wins, losses,  ties, points) |> 
  mutate(season_start_yr = as.integer(str_sub(seasonId,  1,4) ), 
         season_lbl = glue('{str_sub(seasonId,  1,4)}-{str_sub(seasonId,  7,8)}')) |>
  select(-seasonId) |> 
  distinct() |>
  mutate(points_back = max(points) - points, .by = c(season_start_yr, gamesPlayed ,  conferenceAbbrev  )) |> arrange(desc(points_back)) |>
  #filter(season_start_yr == 1992 ) |> 
  #filter(conferenceAbbrev  == 'XVE'& divisionAbbrev == 'NE') |> 
  distinct() |>
  arrange(season_start_yr , gamesPlayed)

p_dat_lbl <- p_dat |> filter(gamesPlayed == max(gamesPlayed),.by = c(season_start_yr, teamAbbrev_default))




p_dat |>
  left_join(p_dat_lbl |>  select(teamAbbrev_default , season_start_yr, points_back)  |> rename(points_back_final := points_back),
            by = join_by(teamAbbrev_default, season_start_yr) )  |> 
  mutate(points_recovered = points_back - points_back_final) |>
  distinct(teamAbbrev_default, season_start_yr, points_recovered) |>
  summarise(points_recovered = max(points_recovered), .by = c(teamAbbrev_default, season_start_yr) ) |>
  arrange(desc(points_recovered))  |> 
  head(20)
  


p_dat_2 <-p_dat |> filter(season_start_yr == 2006 )
p_dat_lbl_2 <- p_dat_2 |> filter(gamesPlayed == max(gamesPlayed),.by = c(season_start_yr, teamAbbrev_default))




p_dat_2 |>
  ggplot(aes(x=gamesPlayed  , y = points_back, color = teamAbbrev_default)) + 
  geom_line() +
  geom_label_repel(data =p_dat_lbl_2, mapping = aes( label = teamAbbrev_default) )  +
  guides(color = 'none') +
  facet_wrap(vars(divisionAbbrev ))
  


p_dat_2 |>
  ggplot(aes(x=gamesPlayed  , y = points, color = teamAbbrev_default)) + 
  geom_line() +
  geom_label_repel(data =p_dat_lbl_2, mapping = aes( label = teamAbbrev_default) )  +
  guides(color = 'none') +
  facet_wrap(vars(divisionAbbrev ))




standings$

standings$`__args_key`  |> unique()
read_feather_here('game_landing_tombstone.feather')
dat <- read_feather_here('standings_result.feather')
dat$`__args_key`  |> unique()
dat <- standings_BASE()  |> View()
dat$teamAbbrev_default
dat$points
# x  <- standings_BASE(sample(db$result$gameDate, 1))
# x$teamName_default