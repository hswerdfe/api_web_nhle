


schedule(team = c('tor', 'ott'), season = c(20222023, 20232024) )

#' games
#' 
#'   returns a list of all game IDs
#'
#' @return vector of game IDs
#' @export
#'
#' @examples
#'     games()
#' 
games  <- function(team = c('tor', 'ott', 'det', 'mtl'), season = c(19621963, 20222023, 20232024)){
  schedule(team = team, season = season ) |> 
  # schedule(team = 'all', season = 'all' ) |> 
    pull(id)  |> 
    unique()
}
all_game_ids <- games(team = 'tor', season =  seasons('tor'))


g_ids <- 
tibble(id = all_mage_ids) |>
  mutate(yr = str_sub(as.character(id), 1,4)) |> 
  slice_sample(n =12 , by = yr ) |>
  pull(id)


pbp_sample <- 
g_ids |> 
  purrr::set_names() |> 
  purrr::map(\(gameid){
    get_url_base('gamecenter/{gameid}/play-by-play', url_type = 'base', get_func = get_url_possibly_cached)    
  })

# details_xCoord was started in 2009-20010 season
pbp_sample |> map_dfr(~{tibble(cols = colnames(.x$plays)) |> mutate(yr = year(as_date(.x$gameDate)))}) |>
  #filter(cols == 'details_xCoord') |> count(yr)
  #filter(str_detect(cols , 'type')) |> count(cols)
  count(cols, yr, sort = T) |>
  arrange(desc(yr)) |> 
  filter(str_detect(cols, 'Coord')) |> View()


pbp_sample |> map_dfr(~{
  p <- .x$plays 
  p[['typeDescKey']] <- coalesce(as.character(p[['typeDescKey']]), as.character('NA'))
  p[['typeCode']] <- coalesce(as.character(p[['typeCode']]), as.character('NA'))
  #p[['details_typeCode']] <- coalesce(as.character(p[['details_typeCode']]), as.character('NA'))
  p |>
    count(typeDescKey, typeCode) |> 
    mutate(id = .x$id)
}) |> 
  mutate(yr = as.integer(str_sub(id,1,4))) |>
  summarise(yr = min(yr), .by = typeDescKey) |>
  arrange(yr)
  
  


View()
    
    pbp_sample[[1]][['asdfasdgasdga']]
details_xCoord
pbp_sample[[1]]$gameDate


pbp <- get_url_base('gamecenter/{gameid}/play-by-play', url_type = 'base', get_func = get_url_possibly_cached)


all_mage_ids |> as.character() |> str_sub(1,4)
teams()
seasons()
gameid  = 2023021261 
gameid  = 1962020029

pbp$plays |> View()

names(pbp)

pbp$rosterSpots |> View()

landing <- get_url_base('gamecenter/{gameid}/landing', url_type = 'base', get_func = get_url_possibly_cached)
names(landing$summary)
landing  |>
  extract2('summary') |> 
  extract2('threeStars')
  [['summary2']][['threeStars']]

landing[['summary2']][['threeStars']]
names(landing)
summary <- landing$summary
names(summary)

summary$gameInfo$homeTeam$scratches
summary$gameInfo$awayTeam$scratches
summary$gameInfo$linesmen
summary$gameInfo$referees
summary$penalties |> 
  as_tibble() |>
  unnest('penalties')
  

summary$shotsByPeriod  |>
  as_tibble()


summary$teamGameStats


summary$threeStars
summary$shootout

summary$scoring |> tibble() |> unnest(cols = c(goals))


landing$tvBroadcasts

landing$p


threeStars <- summary$threeStars
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
