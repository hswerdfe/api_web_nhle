

library(ggrepel)
library(dplyr)
library(arrow)

library(dplyr)
library(ggplot2)
library(ggimage)

source(file.path('R', 'source_here.R'))
here_source('nhl_param_norm.R')
here_source('request_wrapper.R')
here_source('vectorizer.R')
here_source('url_make.R')

################################
#Seasons where API seams to have 3 Stars, much of the time.
seasons <- seq(2008, 2023)




###########################
# Get all the games that
game_ids <- nhl_season_full(team = 'all', season = seasons, lang = 'en') |> 
  pull(id) |> 
  unique() 



######################
# Build up 3 stars dataframe one request at a time >20,000 requests later!!!!!
threeStars <- 
  game_ids |> 
  imap(~{
    cat(.y, ' ', Sys.time() , ' ... ')
    
    landing <- nhl_get('gamecenter/{.x}/landing')
    
    summary <- landing$summary
    names(landing)
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
             game_win_lose = dplyr::case_when(
               is_winning ~ 'winning',
               is_losing ~ 'losing',
               .default = 'tie'
             ),
             winner = winning_team,
             gameDate = landing$gameDate,
             season =landing$season
      )
  }) |>
  list_rbind()




theme_set(
  theme_minimal()
)
threeStars |> write_feather('3_stars.feather')
threeStars <- read_feather('3_stars.feather') |>
  rename(game_win_lose := 'dplyr::case_when(...)') |>
  filter(!is.na(star)) |>
  mutate(season_lbl = fct_reorder(paste0(str_sub(season, 1,4) ,'-',str_sub(season, 7,8)), season) ,
         star_points = case_match(star,
                                         1 ~ 30,
                                         2 ~ 30,
                                         3 ~ 10,
                                         .default = NA
         ),
         star = fct_reorder(
           case_match(star,
                      1 ~ '1st Star ⭐',
                      2 ~ '2nd Star ⭐⭐',
                      3 ~ '3rd Star ⭐⭐⭐'
           ), star
         )
         
  )


p_dat <- 
  threeStars |> 
  count(season, season_lbl, star, game_win_lose) |>
  mutate(f = n / sum(n), .by = c( season,  season_lbl, star)) |>
  filter(game_win_lose == 'winning')

main_lbl  <-    
  p_dat  |>
    summarise(season = mean(range(season)), f = 0.25, .by = c(star))

end_lbls <- p_dat |> filter(season %in% range(season))

############################
# Winning 
p_dat |>  
  ggplot(aes(x = season, y = f, color = star)) +
  #geom_hline(yintercept = 0.5, color = 'grey') +
  #geom_col(width = 10001, color = 'black', alpha = 0.25) +
  geom_line(linetype = 'dashed') +
  geom_smooth(method = lm, se = FALSE)  +
  geom_label(aes(y = f, label = paste0(round(f*100, 0), '%')),
             fill = 'white',
             alpha = 1
  ) + 
  geom_label_repel(data = end_lbls, mapping = aes(label = star, fill = star), color = 'white',  nudge_y  = 0.02)  +
  labs(
    title = 'NHL: Probability of Star Being Awarded To Winning Team.',
    subtitle = '',
    fill = 'Star on ____   team',
    y = '',
    x = 'Season', 
    caption  = 'Source :  https://api-web.nhle.com/v1/gamecenter/{game-id}/landing'
  ) +
  annotate(
    geom = 'text', 
    x = mean(range(p_dat$season)), 
    y = 0.86, 
    color = 'grey',
    size = 10,
    label = str_to_title('2nd Stars: Awarded to Losing team more in the past.')
    
  ) +
  scale_x_continuous(
    breaks = c(first(p_dat$season), last(p_dat$season)),
    labels = c(first(p_dat$season_lbl), last(p_dat$season_lbl))
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, color = 'black'),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5) ,
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  guides(
    color = 'none',
    fill = 'none'
  )

season_rng <- 
  threeStars |> 
  filter(season %in% range(season))  |>
  select(season_lbl) |>
  distinct() |>
  pull() |>
  paste0(collapse = ' to ')
  

########################
#  HOME
threeStars |>
  count(star, is_home) |>
  mutate(f = n / sum(n), .by = c( star)) |>
  filter(is_home  == TRUE) |>
  ggplot(aes(x = star, y = f)) +
  geom_col(alpha = 0.5, color = 'black', fill = 'lightgrey', alpha = 0.5) + 
  geom_label(aes(y = f, label = paste0(round(f*100, 0), '%')),
             fill = 'white',
             alpha = 1,
             size = 8
  )   +
  geom_text(date = main_lbl, mapping = aes(
    label = star
  ), y = 0.15,  size = 14) +
  annotate(
    geom = 'label', 
    x = 2, 
    y = 0.45, 
    color = 'grey',
    size = 10,
    label = str_to_title('Home team : More likely to be awared all stars')
    
  ) +
  geom_hline(yintercept = 0.5) +
  labs(
    title = glue('NHL: Probability of Star Being Awarded To Home (🏠)  Team.'),
    subtitle = glue('Seasons {season_rng}'),
    y = '',
    x = '', 
    caption  = 'Source :  https://api-web.nhle.com/v1/gamecenter/{game-id}/landing'
  ) +  
  # scale_y_continuous(
  #   limits = c(0, 1),
  #   breaks = c(0, 0.5, 1), 
  #   labels = c('0%', '50%', '100%')
  # ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5) ,
    strip.background = element_blank(),
    strip.text = element_blank()
  )



p_dat <- 
  threeStars |>
  filter(!is.na(star)) |>
  count(season, season_lbl,  position) |>
  filter(position != 'F') |>
  mutate(position = 
      case_match(
             position,
             'C' ~ 'Center',
             'D' ~ 'Defence',
             'L' ~ 'Left Wing',
             'R' ~ 'Right Wing',
             'G' ~ 'Goalie',
            )
  )  |>  
  mutate(f = n / sum(n), .by = c( season,season_lbl))


ends_lbls <- 
  p_dat  |> 
  filter(season %in% range(season))

mid_lbls  <- 
  p_dat  |> 
  filter(season %in% mean(range(season)))



############################
#
#  By Position
#
p_dat |>
  ggplot(aes(x = season, y = f, color = position)) +
  geom_line()  +
  geom_point()  +
  geom_label_repel(
    data = ends_lbls,
    aes(label =  paste0(round(f * 100, 0),  '%')),
    size = 8,
    alpha = 0.75
  ) + 
  geom_label_repel(
    data = mid_lbls,
    mapping = aes(label =  position, fill = position ),
    color = 'white',
    size = 12,
    alpha = 0.75
  ) + 
  annotate(
    geom = 'text', 
    x = mean(range(p_dat$season)), 
    y = 0.25, 
    color = 'grey',
    size = 10,
    label = str_to_title('Center: most and Increasingly likely\nto be awarded any Star.')
    
  ) +  
  guides(color = 'none') +
  labs(
    title = glue('NHL: Probability of Star Being Awarded by position.'),
    subtitle = glue('Seasons {season_rng}'),
    y = '',
    x = '', 
    caption  = 'Source :  https://api-web.nhle.com/v1/gamecenter/{game-id}/landing'
  ) +  
  scale_x_continuous(
    breaks = c(first(p_dat$season), last(p_dat$season)),
    labels = c(first(p_dat$season_lbl), last(p_dat$season_lbl))
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20, hjust = 0.5),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5) ,
    strip.background = element_blank(),
    strip.text = element_blank()
  )+  
  guides(
    color = 'none',
    fill = 'none'
  )

threeStars_cum <-   
  threeStars  |>
  arrange(gameDate) |>
  mutate(star_points_cum = cumsum(star_points), .by = playerId) 
  
  
rise_of_stars <-
  threeStars_cum |> 
  summarise(star_points_max = max(star_points_cum), .by = playerId) |>
  slice_max(star_points_max,n = 50)
  
colored_players <- 
  rise_of_stars |>
  slice_max(star_points_max,n = 8, with_ties = FALSE)


image_players <- 
  colored_players |>
  slice_max(star_points_max,n = 3, with_ties = FALSE)




p_dat <- 
  threeStars_cum |>
  inner_join(rise_of_stars, by = 'playerId') |>
  select(-headshot) |>
  inner_join(
  nhl_roster_full(team = 'all', season = 'all', lang = 'en') |>
  select(id, season, headshot, firstName_default, lastName_default, positionCode , sweaterNumber) |> 
  slice_max(season, by = id) |> 
  mutate(lbl = glue('{q}')),
  by = c('playerId' = 'id')
  ) |>
  mutate(gameDate = as.Date(gameDate))

library(cluster) 


p_dat_lbl <- 
  p_dat |> 
  inner_join(colored_players, by = 'playerId') |>
  #group_by(playerId)  |>
  slice_max(gameDate, n = 1, by = 'playerId') 
  #sample_n(1) |>
  #ungroup()
  

  
p_image_lbl <-  
  p_dat_lbl |>
  inner_join(image_players, by = 'playerId')

p_dat |> select(gameDate)

p_dat |> 
  ggplot(aes(x = gameDate, y = star_points_cum, group = playerId)) +
  geom_line(color = 'grey', size = 0.1) +
  geom_line(data =p_dat |> inner_join(colored_players, by = 'playerId') , aes(color = lbl), size = 1) +
  #geom_point(data =p_dat |> inner_join(colored_players, by = 'playerId') , aes(color = lbl)) +
  #geom_image(data = p_image_lbl, aes(image = headshot), size = 0.10, alpha = 0.5) +   
  #geom_image(data = p_image_lbl, aes(image = headshot, color = lbl), size = 0.10, alpha = 0.5) + 
  geom_label_repel(data = p_dat_lbl, aes(label = lbl, color = lbl), nudge_x = 300) +    
  labs(
    title = str_to_title('3 Stars: Cummulative Star Point'),
    subtitle = str_to_title('Unfortunately: games before 2009 do not typically report the 3 stars'),
    y = '', x = '',
    caption  = 'Source :  https://api-web.nhle.com/v1/gamecenter/{game-id}/landing'
    
  ) +
  scale_x_date(
    breaks = range(p_dat$gameDate)
    
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20, hjust = 0.5),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5) ,
    strip.background = element_blank(),
    strip.text = element_blank()
  ) 

  



star_points <- 
  threeStars  |>
  summarise(
    n = n(), 
    star_points = sum(star_points), 
    .by = c(playerId, season, season_lbl)
  ) |>
  arrange(desc(star_points)) |> 
  mutate(rank =  rank(desc(star_points),  ties.method = 'random'), .by = c(season, season_lbl) ) |>
  filter(rank <= 3)




# Perform your data operations
combined_data <- 
  nhl_roster_full(team = 'all', season = 'all', lang = 'en') |>
  select(id, season, headshot, firstName_default, lastName_default, positionCode , sweaterNumber) |>
  inner_join(star_points, by = c('id' = 'playerId', 'season' = 'season'))  |>
  mutate(lbl = glue('{lastName_default}') )# {sweaterNumber} {positionCode}'))

combined_data |>
  filter(season  >20092010)  |>
  ggplot(aes(x = season_lbl, y = star_points)) +
  geom_col(width = 1) +
  geom_image(aes(image = headshot,y=300), size = 0.35) +  # Adjust size as needed
  geom_label(aes(label = lbl), vjust = -0.5) +  # Adjust label position
  #geom_label(aes(label = star_points, y = 0), vjust = -0.5) +  # Adjust label position
  facet_grid(rows = vars(rank)) +
  labs(
    x = 'Season',
    y = 'Star Points'
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20, hjust = 0.5),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5) ,
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +  
  theme_minimal() +
  

nhl_teams(team = 'all')


nhl_roster_full(team = 'all', season = 'all', lang = 'en') |>
  select(id, season, headshot, firstName_default, lastName_default) |>
  #distinct(id, .keep_all = TRUE)  |>
  inner_join(star_points,by = c('id' = 'playerId', 'season' = 'season')) |># pull(headshot)
  ggplot(aes(x = season_lbl, y = star_points)) +
  geom_col() +
  geom_label(aes(label = star_points))  +
  facet_grid(rows = vars(rank))
  

threeStars |>
  filter(!is.na(star)) |>
  count(season, playerId, sort = TRUE)


nhl_roster_full(team = 'all', season = 'all', lang = 'en') |>
  filter(id == 8469608) |>
  distinct()

