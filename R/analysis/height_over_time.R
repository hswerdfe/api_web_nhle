
library(ggrepel)
source(file.path('R', 'source_here.R'))
here_source('cache_vec.R')
here_source('season_team_vector.R')
here_source('download.R')
require(glue)
require(purrr)
require(dplyr)
library(gganimate)



# Function to format y-axis labels as feet and inches
format_height <- function(height_inch, digits = 0) {
  feet <- floor(height_inch / 12)
  inches <- height_inch %% 12
  glue('{feet}ft {round(inches, digits  =   digits )}in')
}


roster <- 
  read_db(file_pattern = 'roster_(.*).feather') |>  
  extract2('result') |> 
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
  mutate(season_in_league = season_start_yr - min(season_start_yr), .by = id)



p_dat <- roster |>
  summarise(heightInInches  = mean(heightInInches, na.rm = TRUE ),
            num = n(),
            .by = c(positionCode , season_start_yr))  #|>
  #filter(season_start_yr >= 1975 & season_start_yr  <= 2023)  
roster |> 
  distinct(id, firstName_default , lastName_default , heightInInches) |>
  arrange(desc(heightInInches)) |> mutate(ht_ft_in = format_height(height_inch  =heightInInches,digits=  7)) |>
  head(19)

roster |> 
  distinct(id, firstName_default , lastName_default , heightInInches) |>
  filter(lastName_default == 'Dryden' & firstName_default == 'Ken')  |>
  mutate(ht_ft_in = format_height(height_inch  =heightInInches,digits=  7))


roster |> 
  distinct(id, `__cached_on`, firstName_default , lastName_default , heightInInches) |>
  filter(lastName_default == 'Rempe' & firstName_default == 'Matt')  |>
  mutate(ht_ft_in = format_height(height_inch  =heightInInches,digits=  7))


p_dat_lbl <- 
  p_dat |> 
  filter(heightInInches %in% range(heightInInches), .by = positionCode ) |>
  mutate(lbl = glue('{positionCode} in {season_start_yr}\n{format_height(heightInInches)}'))  



  


p <- 
  p_dat |> 
  ggplot(aes(x = season_start_yr, y = heightInInches, fill = positionCode, color = positionCode)) +
  geom_smooth(level  = NA) + 
  geom_point() +
  scale_y_continuous(breaks = round(seq(min(p_dat$heightInInches), max(p_dat$heightInInches), 1)), labels = format_height) +
  geom_label_repel(
    data = p_dat_lbl, 
    mapping = aes(label = lbl),
    color = 'black',
    alpha = 0.5
  )  +
  scale_x_continuous(breaks = seq(min(p_dat$season_start_yr), max(p_dat$season_start_yr), 5))  +

  theme_minimal()  +
  guides(fill = 'none', color = 'none')  +
  labs(x = 'Season', 
       y = 'Average Height', title = 'Average Height in the NHL by Position and Year', 
       subtitle = 'Goalies went from the shortest players in the 1980s to the tallest today.'
      ) +
  theme(axis.text.x = element_text(size = 13, color = 'darkgrey'), 
        axis.text.y = element_text(size = 13, color = 'darkgrey'),
        panel.grid.major =  element_line(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 20, color = 'grey'),
        plot.title = element_text(size = 35, color = 'grey',hjust = 0.5),
        plot.subtitle = element_text(size = 15, color = 'grey',hjust = 0.5)
        
      ) 
p

ggsave(file.path('R', 'analysis',  "player_height_by_year_position_line.jpg"), plot = p)


pp_dat <- 
  roster |>
  filter(!is.na(heightInInches)) |>
  count(season_start_yr, positionCode, heightInInches) |>
  mutate(f = n/sum(n), .by = c(season_start_yr, positionCode)) |>
  filter(season_start_yr >= 1975 & season_start_yr  <= 2023)  
pp_dat_lbl <- 
  pp_dat |> 
  mutate(f = mean(range(f))/2, heightInInches = max(heightInInches)) |>
  select(-n) |>
  distinct() |>
  mutate(lbl = glue('{positionCode}' ))



pp_data_lbl_yr <- 
  pp_dat |> 
  summarise(
    f = mean(range(f)), heightInInches= mean(range(heightInInches))
  )  |>
  mutate(positionCode  = 'Forward')  |>
  cross_join(pp_dat |> distinct(season_start_yr))

animated_plot <- 
  pp_dat |> 
  ggplot(aes(x = heightInInches, y = f, fill= positionCode)) + 
  geom_col(alpha = 0.5, width = 1,  colour = 'black') + 
  geom_label(data = pp_dat_lbl, mapping = aes(label = lbl), size = 8, color = 'white', alpha = 0.5)   +
  geom_text(data = pp_data_lbl_yr, mapping = aes(label = season_start_yr), size = 40, color = 'grey', alpha = 0.5)   +
  
  scale_x_continuous(breaks = function(limits) seq(0, limits[2], by = 1), labels = format_height) +
  scale_y_continuous(limits = c(0, max(pp_dat$f))) +
  facet_grid(cols = vars(positionCode), scales = 'free_x') +
  labs(
    title = "NHL {frame_time} Player Distribution of Height by Position",
    #subtitle = "Season: {closest_state}",
    x = "",
    y = ""
  ) +
    coord_flip() +
  guides(fill = 'none') +
  theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_text(size = 13, color = 'darkgrey'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 20, color = 'grey'),
          plot.title = element_text(size = 35, color = 'grey',hjust = 0.5),
          plot.subtitle = element_text(size = 15, color = 'grey',hjust = 0.5),
          strip.text = element_blank()
    ) +
  transition_time(
    season_start_yr,
    #transition_length = 1,
    #state_length = 2
  ) #+
  #ease_aes('linear')

ap <- 
animate(
  animated_plot, 
  nframes = pp_dat_lbl$season_start_yr |> unique()  |> length(), 
  fps = 2,
  width = 1261,    # Set width in pixels
  height =  700,
  start_pause = 8,    # Pause at the start
  end_pause = 15       # Pause at the end
)
ap
anim_save(file.path('R', 'analysis',  "player_height_by_year_position_histogram.gif"), 
          animation = ap)

