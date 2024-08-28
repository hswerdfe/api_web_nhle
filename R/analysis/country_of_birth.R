

here::here()  |>
  file.path('R', 'source_here.R') |> 
  source()
here_source_dir()



nhl_country('en')$imageUrl

dat <- nhl_roster_full(team = 'all', season = 'all', lang = 'en') 
  

dat |> count(birthCountry, birthStateProvince_default, birthCity_default, sort = T ) |> filter(is.na(birthCity_default     ))
dat  |>
  summarise(n = n(), .by = c(birthCountry, season))  |>
  mutate(f = n/sum(n), .by = c(season))  |>
  ggplot(aes(x  = season, y = f, fill = birthCountry)) +
  geom_col()

countries <- 
  nhl_country('en')  |> 
  select(id, countryName)  |> 
  distinct() |>
  bind_rows(tibble(id = c('TWN', 'BRN', 'TZA','IDN','BGR', 'UZB'), 
                
                   countryName   =c('Taiwan',  'Brunei',  'Tanzania', 'Indonesia',  'United Kingdom',  'Uzbekistan') ))

p_dat  <- 
  dat |> 
  count(season, birthCountry) |>
  left_join(countries, by = c('birthCountry'  = 'id' ))  |>
  filter(season == max(season)) |> arrange(desc(n)) |>
  mutate(f = n/sum(n), .by = c(season)) 
  
  mutate(countryName = fct_lump(countryName, n = 5, w = n)) |>
  summarise(n = sum(n), .by = c(season, countryName)) |>
  mutate(f = n / sum(n), .by = season  ,  
         season_lbl = fct_reorder(paste0(str_sub(season, 1,4) ,'-',str_sub(season, 7,8)), season)
         ) |>
  mutate(
    countryName  = fct_reorder(countryName, -f, .fun = sum)
  ) |>
  arrange(season_lbl, desc(countryName)) |>
  mutate(
    y_pos = cumsum(f)-(f/2), 
    low_pos = cumsum(f)-f, 
    high_pos = cumsum(f),
    .by = season
  )



peak_data <- 
p_dat  |> 
  slice_max(f,  by = countryName, n = 1)  
  
end_data <- 
p_dat  |> 
  slice_max(season,  by = countryName, n = 1)  

p_dat |> 
    ggplot(aes(x = season, y = f, fill = countryName, color = countryName)) +   
      geom_col(width = 10001) +
      geom_label_repel(
        data = peak_data, 
        mapping = aes(y = y_pos, label = countryName), 
        fill = 'white',
        size = 6,
        nudge_y = 0.05
      ) +
      geom_label_repel(
        data = end_data, 
        mapping = aes(y = y_pos, label = paste0(round(f*100, 0), '%')), 
        fill = 'white',
        size = 6,
        nudge_x = 50000
      )       +
      scale_x_continuous(
        breaks = unique(p_dat$season)[seq(4, length(p_dat$season), by = 10)],
        labels = unique(p_dat$season_lbl)[seq(4, length(p_dat$season_lbl), by =10)]
      ) +
      labs(
        title = 'NHL Players By Country of Birth',
        x  = '',
        y = '',
        caption = 'Source :https://api-web.nhle.com/v1/roster/{team}/{season}'
      ) +
      guides(color = 'none', fill = 'none') + 
      theme_minimal()  +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18, color = 'darkgrey'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 25, hjust = 0.5, color = 'darkgrey'),
        plot.subtitle = element_text(size = 12, hjust = 0.5) ,
        strip.background = element_blank(),
        strip.text = element_blank()
      )  
  
