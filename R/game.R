

  # Unnest all <list> columns sequentially
  unnest_all_lists <- function(dat, list_cols  = NULL , names_sep = '_') {
    #dat <- .x2
    list_cols <- 
      if ( is.null(  list_cols )){
         dat |>
          select(where(is.list)) |>
          names()   
      }else{list_cols}

    for (col in list_cols) {
      #col <- list_cols[[1]]
      dat <- 
        if ( is.null(dat[[col]] |>  ncol() )  ){
          dat2 <- 
            dat |> 
            mutate(
              !!col := map(
                .data[[col]],
                ~ if (is.list(.x) && length(.x) == 0) {
                  NULL  # Replace empty lists with an empty tibble
                } else {
                  .x  # Keep the element as is
                }
              )
            )
          
          dat2 |>
            unnest( {{ col }}, keep_empty = TRUE,   names_sep = names_sep)
        } else if (dat[[col]] |>  ncol()  == 1){
          dat |> unnest(col, names_sep = names_sep)
        } else{
          dat |> unnest(col, names_sep = names_sep)
        }

      
      
      
      # dat |> 
      #   mutate(
      #     !!col := map(
      #       .data[[col]],
      #       ~ class(.x)
      #     )
      #   ) |> unnest(!!sym(col)     )

      # dat <- 
      #   dat |>
      #   unnest( {{ col }}, keep_empty = TRUE,   names_sep = names_sep)
      
      
    }
    return(dat)
  }
  


#' game_landing_BASE
#'
#' @param game  A game ID
#' @param pattern 
#' @param url_type 
#'
#' @return
#' @export
#'
#' @examples
#'     x = game_landing_BASE(game = 2023020204)
#'     game_landing_BASE(game = 1988020511)
#'     game_landing_BASE(game = 2023010073)
game_landing_BASE <- function(
    game,
    pattern = 'gamecenter/{game}/landing',
    url_type = 'base'
){
  dat <- get_url_base(pattern = pattern, url_type = url_type)  
  dat_2 <- dat |> nhl_as_tibble()
  tombstone <- dat_2 |> nhl_as_flat_tibble()
  db <- 
    dat_2 |> 
    extract_dataframes() |> 
    map(~{
      #.x <- xx[[1]]
        .x |>
        mutate(gameID = game) |>
        unnest_all_lists() |>
        unnest_all_lists() |>
        unnest_all_lists()
    
    })
  
  c(
    list(tombstone = tombstone),
    db
  )
}

