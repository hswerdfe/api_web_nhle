

source(file.path('R', 'source_here.R'))
here_source('util.R')
here_source('glue_do.R')


library(purrr)
library(dplyr)


download_data <- function(
    func,
    all_args,
    file_name_prefix,
    dir = file.path('data','download'),
    
    file_name_ext = 'feather',
    cache_key_col = '__args_key',
    cache_time_col   = '__cached_on',
    force_refresh_after_sec =  60 * 60 * 24 * 90
){
  
  
  ####################
  # Read in existing database
  db <- read_db(path  = dir, file_pattern = paste0(file_name_prefix, '(.*)\\.', file_name_ext))
  

  
  ###################
  # generate a key column for the arguments
  all_args_2  <- 
    all_args  %>%
    mutate(!!sym(cache_key_col) := unlist(pmap(., \(...){
      .args <- list(...)
      cache_make_key_internal_args(.args)
    })))
  
  
  ####################
  # we need the arguments that are either missing or old
  all_args_3 <- 
    if (length(db) == 0){
      all_args_2  |> 
        #select(-!!sym(cache_key_col)) |> 
        distinct()
    }else{
      previous_call <- 
        map_dfr(db, ~{
          .x |> 
            select(!!sym(cache_time_col), !!sym(cache_key_col))  |>
            mutate(
              age_sec =   as.numeric(Sys.time()- !!sym(cache_time_col), units = 'secs')  
            )
        })
      
      not_yet_called <- 
        all_args_2 |> 
        anti_join(previous_call, by = cache_key_col)
      
      called_but_old <- 
        previous_call |> 
          filter(age_sec > force_refresh_after_sec) |>
          select ( - age_sec, - !!sym(cache_time_col))  |>
          inner_join(all_args_2, by = cache_key_col)
      
      
      bind_rows(
        not_yet_called, 
        called_but_old
      )  |>
        distinct()
    }
  
  db  <- 
    db |>
      map(~{
        .x |> anti_join(all_args_3, by = cache_key_col)
      })
  
  func_name_orig <- as.list(match.call())[[2]] |> as.character() 
  #func_txt <- func |> deparse() |> paste(collapse = '\n')
  glue_message('Downloading Data for `{func_name_orig}` looking for `{nrow(all_args_2)}` calls. uncached calls are at `{nrow(all_args_3)}.`')
  
  ############################
  # Iterate over all_args and update lst
  for (i in 1:nrow(all_args_3)) {
    #i=1
    arg <- all_args_3  |>
      slice(i) |>
      as.list()
    arg[[cache_key_col]] <- NULL
    
    glue_message('{as.character(Sys.time())}, {i}/{nrow(all_args_3)}, {lst_2_str(arg)}')
    #########################
    # Call the download function
    result <- tryCatch({
      do.call(func, arg)
    }, error = function(e) { 
      glue_message("Error: {e$message} on argument:, {lst_2_str(arg)}")
      return(NULL)
    })
    
    
    if ( ! is.null(result)) {
      result <-
        if  (inherits(result,  'data.frame')){
          list(result = result)
        }else{
          result
        }
      result <- 
        map(result, \(.v){
          .v[[cache_key_col]] <- cache_make_key_internal_args(arg)
          .v[[cache_time_col]]  <- Sys.time()
          .v
        })  
      
      db <- map2(
        result, 
        names(result), 
        \(.v, .nm){
          bind_rows(.v, db[[.nm]])
        })
    }
    
    # Save intermediate results every 5 iterations
    if (i %% 50 == 0) {
      glue_message('{as.character(Sys.time())}, Saving to file {file_name_prefix}...{file_name_ext}')
      save_db(db, dir  = dir, base_name = paste0(file_name_prefix, '{data_type}.', file_name_ext))
    }
  }
  
  # Save final results
  save_db(db, dir = dir, base_name = paste0(file_name_prefix, '{data_type}.', file_name_ext))
  
  return(db)
}




