

source(file.path('R', 'source_here.R'))
here_source('glue_do.R')


library(tidyverse)
require(purrr)
require(tibble)
require(stringi)
require(stringdist)
require(dplyr)
require(tidyr)


#' nhl_as_tibble
#'  
#'  converts nested lists back to tibbles.
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
nhl_as_tibble <- function(x){
  
  x |> purrr::map(~{
    if(inherits(.x, 'data.frame')){
      
      # Data frames are tibbles!
      df <- tibble::as_tibble(.x)
      
      
      df |> colnames() |> purrr::map_dfc(~{
        if ( inherits(df[[.x]], 'data.frame') ){
          
          #########################
          # if the column is it self a dataframe pull it out
          df[[.x]] |> dplyr::rename_all(\(.nm){paste(.x, .nm, sep = '_')})
        }else if( inherits(df[[.x]], 'list') ){
          
          #########################
          #TODO : Maybe do something different with lists (that are a type of column) generically, maybe not
          if (length(df[[.x]]) == 0){
            tibble::tibble()
          }else{
            df[.x]  
          }
          
        }else{
          df[.x]
        }
      })
      
    }else if(inherits(.x, 'list')){
      if (length(.x) == 0){
        
        # empty lists are empty tibbles
        tibble::tibble()
      }else{
        
        # You might have a list of dataframes to make tibbles
        # recursive call
        nhl_as_tibble(x = .x)
      }      
      
    }else{
      
      # not a list or dataframe return it
      .x
    }
  }) 
}




#' nhl_pluck
#'
#'  pluck then tibble
#'
#' @param x 
#' @param nm 
#'
#' @return
#' @export
#'
#' @examples
nhl_pluck <- function(x, nm = 'data'){
  x |> 
    purrr::pluck(nm) |>
    tibble::as_tibble()
}







#' nhl_list_bind
#' 
#'  
#'
#' @param x 
#' @param var_nm 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' v <- iris$Species |> unique()  |> as.character()
#' v |>   
#' purrr::set_names(v) |>
#' purrr::map(~{
#'     iris |>
#'       dplyr::filter(Species == .x)
#'   }) |>
#'   nhl_list_bind('xxx')
#'   
#'   
nhl_list_bind <- function(x, var_nm){
  nm <- names(x)
  purrr::map2(x, nm , \(.v, .nm){
    .v[[var_nm]] <- .nm
    .v
  }) |>
    purrr::list_rbind()
}





#' nhl_str_base_clean
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_str_base_clean('Montréal')
#' nhl_str_base_clean('MontrEal')
#' 
nhl_str_base_clean <- function(x){
  str_squish( stringi::stri_trans_general(str_to_upper(x), "Latin-ASCII"))
}








#' nhl_search_df
#' 
#'  search a dataframe for a single thing and return the single thing if you can or throw an error
#'
#' @param df 
#' @param x 
#' @param rot_cols 
#' @param key 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_search_df(df = nhl_teams('en'), x = 'toronto', rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('en'), x = 'Maple Leafs', rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('en'), x = 'maple      leaf      ', rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('en'), x = 'hartford', rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('en'), x = 'montreal', rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('en'), x = 'canadiens', rot_cols = c('fullName', 'id', 'Tricode'), key = 'rawTricode')
#'   nhl_search_df(df = nhl_teams('fr'), x = 'CGS', rot_cols = c('fullName', 'id', 'tricode'), key = 'rawTricode')
#'   
nhl_search_df <- function(df, 
                          x,
                          rot_cols,
                          key
){
  
  #df <- nhl_teams()
  
  #rot_cols <- c('fullName', 'id', 'rawTricode')
  #key <- 'triCode'
  
  
  # Ensure the thing you look for is a character
  x <- as.character(x)
  
  
  # Rotate the keys
  df_k <-
    df |> 
    dplyr::mutate(dplyr::across(tidyselect::all_of(rot_cols), as.character)) |>
    dplyr::select(tidyselect::all_of(c(key, rot_cols))) |>
    tidyr::pivot_longer(cols = tidyselect::all_of(rot_cols), values_drop_na = TRUE) 
  
  
  ########################
  # Find and exact match, return it if found
  df_f <- 
    df_k |>
    dplyr::filter( !!sym(key) == x | value == x  )
  if ((df_f[[key]] |> unique() |> length()) == 1){
    return (  df_f[[key]] |> first() )
  } 
  
  
  ########################
  # Find a cleaned exact match match, return result if found
  df_f <- 
    df_k |>
    dplyr::filter(nhl_str_base_clean(value) == nhl_str_base_clean(x))
  if ((df_f[[key]] |> unique() |> length()) == 1){
    return (  df_f[[key]] |> first() )
  }   
  
  
  
  ########################
  # Find a subset match , return result if found
  df_f <- 
    df_k |>
    dplyr::filter( stringr::str_detect(nhl_str_base_clean(value) , nhl_str_base_clean(x)) )
  if ((df_f[[key]] |> unique() |> length()) == 1){
    return (  df_f[[key]] |> first() )
  }    
  
  
  
  
  
  ########################
  # 
  #
  # So at this point we do not match and return anything, we just make an error message
  #
  #
  #########################
  
  
  
  if ((df_f[[key]] |> unique() |> length()) > 1){
    options <- 
      df_f |> 
      dplry::mutate(
        sim = stringdist::stringsim( a = nhl_str_base_clean(value), b = nhl_str_base_clean(x) ,  method = 'lcs' )
      ) |> dplry::arrange(dplry::desc(sim)) |>
      dplry::mutate(lbl = paste(!!sym(key), value, sep = '=')) |> 
      dplyr::pull(lbl) |>
      unique() |> 
      paste0(collapse = '; ') |> 
      stringr::str_trunc(200)
    
    
    glue_stop('🎯️ Normalizing `{x} `found  `{nrow(df_f |> dplyr::distinct())}` options {options}. Try being more specific!')
  }
  if ((df_f[[key]] |> unique() |> length()) == 0){
    df_sim <- 
      df_k |> dplyr::mutate(
        sim = stringdist::stringsim( a = nhl_str_base_clean(value), b = nhl_str_base_clean(x) ,  method = 'lcs')
      ) |> 
      dplyr::arrange(dplyr::desc(sim)) |>
      dplyr::distinct()
    
    
    options <- 
      df_sim |> 
      dplyr::mutate(lbl = paste(!!sym(key), value, sep = '=')) |> 
      dplyr::pull(lbl) |> 
      paste0(collapse = '; ') |> 
      stringr::str_trunc(200)
    
    
    glue_stop('🎯 Normalizing `{x}`found NO options, some options are {options}. Try being more specific!')
  }
}



#' unnamed_element_i
#' 
#'  returns the index of the ith unnamed element from a named list
#'
#' @param lst : a list where some of the values may or may not have names
#' @param i : Default of 1 : will return the ith (first) item without a name
#'
#' @return
#' @export
#'
#' @examples
#'    unnamed_element_i(list(a=4, b=3)) #returns NULL
#'    unnamed_element_i(list(a=4, 3))
#' 
unnamed_element_i <- function(lst, i = 1){
  element_names <- names(lst)
  unnamed_indices <- which(is.na(element_names) | element_names == '') 
  if (length(unnamed_indices)   == 0) {
    return( NULL)
  }
  unnamed_indices[[i]]
}


