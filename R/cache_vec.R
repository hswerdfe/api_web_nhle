

source(file.path('R', 'source_here.R'))

here_source('cache.R')
here_source('util.R')
here_source('glue_do.R')
here_source('vectorizer.R')


#here_source_dir()

require(purrr)
require(tibble)
require(dplyr)
require(digest)
require(arrow)
require(hash)









#' cache_vect_clean_cache
#' 
#'  Takes a function that has been proccessed by `cache_vect_maker`  returns the cache associated with that function after some cleaning. all other parameters are optional
#'
#' @param func  A function that was returned from `cache_vect_maker`
#' @param cache_df 
#' @param force_refresh_after_sec_default 
#' @param cache_time_col 
#' @param cache_key_col 
#' @param curr_time 
#'
#' @return
#' @export
#'
#' @examples
cache_vect_clean_cache_internal  <- function(
      cache_df,
      force_refresh_after_sec_default,
      cache_time_col,
      cache_key_col,
      curr_time
){
  keys_to_delete  <- 
    cache_df |>
    filter(
      as.numeric(difftime(curr_time, !!sym(cache_time_col), units = "secs"))  >= force_refresh_after_sec_default
    ) |> 
    pull(!!sym(cache_key_col)) |>
    unique()   
  
  cache_df  |>
    filter( !  (!!sym(cache_key_col)  %in%  keys_to_delete)) 
}






cache_vect_clean_cache <-function(
    func,
    cache_df = get('cache_df',  envir = environment(func)),
    force_refresh_after_sec_default = get('force_refresh_after_sec_default',  envir = environment(func)),
    cache_time_col = get('cache_time_col'  ,  envir = environment(func)),
    cache_key_col = get('cache_key_col'  ,  envir = environment(func)),
    curr_time = Sys.time()
    ){
  #func = get_diamonds
  cache_vect_clean_cache_internal(
    cache_df = cache_df,
    force_refresh_after_sec_default = force_refresh_after_sec_default,
    cache_time_col = cache_time_col,
    cache_key_col = cache_key_col,
    curr_time = curr_time
  )
}


#' cache_vec_save
#' 
#'  Saves the cache of a function to disk
#'
#' @param func a function that has been proccessed by cache_vect_maker
#' @param cache_df 
#' @param hard_cache_file_name 
#'
#' @return
#' @export
#'
#' @examples
#'    cache_vec_save(roster)
cache_vec_save <- function(
    func,
    cache_df = get('cache_df',  envir = environment(func)),
    hard_cache_file_name = get('hard_cache_file_name',   envir = environment(func)),
    cache_record = get('cache_record',  envir = environment(func)),
    func_name_orig = get('func_name_orig',  envir = environment(func)), 
    delete_other = TRUE
){
  glue_message('💾 saving `{nrow(cache_df)}` rows, `{length(cache_record)}` calls to func  `{func_name_orig}` on disk as `{hard_cache_file_name}`.')
  arrow::write_feather(cache_df, hard_cache_file_name)
  

  
  if (delete_other){
    all_files <- here_list_files(pattern = glue('{func_name_orig}____CACHE____.*.feather'), not_found_func = glue_warning,  default = 'data')
    files_to_delet <- all_files[all_files != hard_cache_file_name]
    file.remove(files_to_delet)
  }
}



#' get_hard_caches
#' 
#'  return a dataframe of the `best` cache entries for this fuction includin ones that are from old version of the function with different text
#'
#' @param func a function proccessed by cache_vect_maker
#' @param func_name_orig defaults to getting this value from func
#' @param fil_nm a vector of cache files, defaults to a list of possible caches for this function
#'
#' @return
#' @export
#'
#' @examples
#'    get_hard_caches(roster)
#' 
get_hard_caches <- function(
    func,
    func_name_orig = get('func_name_orig',  envir = environment(func)), 
    fil_nm = here_list_files(pattern = glue('{func_name_orig}____CACHE____.*.feather'), not_found_func = glue_warning,  default = 'data'),
    ...
){
  #func = roster
  
  dfs <- 
    fil_nm |> 
    purrr::set_names() |>
    purrr::map(~{
    arrow::read_feather(.x)
  }) 
  

  key_by_file <-   
    purrr::map2_dfr(dfs, names(dfs), ~{
      .x |>
      summarise(`__cached_on` = min(`__cached_on`), .by = `__args_key`) |> 
      mutate(fn = .y)
    }) |>
    slice_max(order_by = `__cached_on`, by = `__args_key`) |>
    select(-`__cached_on`)
  
  consolodated_cache <-
    key_by_file |>
    pmap_dfr(\(`__args_key`, fn){
      .key <- `__args_key`
      dfs[[fn]] |> 
        filter(`__args_key` == .key)
    })
  consolodated_cache
}


#' set_func_cache
#'
#' sets the cache based on all the caches found for this function, then returns a modified version of the function
#'
#' @param func 
#' @param hard_caches 
#'
#' @return
#' @export
#'
#' @examples
#'   roster = set_func_cache(roster)
#' 
set_func_cache <- function(func, 
                           hard_caches = get_hard_caches(func)
                           ){
  cache_key_col = get('cache_key_col', envir = environment(func))
  cache_time_col = get('cache_time_col', envir = environment(func))
  cache_record  <-  cache_vect_generate_record(hard_caches, cache_key_col, cache_time_col)

  assign(x = 'cache_df', value = hard_caches, envir = environment(func))
  assign(x = 'cache_record', value = cache_record, envir = environment(func))
  func
}




cache_vect_generate_record  <- function(
    cache_df,
    cache_key_col, 
    cache_time_col
){
  
  cache_record  <- 
    cache_df[[cache_key_col]] |> 
    unique()   |>
    purrr::set_names() |>
    map(~{
      .x_df <- 
        cache_df  |>
        filter(!!sym(cache_key_col) == .x) 
      hash::hash(
        cached_on = .x_df |> pull(!!sym(cache_time_col))  |>unique(),
        num_row = nrow(.x_df)
      )
    }) 
  cache_record  <- 
    if (length(cache_record)  == 0){
      hash::hash()   
    }else{
      cache_record
    }
  
  return (cache_record)
}
cache_make_blank <- function(
    cache_key_col = '__args_key',
    cache_time_col   = '__cached_on'  
  ){
  tibble::tibble(
    !!sym(cache_key_col) := character(), 
    !!sym(cache_time_col) := Sys.time()
  )
}

#' expand_vector_lst, expand_vector_dots
#'
#' @param args_vec 
#' @param ...  
#' @param is_valid_combo 
#' @param stringsAsFactors 
#'
#' @return
#' @export
#'
#' @examples
#'    expand_vector_lst(args_vec = list(a=1:100, b=1:100), is_valid_combo = \(a,b){(a+b) %% 2 == 0})
#'    expand_vector_dots(a=1:100, b=1:100, is_valid_combo = \(a,b){(a+b) %% 2 == 0})
expand_vector_lst <- function(   args_vec , is_valid_combo   , stringsAsFactors = FALSE){
  args_vec  |> 
    expand.grid(stringsAsFactors = stringsAsFactors)  |> 
    dplyr::tibble() %>%
    filter(., unlist(purrr::pmap(., is_valid_combo))) 
}
expand_vector_dots <- function( ... , is_valid_combo   , stringsAsFactors = FALSE){
  args_vec <- list(...)
  expand_vector_lst(args_vec = args_vec, is_valid_combo = is_valid_combo  , stringsAsFactors = stringsAsFactors)
}


#' cache_vect_maker
#' 
#'  Makes a cache an vectorized function from the original function.
#'  for the resulting function to be done correctly all arguments should be named arguments
#'
#' @param func : A function that returns a dataframe, and takes an argument called arg_name
#' @param x_vec_nms : Vector of String. names of the parameter to vector over
#' @param all_possible  : a list of vectors. all possible values of the argument, if a  vector it is all the values
#' @param is_valid_combo  : when cross combining multiple vectors with exapand.grid value will decide if this is a valid combo
#' @param binding_func  : function that will take the output from purrr::map which is how this makes the vectorized function and unifies them for return. By default it is purrr::list_rbind
#'
#' @return
#' @export
#'
#' @examples
cache_vect_maker <- function(
    func, 
    x_vec_nms = c(), 
    all_possible = list(),
    all_expansion_val = 'all',
    is_valid_combo  = \(...){TRUE},
    binding_func = purrr::list_rbind,
    force_refresh_after_sec_default =  60 * 60 * 24 * 90,
    force_hard_cache_save_after_sec_default = 100,
    cache_key_col = '__args_key',
    cache_time_col   = '__cached_on'
){

  
  func_name_orig <- as.list(match.call())[[2]] |> as.character() 
  func_txt <- func |> deparse() |> paste(collapse = '\n')
  hard_cache_file_name  <-  here_full_file_name(file_name = glue('{func_name_orig}____CACHE____{digest::digest(func_txt)}.feather'), not_found_func = glue_warning,  default = 'data')
  
  glue_message('💲 making cached version of the function, {func_name_orig}')
  
  
  ###################
  # Create the cache
  cache_df <- 
    if (file.exists(hard_cache_file_name)){
      glue_message('💲 Hard cache file found, {hard_cache_file_name}')
      arrow::read_feather(hard_cache_file_name)
    }else{
      cache_make_blank(cache_key_col, cache_time_col)
    }
  cache_record <- cache_vect_generate_record(cache_df, cache_key_col, cache_time_col)
  
    
  
  
  calc_time_since_save = 0

  initial_args = names(formals(func))
  
  
  
  
  # Ensure names of x_vec_nms  and all_possible  match
  if (  ! all(x_vec_nms |> purrr::map(~{.x %in% names(all_possible)}) |> unlist())){
    glue_stop('When creating cached vectorized version all of x_vec_nms, must have matching names in all_possible')
  }  
  
  
  
  
  ##################
  # Create the wrapper function
  cached_func_df <- function(..., force_refresh_after_sec = force_refresh_after_sec_default, force_hard_cache_save_after_sec  = force_hard_cache_save_after_sec_default){
    
    
    
    ################
    # Get the arguments as a list
    args <- list(...)

    
    
    ####################
    # All arguments need to be named. Makes my life easier
    if (length(args) > 0 ){
      if (is.null(names(args)) | any(nchar(names(args)) == 0)){
            glue_stop('when calling the `cache_vect_maker`version of `{func_name_orig}` all arguments should be named arguments.')  
      }
    }
    
    
    
    args_key_val <- cache_make_key_internal_args(args)
    curr_time <- Sys.time()

    
    
    cache_df  <<- 
      cache_vect_clean_cache_internal(      
        cache_df  = cache_df,
        force_refresh_after_sec_default  = force_refresh_after_sec,
        cache_time_col = cache_time_col,
        cache_key_col = cache_key_col,
        curr_time = curr_time
      )    
    cache_record  <<-  cache_vect_generate_record(cache_df, cache_key_col, cache_time_col)
    
    #############
    # split arguments into vectorized and not vectorized
    #args = list(col = c('E', 'I'), cla='SI2', other_param = 'ZZZ')
    #x_vec_nms  = c('col', 'cla')
    args_vec <- args[x_vec_nms]
    args_other <- args[setdiff(names(args), x_vec_nms)]
    
    
    
    ###########################
    # Expand  when we use the ALL values value!
    args_vec <- 
      purrr::map2(args_vec, names(args_vec), \(.v, .nm){
          if (length(.v) == 1){
            if (.v == all_expansion_val){
              return (all_possible[[.nm]])
            }
          }
          .v
      })
      
    
    
    
    #################
    # This is the base case, for the vectorization
    if (  all(sapply(args_vec, length) == 1)    ){
      if (! is.null(cache_record[[args_key_val]])){
          age_sec <- (curr_time -  cache_record[[args_key_val]][['cached_on']])  |>  as.numeric(units = 'secs')  
          if (age_sec < force_refresh_after_sec & is.finite(age_sec)) {
            glue_message('💰 Returning cached version of function {round(age_sec, 0)} seconds old: {func_name_orig}({args_key_val}).')
            
            ###################
            # looking for the cache result
            cache_result <- 
              cache_df  |> 
              dplyr::filter(!!sym(cache_key_col) == args_key_val)            
            return(cache_result)            
          }else{
            
            glue_message('💸 deleting cache {round(age_sec, 0)} seconds old: {func_name_orig}({args_key_val}).')
            
            cache_df <<- 
              cache_df |>
              dplyr::anti_join(
                cache_result |> 
                  dplyr::select(tidyselect::all_of(cache_key_col)) |> 
                  dplyr::distinct(), by = cache_key_col
              )
            cache_record  <<-  cache_vect_generate_record(cache_df, cache_key_col, cache_time_col)
            
          } 
          
        }# end uncached
        

        ###############################
        # Still base case but uncached!
        glue_message('🧮 Calculating {func_name_orig}({args_key_val}). ')
        args_2 <- c(args_vec, args_other)
        
        result <- do.call(func, args_2)
        
        curr_calc_sec <- (Sys.time()  - curr_time)  |>as.numeric(units = 'secs') 
        calc_time_since_save <<- calc_time_since_save  + curr_calc_sec
        
        
        
        
        result[[cache_key_col]] <- args_key_val
        result[[cache_time_col]] <- curr_time
        
        
        args_vec_dunder <-
          args_vec |>
            purrr::set_names(
              paste0('__',  names(args_vec))
            )
        #print('args_vec_dunder')
        #print(args_vec_dunder)
        result <- bind_cols(result, args_vec_dunder)
        
        
        
        
        result <- 
          result |> 
          relocate(all_of(c(cache_key_col, cache_time_col)))
        

        
        
        cache_df <<- 
          cache_df |> 
          filter(!!sym(cache_key_col) != args_key_val) |>
          dplyr::bind_rows(result)
        cache_record[[args_key_val]] <<- hash::hash(
          cached_on = curr_time,
          num_row = nrow(result)
        )
        #cache_record  <<-  cache_vect_generate_record(cache_df, cache_key_col, cache_time_col)

        
        
        if (calc_time_since_save  > force_hard_cache_save_after_sec){
          cache_vec_save(
              func = NULL, 
              cache_df = cache_df, 
              hard_cache_file_name= hard_cache_file_name,
              cache_record = cache_record, 
              func_name_orig = func_name_orig
              )
          #glue_message('💾 saving `{nrow(cache_df)}` rows, `{length(cache_record)}` calls to func  `{func_name_orig}` on disk as `{hard_cache_file_name}`.')
          #arrow::write_feather(cache_df, hard_cache_file_name)
          calc_time_since_save <<- 0
        }        
        
        
        return(result)
        
      }#  End Base Case
    
    
      
      ##################
      # Expand multiple argument
      valid_args_df <- 
        args_vec  |> 
          expand_vector_lst(is_valid_combo  = is_valid_combo) %>%
          mutate(!!sym(cache_key_col) := unlist(pmap(., \(...){
            .args <- c(list(...), args_other)
            cache_make_key_internal_args(.args)
          })))
      

      
      #################
      #  Which  part is cached already
      cache_found_parts <-   
        cache_df |>
        filter(!!sym(cache_key_col) %in% valid_args_df[[cache_key_col]])
      
      ############
      # Which parts are left over
      valid_args_df_not_found <- 
        valid_args_df |>
        anti_join(cache_found_parts, by = cache_key_col )
    

    
      new_result <- 
        valid_args_df_not_found  |>
        select(-!!sym(cache_key_col))  |>
          purrr:::pmap(\(...){
            curr_vec_args <- list(...)

            curr_args_all <- c(curr_vec_args, args_other)
            result <- do.call('cached_func_df', curr_args_all)
            
            result
          }) |> 
          binding_func()
      if (nrow(cache_found_parts) > 0 ){
        ncalls  <-  cache_found_parts[[cache_key_col]] |>  unique() |> length()
        glue_message('💰 Found cache for `{ncalls}`  calls  with `{nrow(cache_found_parts)}` rows.')  
      }
      
      bound_result <- binding_func(list(cache_found_parts,  new_result))
      return(bound_result)
    }# End Wrapper Function
 return (cached_func_df)
}






# 
# team <- nhl_param_norm_team(team, lang)
# season <- nhl_param_norm_season(season
# ache_vect_maker(
#     func = roster_BASE,
#     x_vec_nms  =  c('team', 'season'),
#     all_possible  = list(
#       team = diamonds$color   |> unique() |> as.character(),
#       season = diamonds$clarity   |> unique() |> as.character()
#     ),
#     force_hard_cache_save_after_sec_default = 1
# )



# 
# 
# get_diamonds_BASE <- function(col, cla,   other_param = 'some param'){
#   Sys.sleep(0.26)
#   diamonds |>
#     filter(color == col) |>
#     filter(clarity  == cla) |>
#     mutate(other_param_name = other_param  )
# }
# get_diamonds <- cache_vect_maker(func = get_diamonds_BASE,
#                                  x_vec_nms  =  c('col', 'cla'),
#                                  all_possible  = list(
#                                    col = diamonds$color   |> unique() |> as.character(),
#                                    cla = diamonds$clarity   |> unique() |> as.character()
#                                  ),
#                                  force_hard_cache_save_after_sec_default = 1
# 
# )           
# 
# 
# get_diamonds(col = 'E', cla= 'SI2', other_param = 'ZZZ')
# get_diamonds(col = 'E', cla= 'SI2', other_param = 'ZZZ')
# get_diamonds(col = 'I', cla= 'SI2', other_param = 'ZZZ')
# get_diamonds(col = c('E','I'), cla= 'SI2', other_param = 'ZZZ')
# get_diamonds(col = c('E','I', 'J'), cla= 'SI2', other_param = 'XXXXXXX')
# get_diamonds(col = c('E','I'), cla= c('all'), other_param = 'ZZZ', force_hard_cache_save_after_sec = -1)                                 
# 
# get_diamonds(col = c('all'), cla= c('all'), other_param = 'ZZZ')                                 
# 
# cache_df <- get('cache_df',  envir = environment(get_diamonds))
# ls(envir = environment(get_diamonds))

