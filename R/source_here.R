
require(here)
require(purrr)
require(stringr)
source(file.path(here::here(), 'R', 'glue_do.R'))

G_DIRS <- c(here::here(), 
            file.path(here::here(),'R'), 
            file.path(here::here(),'data'), 
            getwd()
)
G_DIRS_SUB <- list.dirs(path = G_DIRS, full.names = TRUE, recursive = TRUE)
G_DIRS_SUB  <- G_DIRS_SUB[!str_detect(G_DIRS_SUB, '/\\.') ]  |> unique()


#' here_full_file_name
#'
#'  Find where a file is returns full file name and path to the file specified
#'
#' @param file_name 
#' @param dirs defaults G_DIRS
#'
#' @return string, full file name and path to the file specified.
#' @export
#'
#'       here_full_file_name(file_name = 'NHL_CACHE_FILE_NAME.RDS' )
#' 
#' 
here_full_file_name <- function(file_name, 
                                dirs = G_DIRS, 
                                not_found_func = glue_stop,   
                                recursive = FALSE,
                                default = ''
){
  
  for (dir in dirs) {
    #dir <- 'C:/Users/swerdfeh/projects/api_web_nhle/data'
    #file_name = 'roster_BASE____CACHE____.*.feather'
    full_file_name <- list.files(path = dir, pattern = file_name, recursive = recursive, full.names = TRUE)
    
    if (length(full_file_name) == 1){
      return(full_file_name)
    }
    
    if (length(full_file_name) > 1){
      glue_stop('error looking for file, `{file_name}` found `{length(full_file_name)}` copies. in{dir}')
    }
  } 
  not_found_func('error looking for file, `{file_name}` found no copies in any of `{paste0(dirs, collapse  = "; ")}`')
  for (dir in dirs) { 
    if (
      dir  |>  
        str_replace_all("\\\\", "/") |>
        normalizePath(winslash = '/')  |>
        str_split_1('/')  |>
        last()   == default
    ){
      return(file.path(dir, file_name))
    }
  }
  return(file_name)
}


#' here_list_files
#' 
#'  return a vector of files that match the pattern
#'
#' @param pattern 
#' @param dirs 
#' @param not_found_func 
#' @param recursive 
#' @param default 
#'
#' @return
#' @export
#'
#' @examples
here_list_files <- function(
    pattern, 
    dirs = G_DIRS_SUB, 
    not_found_func = glue_stop,   
    recursive = FALSE,
    default = ''
){
  dirs |> 
    purrr::map(~{
      list.files(path = .x, pattern = pattern, recursive = recursive, full.names = TRUE)
    }) |> unlist()
}


#' here_xx
#' 
#' 
#' here_do , here_source looking the here::here() locations and some related location and try to perform .func
#'
#' @param file_name string file name
#' @param dirs directories to look for 
#' @param .func Default is source function
#'
#' @return
#' @export
#'
#' @examples
here_do <- function(file_name , 
                    .func , 
                    dirs = G_DIRS_SUB 
){
  
  file_found <- FALSE
  x <- NA
  for (dir in dirs) {
    full_file_name <- file.path(dir, file_name)
    if (file.exists(full_file_name)){
      x <- full_file_name |> .func()
      file_found <- TRUE
      break
    }
  }
  if ( ! file_found ){
    glue_stop('Unable to find file `{file_name}` looked in 🔍 dirs 📁= {dirs |> paste0(collapse = "; ")}')
  }
  x
}




#' read_feather_here
#'
#' @param file   a file name
#' @param ... passed to here_do
#'
#' @return
#' @export
#'
#' @examples
read_feather_here <- function(file, ...){
  here_do(file_name = file, .func = arrow::read_feather, ...)
}


#' here_source
#'
#' @param file_name 
#' @param ... 
#' @param .func 
#'
#' @return
#' @export
#'
#' @examples
#'     here_source('cache.R')
#'     here_source('not_a_file_123456_switch_654321_file_a_not.R')
#'    here_source('cache_vec.R')
here_source <- function(file_name, ... , .func = source){
  here_do(file_name = file_name, ... , .func = .func)
}









#' here_source_dir
#'
#' @param dir 
#' @param pattern 
#'
#' @return
#' @export
#'
#' @examples
here_source_dir <- function(dir = 'R', pattern = '^.*\\.R$'){
  here::here() |> 
    file.path(dir) |> 
    list.files(pattern = pattern, full.names = TRUE) |>
    purrr::map(source)
}



