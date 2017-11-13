#' @title Get local cache path
#'
#' @description
#' Downloaded shapefiles and databases are cached locally.
#' This location can be accessed and reset so that the best
#' available locaiton can be used. For example, a fast SSD
#' or large secondary hard drive may be used.
#'
#' @seealso \code{\link{cache_set_dir}}
#'
#' @examples
#' #see where cached files are being stored
#' print(cache_get_dir())
#'
#'
#' @export

cache_get_dir = function(){
  path = ""
  pathFile = file.path(rappdirs::user_data_dir(appname = 'hydrolinks'), "path")
  if(!file.exists(pathFile)){
    path = rappdirs::user_data_dir(appname = 'hydrolinks')
  }
  else{
    path = readChar(pathFile, file.info(pathFile)$size)
    path = gsub("[\r\n]", "", path)
  }
  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }
  return(path)
}


#' @title Set local cache path
#'
#' @description
#' Set location of local data file cache. If the directory does not exist, it
#' will be created recursively. If no custom path is set, the
#' default user data directory for the package will be used. See
#' \code{\link[rappdirs]{user_data_dir}} for details.
#'
#' @param path
#' character path to new local files path. If null, path will be
#' reset to default user data directory location.
#'
#' @seealso \code{\link{cache_get_dir}}
#'
#' @examples
#' \dontrun{
#'   #set a different cache path
#'   set_cache_path('z:/big_datasets/hydrolinks')
#'
#' }
#'
#' @export
cache_set_dir = function(path = NULL){
  app_dir = rappdirs::user_data_dir(appname = 'hydrolinks')
  if(!dir.exists(app_dir)){
    dir.create(app_dir, recursive = TRUE)
  }
  if(!is.null(path)){
    if(!dir.exists(path)){
      dir.create(path, recursive = TRUE)
    }
    write(path, file = file.path(rappdirs::user_data_dir(appname = 'hydrolinks'), "path"))
  }
  else{
    pathFile = file.path(rappdirs::user_data_dir(appname = 'hydrolinks'), "path")
    if(file.exists(pathFile)){
      file.remove(pathFile)
    }
  }
}


#' @title Remove locally cached files
#'
#'
#' @export
cache_clear = function(){
  cache = cache_get_dir()
  unlink(cache, recursive = TRUE)
}

#' @title Get local file cache info
#'
#' @return prints locally cached files and their sizes.
#' @export
cache_info = function(){
  cat("<hydrolinks cached files>", sep = "\n")
  cat(sprintf("  directory: %s\n", cache_get_dir()), sep = "\n")
  x = list.files(cache_get_dir())
  for (i in seq_along(x)) {
    info = file.info(file.path(cache_get_dir(), x[i]))
    if(!info$isdir){
      cat(paste0("  file: ", sub(cache_get_dir(), "", basename(x[i]))), sep = "\n")
      size = file.info(file.path(cache_get_dir(), x[i]))$size / 1024 / 1024
    }
    else{
      cat(paste0("  directory: ", sub(cache_get_dir(), "", basename(x[i]))), sep = "\n")
      size = sum(file.info(list.files(file.path(cache_get_dir(), x[i]), all.files = TRUE, recursive = TRUE, full.names = TRUE))$size) / 1024 / 1024
    }
    cat(paste0("  size: ", size, " mb"), sep = "\n")
    cat("\n")
  }
}