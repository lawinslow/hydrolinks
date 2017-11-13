#' @title Get local cache path
#'
#' @description
#' Downloaded shapefiles and databases are cached locally.
#' This location can be accessed and reset so that the best
#' available locaiton can be used. For example, a fast SSD
#' or large secondary hard drive may be used.
#'
#' @seealso \code{\link{set_local_path}}
#'
#' @examples
#' #see where cached files are being stored
#' print(local_path())
#'
#'
#' @export

local_path = function(){
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


#' @title Set local files path
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
#' @seealso \code{\link{local_path}}
#'
#' @examples
#' \dontrun{
#'   #set a different cache path
#'   set_cache_path('z:/big_datasets/hydrolinks')
#'
#' }
#'
#' @export
set_local_path = function(path = NULL){
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

