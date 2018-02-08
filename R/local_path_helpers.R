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
#'
#' \dontshow{cache_set_dir(temppath=TRUE)}
#'
#' print(cache_get_dir())
#'
#'
#' @export
cache_get_dir = function(){

  path = ""
  #if we are using temp directory, then get out early
  # this is for CRAN so we don't create user path directories
  if(cache_options$usetemp){

    path = file.path(tempdir(), 'hydrolinks_cache')
  }else{

    #If we aren't using a temp directory, us rappdirs or custom path
    pathFile = file.path(rappdirs::user_data_dir(appname = 'hydrolinks'), "path")
    if(!file.exists(pathFile)){
      path = rappdirs::user_data_dir(appname = 'hydrolinks')
    }
    else{
      path = readChar(pathFile, file.info(pathFile)$size)
      path = gsub("[\r\n]", "", path)
    }

  }

  #create the cache directory if it doesn't exist
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
#' Character path to new local files path. If null, path will be
#' reset to default user data directory location.
#' @param temppath
#' Boolean flag indicating if the default R temp directory should
#' be used instead of a custom or user-workspace area. Warning:
#' This setting will not persist between R sessions and the
#' temp directory is cleared when R is closed. Using temp will result
#' in frequent file downloads and extremely slow performance
#'
#' @seealso \code{\link{cache_get_dir}}
#'
#' @examples
#' \dontrun{
#'   #set a different cache path
#'   set_cache_path('z:/big_datasets/hydrolinks')
#' }
#'
#' @export
cache_set_dir = function(path = NULL, temppath=FALSE){

  #two paths here.
  # 1.Tempfile path (do nothing other than set cache_options)
  # 2.!temp path, then rappdirs or custom cache directory (Do a bunch of stuff)

  cache_options$usetemp = temppath

  if(!temppath){
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
}


#' @title Remove locally cached files
#'
#' @description deletes the currently set cached directory found at \link{cache_get_dir}.
#'
#' @export
cache_clear = function(){
  cache = cache_get_dir()
  unlink(cache, recursive = TRUE)
}

#' @title Get local file cache info
#'
#' @description
#' Returns info on all locally cached files stored at the current cache.
#' By default, prints a summary of cache info including total size.
#'
#' @return Returns a data.frame that has the columns \code{file, type, size.MB}.
#' @examples
#' \dontshow{cache_set_dir(temppath=TRUE)}
#' cache_info()
#'
#' @export
cache_info = function(){
  #cat("<hydrolinks cached files>", sep = "\n")
  #cat(sprintf("  directory: %s\n", cache_get_dir()), sep = "\n")

  x = list.files(cache_get_dir(), recursive = TRUE, full.names = TRUE)

  if(length(x) == 0){
    return(structure(data.frame(), class=c('hydrolinks_cache_info', 'data.frame')))

  }else{
    return(structure(do.call(rbind.data.frame, lapply(x, file_info_)),
                     class=c('hydrolinks_cache_info', 'data.frame')))
  }
}


file_info_ <- function(x) {
  fs <- file.size(x)
  list(file = x,
       type = tools::file_ext(x),
       `size MB` = if (!is.na(fs)) round(fs/10e6,3) else NA
  )
}


#' @export
print.hydrolinks_cache_info <- function(x, ...) {
  cat("<landsat cached files>\n", sep = "")
  cat(sprintf(" directory: %s\n", cache_get_dir()), sep = "")

  if(nrow(x) < 1){
    cat(sprintf("  total size: %g MB\n", 0), sep = "")
    cat(sprintf("  number of files:%g\n", 0), sep = "")
  }else{
    cat(sprintf("  total size: %g MB\n", sum(sum(x$size.MB), na.rm=TRUE)), sep = "")
    cat(sprintf("  number of files:%g\n", nrow(x)), sep = "")
    for (i in 1:min(nrow(x),10)) {
      cat(paste0("    size: ", x$size.MB[i], " mb"), sep = "")
      cat(paste0("    file: ", sub(cache_get_dir(), "", x$file[i])), sep = "")
      cat("\n")
    }
  }
}
