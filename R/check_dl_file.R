#' @title Verify and download data files
#'
#' @description
#' Checks if local data files as defined in master file exist and match MD5 hash. Downloads data if necessary.
#'
#' @param master_file Character path to master file
#' @param fname Character vector of specific file names to check
#' @param md5check boolean
#' @param dest Character path to download destination
#'
#' @import httr
#' @import tools
#' @import rappdirs
#' @import utils

check_dl_file = function(master_file, fname = NULL, md5check = TRUE, dest=local_path()){
  files = read.csv(master_file)
  if(!is.null(fname)){
    files = files[files$filename == fname,]
  }
  apply(files, 1, function(x){
    if(!file.exists(file.path(dest, x["filename"]))){
      print(paste0("Downloading ", x["filename"]))
      r = RETRY("GET", url=x["url"], write_disk(file.path(dest,x["filename"]), overwrite=TRUE), times = 2)
      stop_for_status(r)
      if(!file.exists(file.path(dest, x["filename"]))){
        stop("Data downloaded was incomplete or corrupted - please try again later or submit a bug report.")
      }
      if(md5sum(file.path(dest, x["filename"])) != x["md5"]){
        file.remove(file.path(dest, x["filename"]))
        stop("Data downloaded was incomplete or corrupted - please try again later or submit a bug report.")
      }
      if(file_ext(x["filename"]) == "zip"){
        #dir.create(file.path(dest, "unzip", basename(x["filename"])))
        unzip(zipfile = file.path(dest, x["filename"]), exdir = file.path(dest, "unzip", basename(x["filename"])))
      }
    }
  })
  if(md5check){
    apply(files, 1, function(x){
      if(md5sum(file.path(dest, x["filename"])) != x["md5"]){
        print(paste0("Redownloading ", x["filename"]))
        r = RETRY("GET", url=x["url"], write_disk(file.path(dest, x["filename"]), overwrite=TRUE))
        stop_for_status(r)
        if(!file.exists(file.path(dest, x["filename"]))){
          stop("Data downloaded was incomplete or corrupted - please try again later or submit a bug report.")
        }
        if(md5sum(file.path(dest, x["filename"])) != x["md5"]){
          file.remove(file.path(dest, x["filename"]))
          stop("Data downloaded was incomplete or corrupted - please try again later or submit a bug report.")
        }
        if(file_ext(x["filename"]) == "zip"){
          dir.create(file.path(dest, "unzip", basename(x["filename"])))
          unzip(zipfile = file.path(dest, x["filename"]), exdir = file.path(dest, "unzip", basename(x["filename"])))
        }
      }
    })
  }
}

#' @title Get local files path
#'
#' @description Get path to cached data files
#'
#' @export


local_path = function(){
  path = ""
  pathFile = file.path(rappdirs::user_data_dir(appname = 'hydrolinks', version=packageVersion('hydrolinks')), "path")
  if(!file.exists(pathFile)){
    path = rappdirs::user_data_dir(appname = 'hydrolinks', version=packageVersion('hydrolinks'))
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
#' @description Set location of local data file cache
#'
#' @param path character path to new local files path. If null, path will be reset to default user data directory location.
#'
#' @export
set_local_files_path = function(path = NULL){
  if(!is.null(path)){
    if(!dir.exists(path)){
      dir.create(path, recursive = TRUE)
    }
    write(path, file = file.path(rappdirs::user_data_dir(appname = 'hydrolinks', version=packageVersion('hydrolinks')), "path"))
  }
  else{
    pathFile = file.path(rappdirs::user_data_dir(appname = 'hydrolinks', version=packageVersion('hydrolinks')), "path")
    if(file.exists(pathFile)){
      file.remove(pathFile)
    }
  }
}
