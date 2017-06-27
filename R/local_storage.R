pkg_env = new.env()
pkg_env$local_storage = 'Z:/big_datasets/NHD'

#' @title Path to local NHD storage
#'
#' @description
#' Returns internal cache path to local NHD storage
#' location. Can be set by \link{set_local_storage}.
#'
#'
#' @export
local_storage = function(){
  return(pkg_env$local_storage)
}

#' @title Set path to local NHD storage
#'
#' @description
#' Sets internal cache path to local NHD storage
#' location. Can be retrieved by \link{local_storage}.
#'
#' @param storage_path Character value of new
#' local storage path
#'
#' @export
set_local_storage = function(storage_path){
  pkg_env$local_storage = storage_path
}
