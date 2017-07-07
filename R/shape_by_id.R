#' @title Link IDs to warterbody shapefiles
#' 
#' @param PERMANENT_match filter using PERMANENT_
#' @param GNIS_ID_match filter using GNIS_ID
#' @param GNIS_NAME_match filter using GNIS_NAME
#' @param REACHCODE_match filer using REACHCODE
#' 
#' @return data table linking ids to filenames which contain them.
#' @import dplyr
#' @import rgdal
#' 
#' @export


waterbody_shape_by_id = function(PERMANENT_match = NULL, GNIS_ID_match = NULL, GNIS_NAME_match = NULL, REACHCODE_match = NULL){
  check_dl_file(system.file("extdata/id_db.csv", package = "nhdtools"), fname = "waterbody_ids.zip")
  id_db = src_sqlite(file.path(local_path(), "unzip", "waterbody_ids.zip", "waterbody_ids.sqlite3"))
  shape = id_db %>%
    tbl("waterbody_ids")
  
  if(!is.null(PERMANENT_match))
    shape = filter(shape, PERMANENT_ %in% PERMANENT_match)
  else if(!is.null(GNIS_ID_match))
    shape = filter(shape, GNIS_ID %in% GNIS_ID_match)
  else if(!is.null(GNIS_NAME_match))
    shape = filter(shape, GNIS_NAME %in% GNIS_NAME_match)
  else if(!is.null(REACHCODE_match))
    shape = filter(shape, GNIS_NAME %in% GNIS_NAME_match)
  shape = collect(shape)
  files = unique(shape$file)
  shapes = list()
  for(i in 1:length(file)){
    check_dl_file(system.file("extdata/nhdh.csv", package="nhdtools"), fname = files[i])
    shapes[[i]] = readOGR(file.path(local_path(), "unzip", files[i], "NHDWaterbody.shp"))
  }
  return(do.call(rbind, shapes))
}


#' @title Link IDs to flowline shapefiles
#' 
#' @param PERMANENT_match filter using PERMANENT_
#' 
#' @return data table linking ids to filenames which contain them.
#' @import dplyr
#' @import rgdal
#' 
#' @export

flowline_shape_by_id = function(PERMANENT_match){
  check_dl_file(system.file("extdata/id_db.csv", package = "nhdtools"), fname = "flowline_ids.zip")
  id_db = src_sqlite(file.path(local_path(), "unzip", "flowline_ids.zip", "flowline_ids.sqlite3"))
  shape = id_db %>%
    tbl("flowline_ids") %>%
    filter(PERMANENT_ %in% PERMANENT_match) %>%
    collect()
  shapes = list()
  files = unique(shape$file)
  for(i in 1:length(file)){
    check_dl_file(system.file("extdata/nhdh.csv", package="nhdtools"), fname = files[i])
    shapes[[i]] = readOGR(file.path(local_path(), "unzip", files[i], "NHDFlowline_projected.shp"))
  }
  return(do.call(rbind, shapes))
}