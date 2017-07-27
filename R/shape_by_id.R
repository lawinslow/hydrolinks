#' @title Link IDs to warterbody shapefiles
#'
#' @description Get shapefiles containing waterbodies with specified IDs. If one argument is provided, no
#' other arguments will be used to filter. Arguments are checked in order: PERMANENT_match, GNIS_ID_match,
#' GNIS_NAME_match, REACHCODE_match.
#'
#' @param PERMANENT_match filter using PERMANENT_
#' @param GNIS_ID_match filter using GNIS_ID
#' @param GNIS_NAME_match filter using GNIS_NAME
#' @param REACHCODE_match filer using REACHCODE
#'
#' @return SpatialPointsDataFrame containing polygons with associated IDs.
#' @import dplyr
#' @import rgdal
#'
#' @export


waterbody_shape_by_id = function(PERMANENT_match = NULL, GNIS_ID_match = NULL, GNIS_NAME_match = NULL, REACHCODE_match = NULL){
  check_dl_file(system.file("extdata/id_db.csv", package = "hydrolinks"), fname = "waterbody_ids.zip")
  id_db = src_sqlite(file.path(local_path(), "unzip", "waterbody_ids.zip", "waterbody_ids.sqlite3"))
  shape = id_db %>%
    tbl("waterbody_ids")

  PERMANENT_ = GNIS_ID = GNIS_NAME = REACHCODE = NULL

  if(!is.null(PERMANENT_match))
    shape = filter(shape, PERMANENT_ %in% PERMANENT_match)
  else if(!is.null(GNIS_ID_match))
    shape = filter(shape, GNIS_ID %in% GNIS_ID_match)
  else if(!is.null(GNIS_NAME_match))
    shape = filter(shape, GNIS_NAME %in% GNIS_NAME_match)
  else if(!is.null(REACHCODE_match))
    shape = filter(shape, REACHCODE %in% GNIS_NAME_match)
  shape = collect(shape)
  files = unique(shape$file)
  shapes = list()
  if(length(files) > 0){
    for(i in 1:length(files)){
      check_dl_file(system.file("extdata/nhdh.csv", package="hydrolinks"), fname = files[i])
      shapes[[i]] = readOGR(file.path(local_path(), "unzip", files[i], "NHDWaterbody.shp"))
    }
    if(length(shapes) > 1)
      return(do.call(rbind, shapes))
    else
      return(shapes[[1]])
  }
}


#' @title Link IDs to flowline shapefiles
#'
#' @description Get shapefiles containing flowlines with specified IDs.
#'
#' @param PERMANENT_match filter using PERMANENT_
#'
#' @return SpatialPointsDataFrame containing polygons with associated IDs.
#' @import dplyr
#' @import rgdal
#' @import dbplyr
#'
#' @export

flowline_shape_by_id = function(PERMANENT_match){
  check_dl_file(system.file("extdata/id_db.csv", package = "hydrolinks"), fname = "flowline_ids.zip")
  id_db = src_sqlite(file.path(local_path(), "unzip", "flowline_ids.zip", "flowline_ids.sqlite3"))

  PERMANENT_ = NULL

  shape = id_db %>%
    tbl("flowline_ids") %>%
    filter(PERMANENT_ %in% PERMANENT_match) %>%
    collect()
  shapes = list()
  files = unique(shape$file)
  if(length(files) > 0){
    for(i in 1:length(files)){
      check_dl_file(system.file("extdata/nhdh.csv", package="hydrolinks"), fname = files[i])
      shapes[[i]] = readOGR(file.path(local_path(), "unzip", files[i], "NHDFlowline_projected.shp"))
    }
    if(length(shapes) > 1)
      return(do.call(rbind, shapes))
    else
      return(shapes[[1]])
  }
  if(length(shapes) > 1)
    return(do.call(rbind, shapes))
  else
    return(shapes[[1]])
}

