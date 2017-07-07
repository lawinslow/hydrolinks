#' @title Link IDs to warterbody shapefiles
#' 
#' @param PERMANENT_match filter using PERMANENT_
#' @param GNIS_ID_match filter using GNIS_ID
#' @param GNIS_NAME_match filter using GNIS_NAME
#' @param REACHCODE_match filer using REACHCODE
#' 
#' @return data table linking ids to filenames which contain them.
#' @import dplyr
#' 
#' @export


waterbody_shape_by_id = function(PERMANENT_match = NULL, GNIS_ID_match = NULL, GNIS_NAME_match = NULL, REACHCODE_match = NULL){
  check_dl_file("")
  id_db = src_sqlite(file.path(local_path(), "waterbody_ids.sqlite3"))
  shape = id_db %>%
    tbl("waterbody_ids")
  
  if(!is.null(PERMANENT_))
    shape = filter(shape, PERMANENT_ %in% PERMANENT_match)
  else if(!is.null(GNIS_ID))
    shape = filter(shape, GNIS_ID %in% GNIS_ID_match)
  else if(!is.null(GNIS_NAME))
    shape = filter(shape, GNIS_NAME %in% GNIS_NAME_match)
  else if(!is.null(REACHCODE))
    shape = filter(shape, GNIS_NAME %in% GNIS_NAME_match)
  shape = collect(shape)
  return(shape)
}


#' @title Link IDs to flowine shapefiles
#' 
#' @param PERMANENT_match filter using PERMANENT_
#' 
#' @return data table linking ids to filenames which contain them.
#' @import dplyr
#' 
#' @export

flowline_shape_by_id = function(PERMANENT_match){
  check_dl_file()
  id_db = src_sqlite(file.path(local_path(), "flowline_ids.sqlite3"))
  shape = id_db %>%
    tbl("flowline_ids") %>%
    filter(PERMANENT_ %in% PERMANENT_match) %>%
    collect()
  return(shape)
}