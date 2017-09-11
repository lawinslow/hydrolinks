#' @title Link IDs to warterbody shapefiles
#'
#' @description Get shapefiles containing waterbodies with specified IDs. If one argument is provided, no
#' other arguments will be used to filter. Arguments are checked in order: PERMANENT_match, GNIS_ID_match,
#' GNIS_NAME_match, REACHCODE_match.
#'
#' @param feature_type name of feature layer to match. The hydrolakes dataset does not include a flowline layer.
#' @param dataset name of dataset to use for matching.
#' @param match_column index containing match ids. Columns indexed by dataset:
#' \tabular{lll}{
#' nhdh \tab nhdplusv2 \tab hydrolakes \cr
#' PERMANENT_ \tab COMID \tab Hylak_id \cr
#' GNIS_ID \tab GNIS_ID \tab Lake_name \cr
#' GNIS_NAME \tab GNIS_NAME \cr
#' REACHCODE \tab REACHCODE
#' }
#' @param match_id ids of features to be matched.
#'
#' @return simple features object containing polygons with associated IDs.
#' @import dplyr
#' @import sf
#' @import RSQLite
#'
#' @export

get_shape_by_id = function(feature_type = c("flowline", "waterbody"), dataset = c("nhdh", "nhdplusv2", "hydrolakes"), match_column, match_id){
  feature_type = match.arg(feature_type)
  dataset = match.arg(dataset)

  #ID cache files alway
  db_name = paste0(dataset, "_", feature_type, "_ids")

  #check_dl_file(system.file("extdata/shape_id_cache.csv", package = "hydrolinks"), fname = paste0(db_name, ".zip"))

  con = dbConnect(RSQLite::SQLite(), file.path(local_path(), 'unzip', paste0(db_name, ".zip"), paste0(db_name, ".sqlite3")))

  sql = paste0('SELECT * from id_lookup where ', match_column, ' IN (', paste(match_id, collapse = ','), ')')

  shape = dbGetQuery(con, sql)

  files = unique(shape$file)

  dbDisconnect(con)

  shapes = list()

  shapefile_name = ""
  if(dataset == "nhdh" || dataset == "nhdplusv2"){
    if(feature_type == "waterbody"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else{
      shapefile_name = "NHDFlowline_projected.shp"
    }
  }
  else if(dataset == "hydrolakes"){
    shapefile_name = "HydroLAKES_polys_v10_projected.shp"
  }

  if(length(files) > 0){
    for(i in 1:length(files)){
      check_dl_file(system.file(paste0("extdata/", dataset, ".csv"), package = "hydrolinks"), fname = files[i])
      shapefile = st_read(file.path(local_path(), "unzip", files[i], shapefile_name), stringsAsFactors = FALSE)
      features = shapefile[shapefile[,match_column, drop = TRUE] %in% match_id,]
      shapes[[i]] = features
    }
  }
  if(length(shapes) > 1)
    return(do.call(rbind, shapes))
  else
    return(shapes[[1]])
}

