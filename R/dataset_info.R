#' @title Return all info for requested dataset
#'
#' @description
#' Single point
#'
#' @return
#' List with dataset metadata elements including
#' \describe{
#'   \item{bb_cache_path}{Path to Rdata file with bounding box cache for dataset}
#'   \item{shapefile_name}{Dataset-unique name for region shapefiles}
#'   \item{db_path}{Path to sqlite database for shape-by-ID lookup}
#'   \item{file_index_path}{Path to the dataset's download file index and hash lookup}
#'   \item{id_column}{Dataset-unique ID column name}
#' }
#'
#'
#' @param dataset Name of dataset
#' @param feature_type Feature type (flowline or waterbody)
#'
#'
#' @export

dataset_info = function(dataset, feature_type){

  # a little messy, but single point to clean this up later
  dataset = tolower(dataset)
  feature_type = tolower(feature_type)

  db_name = paste0(dataset, "_", feature_type, "_ids")
  db_path = file.path(local_path(), 'unzip', paste0(db_name, ".zip"), paste0(db_name, ".sqlite3"))

  if(tolower(dataset) == "nhdh"){
    bb_cache_path=system.file('extdata/nhd_bb_cache_projected.Rdata', package='hydrolinks')
    id_column = "PERMANENT_"
  }
  else if(tolower(dataset) == "hydrolakes"){
    bb_cache_path = system.file('extdata/hydrolakes_bb_cache_projected.Rdata', package='hydrolinks')
    id_column = "Hylak_id"
  }
  else if(tolower(dataset) == "nhdplusv2"){
    bb_cache_path=system.file('extdata/nhdplus_waterbody_bb_cache.rdata', package='hydrolinks')
    id_column = "COMID"
  }


  if(dataset == "nhdh" || dataset == "nhdplusv2"){
    if(feature_type == "waterbody"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else{
      shapefile_name = "NHDFlowline_projected.shp"
    }
  }
  else if(dataset == "hydrolakes"){
    shapefile_name = "HydroLAKES_polys_v10.shp"
  }

  file_index_path = system.file(paste0("extdata/", dataset, ".csv"), package = "hydrolinks")

  out = list()
  out$bb_cache_path = bb_cache_path
  out$db_path = db_path
  out$shapefile_name = shapefile_name
  out$file_index_path = file_index_path
  out$id_column = id_column

  return(out)
}
