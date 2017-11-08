#' @title Return path to all shapefiles
#'
#' @description
#' Returns list of paths to shapefiles for use in custom processing.
#'
#'
#' @param check_dl If TRUE, checks to ensure all files for that dataset have been downloaded.
#' This check takes some time (~30 seconds) to check all files (and much longer to dowload if necessary)
#' @param dataset which dataset to return shapefiles from.
#' 
#' @param feature_type return waterbodies or flowlines. The hydrolakes dataset does not include flowlines.
#'
#'
#' @export
all_shapefiles = function(check_dl=FALSE, dataset = c("nhdh", "hydrolakes", "nhdplusv2"), feature_type = c("waterbody", "flowline")){

  dataset = match.arg(dataset)
  feature_type = match.arg(feature_type)
  dl_file = ""
  id_column = ""
  bbdf = NULL
  bbdf_waterbody = NULL
  if(tolower(dataset) == "nhdh"){
    load(file=system.file('extdata/nhd_bb_cache_projected.Rdata', package='hydrolinks'))
    dl_file = "extdata/nhdh.csv"
    id_column = "PERMANENT_"
    wbd_bb = bbdf
  }
  else if(tolower(dataset) == "hydrolakes"){
    load(file=system.file('extdata/hydrolakes_bb_cache_projected.Rdata', package='hydrolinks'))
    dl_file = "extdata/hydrolakes.csv"
    id_column = "Hylak_id"
    wbd_bb = bbdf
  }
  else if(tolower(dataset) == "nhdplusv2"){
    load(file=system.file('extdata/nhdplus_waterbody_bb_cache.rdata', package='hydrolinks'))
    dl_file = "extdata/nhdplusv2.csv"
    id_column = "COMID"
    wbd_bb = bbdf_waterbody
  }

  if(check_dl){
    check_dl_file(system.file(dl_file, package = "hydrolinks"))
  }
  shapefile_name = ""
  if(tolower(dataset) == "nhdh" || tolower(dataset) == "nhdplusv2"){
    if(feature_type == "waterbody"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else{
      shapefile_name = "NHDFlowline_projected.shp"
    }
  }
  else if(tolower(dataset) == "hydrolakes"){
    shapefile_name = "HydroLAKES_polys_v10_projected.shp"
  }

  files = file.path(local_path(), "unzip", read.csv(system.file(dl_file, package = "hydrolinks"))$filename, shapefile_name)

  return(files)
}
