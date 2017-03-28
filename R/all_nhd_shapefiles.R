#' @title Return path to all NHD shapefiles
#'
#' @description
#' Returns list of paths to NHD shapefiles for use in
#' custom processing.
#'
#'
#'
#' @export
all_nhd_shapefiles = function(){

  return(Sys.glob(file.path(local_storage(), 'HU4', 'Shape_unzip', '*.zip', 'Shape', '*.shp')))

}
