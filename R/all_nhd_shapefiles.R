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
  return(read.csv(system.file("extdata/nhdh.csv", package = "hydrolinks"))$filename)
}
