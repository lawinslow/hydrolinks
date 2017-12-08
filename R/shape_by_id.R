#' @title Link IDs to warterbody shapefiles
#'
#' @description Get shapefiles containing waterbodies with specified IDs. If one argument is provided, no
#' other arguments will be used to filter. Arguments are checked in order: PERMANENT_match, GNIS_ID_match,
#' GNIS_NAME_match, REACHCODE_match.
#'
#' @param feature_type name of feature layer to match. The hydrolakes dataset does not include a flowline layer.
#' @param dataset name of dataset to use for matching.
#' @param match_column index containing match ids. Defaults to dataset ID column.
#' Columns indexed by dataset:
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
#'
#' @examples
#' \dontrun{
#'
#' library(sf)
#' shp = get_shape_by_id('143249470', feature_type = 'waterbody', dataset='nhdh')
#' plot(st_geometry(shp), col='blue')
#'
#' }
#' @export
get_shape_by_id = function(match_id, feature_type = c("flowline", "waterbody"), dataset = c("nhdh", "nhdplusv2", "hydrolakes"), match_column){
  feature_type = match.arg(feature_type)
  dataset = match.arg(dataset)

  dinfo = dataset_info(dataset, feature_type)

  #assume id_column is match column if not specified
  if(missing(match_column)){
    match_column = dinfo$id_column
  }

  #ID cache files alway
  db_name = paste0(dataset, "_", feature_type, "_ids")

  #lookup table file info is always in the same file
  check_dl_file(system.file('extdata/shape_id_cache.csv', package='hydrolinks'), fname = paste0(db_name, ".zip"))

  con = dbConnect(RSQLite::SQLite(), file.path(cache_get_dir(), 'unzip', paste0(db_name, ".zip"), paste0(db_name, ".sqlite3")))

  sql = paste0("SELECT * from id_lookup where ", match_column, " IN ('", paste(match_id, collapse = "','"), "')")

  shape = dbGetQuery(con, sql)

  files = unique(shape$file)

  dbDisconnect(con)

  shapes = list()


  if(length(files) > 0){
    for(i in 1:length(files)){
      check_dl_file(dinfo$file_index_path, fname = files[i])
      shapefile = st_read(file.path(cache_get_dir(), "unzip", files[i], dinfo$shapefile_name))
      features = shapefile[shapefile[,match_column, drop = TRUE] %in% match_id,]
      shapes[[i]] = features
    }
  }
  if(length(shapes) > 1)
    return(do.call(rbind, shapes))
  else
    return(shapes[[1]])
}

