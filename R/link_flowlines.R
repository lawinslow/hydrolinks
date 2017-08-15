#' @title Link geopoints to flowlines
#'
#' @description Link geopoints to flowlines in the NHD
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param max_dist numeric maximum line snapping distance in meters
#' @param dataset Character name of dataset to link against. Can be either "nhdh" or "nhdplus"
#'
#' @return flowline permanent ids
#'
#' @import rgdal
#' @import sp
#' @import maptools
#' @import rgeos
#' @import dplyr
#'
#' @export

link_to_flowlines = function(lats, lons, ids, max_dist = 100, dataset = "nhdh"){
  dl_file = ""
  id_column = ""
  bbdf_streams = NULL
  if(tolower(dataset) == "nhdh"){
    load(file=system.file('extdata/nhd_bb_streams_cache.Rdata', package='hydrolinks'))
    dl_file = "extdata/nhdh.csv"
    id_column = "PERMANENT_"
    wbd_bb = bbdf_streams
  }
  else if(tolower(dataset) == "nhdplus"){
    load(file=system.file('extdata/nhdplus_flowline_bb_cache.rdata', package='hydrolinks'))
    dl_file = "extdata/nhdplus.csv"
    id_column = "COMID"
    wbd_bb = bbdf_flowline
  }
  
  
  
  sites = data.frame(lats, lons, ids)
  xy = cbind(sites$lons, sites$lats)
  not_na = which(!is.na(sites$lats) & !is.na(sites$lons))
  pts = SpatialPointsDataFrame(xy[not_na, , drop=FALSE], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), data=data.frame(ids[not_na, drop=FALSE]))
  ids.not_na..drop...FALSE. = NULL
  pts@data = rename(pts@data, MATCH_ID = ids.not_na..drop...FALSE.)
  pts = spTransform(pts, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  res   = list()
  
  matches_found = 0
  
  xmin = xmax = ymin = ymax = NULL
  
  for(i in 1:nrow(pts@coords)){
    res[[i]] = subset(wbd_bb, xmin <= pts@coords[i,1] & xmax >= pts@coords[i,1] & ymin <= pts@coords[i,2] & ymax >= pts@coords[i,2])
  }
  
  to_check = unique(do.call(rbind, res))
  
  
  match_res = list()
  
  if(nrow(to_check) == 0){
    ret = data.frame(MATCH_ID = sites$ids)
    ret$PERMANENT_ID = NA
    return(ret)
  }
  
  for(i in 1:nrow(to_check)){
    #get nhd layer
    #check_dl_file(system.file(dl_file, package = "hydrolinks"), to_check[i, 'file'])
    nhd       = readOGR(file.path(local_path(), "unzip", to_check[i,'file'], "NHDFlowline_projected.shp"))
    nhd = gBuffer(nhd, byid = TRUE, width = max_dist)
    matches = over(pts, nhd)
    matches$MATCH_ID = sites$ids
    match_res[[i]] = matches
  }
  
  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches[,id_column]), ])
}