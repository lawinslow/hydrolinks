#' @title Link geopoints to flowlines
#'
#' @description Link geopoints to flowlines in the NHD
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param max_dist numeric maximum line snapping distance in meters
#'
#' @return flowline permanent ids
#'
#' @import rgdal
#' @import sp
#' @import maptools
#' @import dplyr
#'
#' @export

link_flowlines = function(lats, lons, ids, max_dist = 100){
  bbdf_streams = NULL
  load(file=system.file('extdata/nhd_bb_streams_cache.Rdata', package='nhdtools'))
  wbd_bb = bbdf_streams

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
    check_dl_file(system.file("extdata/nhdh.csv", package = "nhdtools"), to_check[i, 'file'])

    nhd       = readOGR(file.path(local_path(), "unzip", to_check[i,'file'], "NHDFlowline_projected.shp"))

    ids = rep(NA, length(sites$lats))

    bbox = wbd_bb[wbd_bb$file == to_check[i, 'file'], ]
    pts = pts[bbox$xmin <= pts@coords[, 1] & bbox$xmax >= pts@coords[,1] & bbox$ymin <= pts@coords[,2] & bbox$ymax >= pts@coords[,2],]

    #if there are no points to check that overlap this shapefile, skip
    if(nrow(pts) < 1){
      next
    }

    matches = snapPointsToLines(pts, nhd, maxDist = max_dist, idField = ids)
    if(is.na(matches)){
      match_res[i*nrow(pts@coords) -nrow(pts@coords) + j] = NA
      next
    }
    matches_found = matches_found + nrow(matches)
    for(j in 1:nrow(matches)){
      match_data = nhd@data[as.numeric(as.character(matches$nearest_line_id[j])) + 1,]
      match_data$MATCH_ID = matches$MATCH_ID[j]
      match_res[[i*nrow(matches) - nrow(matches) + j]] = match_data
    }
    if(matches_found == nrow(sites)){
      break
    }
  }

  unique_matches = unique(do.call(rbind, match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches$PERMANENT_), ])
}
