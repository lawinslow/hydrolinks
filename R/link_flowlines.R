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

link_to_flowlines = function(lats, lons, ids, max_dist = 100){
  bbdf_streams = NULL
  load(file=system.file('extdata/nhd_bb_streams_cache.Rdata', package='hydrolinks'))
  wbd_bb = bbdf_streams
  sites = data.frame(lats, lons, ids)
  
  chunk = 1000
  r = rep(1:ceiling(nrow(sites)/chunk), each = chunk)[1:nrow(sites)]
  sites = split(sites, r)
  matches_result = list()
  for(i in 1:length(sites)){
    xy = cbind(sites[[i]]$lons, sites[[i]]$lats)
    not_na = which(!is.na(sites[[i]]$lats) & !is.na(sites[[i]]$lons))
    pts = SpatialPointsDataFrame(xy[not_na, , drop=FALSE], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), data=data.frame(ids[not_na, drop=FALSE]))
    ids.not_na..drop...FALSE. = NULL
    pts@data = rename(pts@data, MATCH_ID = ids.not_na..drop...FALSE.)
    pts = spTransform(pts, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
    
    res   = list()
    
    matches_found = 0
    
    xmin = xmax = ymin = ymax = NULL
    
    for(j in 1:nrow(pts@coords)){
      res[[j]] = subset(wbd_bb, xmin <= pts@coords[j,1] & xmax >= pts@coords[j,1] & ymin <= pts@coords[j,2] & ymax >= pts@coords[j,2])
    }
    
    to_check = unique(do.call(rbind, res))
    
    
    match_res = list()
    
    if(nrow(to_check) == 0){
      ret = data.frame(MATCH_ID = sites[[i]]$ids)
      ret$PERMANENT_ID = NA
      next
    }
    for(k in 1:nrow(to_check)){
      #get nhd layer
      check_dl_file(system.file("extdata/nhdh.csv", package = "hydrolinks"), to_check[k, 'file'])
      
      nhd       = readOGR(file.path(local_path(), "unzip", to_check[k,'file'], "NHDFlowline_projected.shp"))
      
      #ids = rep(NA, length(sites$lats))
      
      bbox = wbd_bb[wbd_bb$file == to_check[k, 'file'], ]
      pts = pts[bbox$xmin <= pts@coords[, 1] & bbox$xmax >= pts@coords[,1] & bbox$ymin <= pts@coords[,2] & bbox$ymax >= pts@coords[,2],]
      
      #if there are no points to check that overlap this shapefile, skip
      if(nrow(pts) < 1){
        next
      }
      
      matches = snapPointsToLines(pts, nhd, maxDist = max_dist)
      if(is.na(matches)){
        #match_res[k*nrow(pts@coords) -nrow(pts@coords) + 1] = NA
        next
      }
      matches_found = matches_found + nrow(matches)
      for(l in 1:nrow(matches)){
        match_data = nhd@data[as.numeric(as.character(matches$nearest_line_id[l])) + 1,]
        match_data$MATCH_ID = matches$MATCH_ID[l]
        match_res[[k*nrow(matches) - nrow(matches) + l]] = match_data
      }
      #if(matches_found == nrow(sites)){
      #  break
      #}
    }
    
    unique_matches = unique(do.call(rbind, match_res))
    matches_result[[i]] = unique_matches
  }
  
  
  matches_result = bind_rows(matches_result)
  
  #return matches that have non-NA value PREMANENT_ID
  return(matches_result[!is.na(matches_result$PERMANENT_), ])
}
