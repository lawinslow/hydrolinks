#' @title Link geoshapes to streams
#'
#'
#' @param shapedf SpatialPolygonsDataFrame of shapes to match
#'
#'
#' @return
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import maptools
#' @import dplyr
#'
#' @export link_streams


snapPointsToLines <- function( points, lines, maxDist=NA, withAttrs=TRUE, idField=NA) {
  
  if (rgeosStatus()) {
    if (!requireNamespace("rgeos", quietly = TRUE))
      stop("package rgeos required for snapPointsToLines")
  } else
    stop("rgeos not installed")
  
  if (class(points) == "SpatialPoints" && missing(withAttrs))
    withAttrs = FALSE
  
  if (class(points) == "SpatialPoints" && withAttrs==TRUE)
    stop("A SpatialPoints object has no attributes! Please set withAttrs as FALSE.")
  
  
  if (!is.na(maxDist)) {
    w = rgeos::gWithinDistance(points, lines, dist=maxDist, byid=TRUE)
    validPoints = apply(w,2,any)
    validLines = apply(w,1,any)
    #origPoints = points
    points = points[validPoints,]
    lines =  lines[validLines,]
    #pointIds = pointIds[points %in% origPoints, ]
  }
  
  d = rgeos::gDistance(points, lines, byid=TRUE) 
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object 
  
  coordsLines = coordinates(lines)  
  coordsPoints = coordinates(points)
  print(nrow(coordsPoints))
  print(nrow(coordsLines))
  if(nrow(coordsPoints) == 0){
    return(NA)
  }
  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points), 
                      function(x) 
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))
  
  # Recover lines' Ids (If no id field has been specified, take the sp-lines id)
  if (!is.na(idField)) nearest_line_id = lines@data[,idField][nearest_line_index] 
  else nearest_line_id = sapply(slot(lines, "lines"), function(i) slot(i, "ID"))[nearest_line_index] 
  
  # Create data frame and sp points
  if (withAttrs) df = cbind(points@data, nearest_line_id) 
  else df = data.frame(nearest_line_id, row.names=names(nearest_line_index))
  
  SpatialPointsDataFrame(coords=t(mNewCoords), data=df, 
                         proj4string=CRS(proj4string(points)))
}



link_streams = function(lats, lons, ids, max_dist = 100){
  wbd_shapes = file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDFlowline_projected.shp')
  load(file=system.file('extdata/nhd_bb_streams_cache.Rdata', package='nhdtools'))
  wbd_bb = bbdf
  
  sites = data.frame(lats, lons, ids)
  xy = cbind(sites$lons, sites$lats)
  not_na = which(!is.na(sites$lats) & !is.na(sites$lons))
  pts = SpatialPointsDataFrame(xy[not_na, , drop=FALSE], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), data=data.frame(ids[not_na, drop=FALSE]))
  pts@data = rename(pts@data, MATCH_ID = ids.not_na..drop...FALSE.)
  pts = spTransform(pts, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  res   = list()
  
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
    nhd       = readOGR(to_check[i,'file'])
    
    ids = rep(NA, length(sites$lats))
    
    
    # for(j in 1:nrow(pts@coords)){
    #   matches = snapPointsToLines(pts[j], nhd, maxDist = 10, idField = ids)
    #   if(is.na(matches)){
    #     match_res[i*nrow(pts@coords) -nrow(pts@coords) + j] = NA
    #     next()
    #   }
    #   match_data = nhd@data[as.numeric(as.character(matches$nearest_line_id)) + 1,]
    #   match_data$MATCH_ID = sites$ids[j]
    #   match_res[[i*nrow(pts@coords) - nrow(pts@coords) + j]] = match_data
    # }
    bbox = wbd_bb[wbd_bb$file == to_check[i, 'file'], ]
    pts = pts[bbox$xmin <= pts@coords[, 1] & bbox$xmax >= pts@coords[,1] & bbox$ymin <= pts@coords[,2] & bbox$ymax >= pts@coords[,2],]
    matches = snapPointsToLines(pts, nhd, maxDist = max_dist, idField = ids)
    if(is.na(matches)){
      match_res[i*nrow(pts@coords) -nrow(pts@coords) + j] = NA
      next()
    }
    for(j in 1:nrow(matches)){
      match_data = nhd@data[as.numeric(as.character(matches$nearest_line_id[j])) + 1,]
      match_data$MATCH_ID = matches$MATCH_ID[j]
      match_res[[i*nrow(matches) - nrow(matches) + j]] = match_data
    }
  }
  
  unique_matches = unique(do.call(rbind, match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches$PERMANENT_), ])
}