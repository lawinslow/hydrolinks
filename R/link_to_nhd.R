#' @title Link geopoints to Waterbodies
#'
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#'
#'
#' @return Water body permanent IDs
#'
#' @import rgdal
#' @import sp
#'
#' @export
link_to_waterbodies = function(lats, lons, ids){

  wbd_shapes = file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDWaterbody.shp')
  load(file=system.file('extdata/nhd_bb_cache.Rdata', package='nhdtools'))
  wbd_bb = bbdf

  sites = data.frame(lats, lons, ids)
  res   = list()

  for(i in 1:nrow(sites)){
    res[[i]] = subset(wbd_bb, xmin <= sites[i,'lons'] & xmax >= sites[i,'lons'] & ymin <= sites[i,'lats'] & ymax >= sites[i,'lats'])
  }

  to_check = unique(do.call(rbind, res))


  match_res = list()
  
  if(nrow(to_check) == 0){
      ret = data.frame(MATCH_ID = sites$ids)
      ret$PERMANENT_ID = NA
      return(ret)
  }
  
  #TODO: Finish this
  for(i in 1:nrow(to_check)){
    #get nhd layer
    nhd       = readOGR(to_check[i,'file'])

    ids = rep(NA, length(sites$lats))

    not_na = which(!is.na(sites$lats) & !is.na(sites$lons))

    xy = cbind(sites$lons, sites$lats)

    pts = SpatialPoints(xy[not_na, , drop=FALSE], proj4string=CRS(proj4string(nhd)))
    matches = over(pts, nhd, fn = NULL, returnList = FALSE)

    matches$MATCH_ID = sites$ids[not_na]
    match_res[[i]] = matches
  }

  unique_matches = unique(do.call(rbind, match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches$PERMANENT_), ])
}
