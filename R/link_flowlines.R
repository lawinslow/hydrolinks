#' @title Link geopoints to flowlines
#'
#' @description Link geopoints to flowlines in the NHD
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param max_dist numeric maximum line snapping distance in meters
#' @param dataset Character name of dataset to link against. Can be either "nhdh" or "nhdplusv2"
#'
#' @return flowline permanent ids
#'
#' @import sf
#' @import dplyr
#'
#' @export

link_to_flowlines = function(lats, lons, ids, max_dist = 100, dataset = c("nhdh", "nhdplusv2")){
  dataset = match.arg(dataset)
  dl_file = ""
  id_column = ""
  bbdf_streams = NULL
  bbdf_flowline = NULL
  if(tolower(dataset) == "nhdh"){
    load(file=system.file('extdata/nhd_bb_streams_cache.Rdata', package='hydrolinks'))
    dl_file = "extdata/nhdh.csv"
    id_column = "PERMANENT_"
    wbd_bb = bbdf_streams
  }
  else if(tolower(dataset) == "nhdplusv2"){
    load(file=system.file('extdata/nhdplus_flowline_bb_cache.rdata', package='hydrolinks'))
    dl_file = "extdata/nhdplusv2.csv"
    id_column = "COMID"
    wbd_bb = bbdf_flowline
  }



  sites = data.frame(lats, lons, ids)
  xy = cbind(sites$lons, sites$lats)
  not_na = which(!is.na(sites$lats) & !is.na(sites$lons))
  pts = list()
  xy = xy[not_na, , drop = FALSE]
  for(i in 1:nrow(xy)){
    pts[[i]] = st_point(c(xy[i, 1], xy[i, 2]))
  }
  pts = st_sf(MATCH_ID = ids[not_na, drop = FALSE], geom = st_sfc(pts), row.names = c(1:nrow(sites)), crs = nhd_proj)
  pts = st_transform(pts, st_crs(nhd_projected_proj))

  res   = list()

  matches_found = 0

  xmin = xmax = ymin = ymax = NULL

  for(i in 1:nrow(pts)){
    res[[i]] = subset(wbd_bb, xmin <= pts$geom[[i]][1] & xmax >= pts$geom[[i]][1] & ymin <= pts$geom[[i]][2] & ymax >= pts$geom[[i]][2])
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
    check_dl_file(system.file(dl_file, package = "hydrolinks"), to_check[i, 'file'])
    nhd       = st_read(file.path(local_path(), "unzip", to_check[i,'file'], "NHDFlowline_projected.shp"), stringsAsFactors=FALSE)
    st_crs(nhd) = nhd_projected_proj
    nhd_buffer = st_buffer(nhd, max_dist)
    matches = st_intersects(pts, nhd_buffer)
    if(length(unlist(matches)) == 0){
      next
    }
    matches_multiple = which(lengths(matches) > 1)
    if(length(matches_multiple) > 0){
      for(j in 1:length(matches_multiple)){
        nhd_rows = nhd[matches[matches_multiple][[j]],]
        distance = st_distance(pts[matches_multiple[j], ], nhd_rows)
        matches[matches_multiple][[j]] = which.min(distance[1,])
      }
    }
    matches[lengths(matches) == 0] = NA
    nhd_matched = nhd[unlist(matches),]
    nhd_matched$MATCH_ID = sites$ids
    nhd_matched = nhd_matched[,,drop = TRUE]
    nhd_matched$geometry = NULL
    match_res[[i]] = nhd_matched
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches[,id_column]), ])
}
