#' @title Link geopoints to Waterbodies by centroids
#'
#' @description Link geopoints to a waterbody with the closest centroid a geospatial dataset
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param dataset Character name of dataset to link against. Can be either "nhd" or "hydrolakes"
#' @param max_dist maximum distance between points and centroids to match
#'
#'
#' @return Water body permanent IDs
#'
#' @import sf
#'
#' @export
link_waterbody_centroids = function(lats, lons, ids, dataset = c("nhdh", "nhdplusv2", "hydrolakes"), max_dist = 25){
  dataset = match.arg(dataset)
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

  sites = data.frame(lats, lons, ids)
  not_na = which(!is.na(sites$lats) & !is.na(sites$lons))
  
  xy = cbind(sites$lons, sites$lats)
  xy = xy[not_na, , drop = FALSE]
  
  pts = list()
  for(i in 1:nrow(xy)){
    pts[[i]] = st_point(c(xy[i, 1], xy[i,2]))
  }
  
  pts = st_sf(MATCH_ID = ids[not_na, drop = FALSE], geom = st_sfc(pts), row.names = c(1:nrow(sites)), crs = nhd_proj)
  pts = st_transform(pts, st_crs(nhd_projected_proj))
  
  res   = list()

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
    #get waterbody layer
    check_dl_file(system.file(dl_file, package = "hydrolinks"), to_check[i, 'file'])

    shapefile_name = ""
    if(tolower(dataset) == "nhdh" || tolower(dataset) == "nhdplusv2"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else if(tolower(dataset) == "hydrolakes"){
      shapefile_name = "HydroLAKES_polys_v10_projected.shp"
    }

    nhd       = st_read(file.path(local_path(), "unzip", to_check[i,'file'], shapefile_name), stringsAsFactors=FALSE)

    centroids = list()
    for(j in 1:nrow(nhd)){
      centroids[[j]] = st_point(c(nhd$centroid_x[j], nhd$centroid_y[j]))
    }
    
    nhd_data = nhd[,,drop=TRUE]
    nhd_data$geometry = NULL
    centroids = st_sf(nhd_data, geometry = st_sfc(centroids), crs = nhd_projected_proj)
    centroids_buffer = st_buffer(centroids, max_dist)
    matches = st_intersects(pts, centroids_buffer)
    if(length(unlist(matches)) == 0){
      next
    }
    matches_multiple = which(lengths(matches) > 1)
    if(length(matches_multiple) > 0){
      for(j in 1:length(matches_multiple)){
        nhd_rows = centroids[matches[matches_multiple][[j]],]
        distance = st_distance(pts[matches_multiple[j], ], nhd_rows)
        matches[matches_multiple][[j]] = which.min(distance[1,])
      }
    }
    matches[lengths(matches) == 0] = NA
    nhd_matched = centroids[unlist(matches),]
    nhd_matched$MATCH_ID = sites$ids
    nhd_matched = nhd_matched[,,drop = TRUE]
    nhd_matched$geometry = NULL
    match_res[[i]] = nhd_matched
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value id
  if(nrow(unique_matches) > 0)
    return(unique_matches[!is.na(unique_matches[,id_column]),])
  else{
    return(NULL)
  }
}
