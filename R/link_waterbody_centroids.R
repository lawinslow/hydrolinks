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
#' @examples
#' \dontrun{
#' centroidpt = c(33.655277, -117.834007)
#'
#' #should be item ID 126859554
#' link_waterbody_centroids( centroidpt[1], centroidpt[2], 'dummyid', dataset='nhdh')
#' }
#'
#' @export
link_waterbody_centroids = function(lats, lons, ids, dataset = c("nhdh", "nhdplusv2", "hydrolakes"), max_dist = 25){
  dataset = match.arg(dataset)

  dinfo = dataset_info(dataset, 'waterbody')
  bbdf = NULL
  load(dinfo$bb_cache_path)

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
  st_crs(bbdf) = nhd_projected_proj

  res   = list()

  xmin = xmax = ymin = ymax = NULL
  for(i in 1:nrow(pts)){
    res = c(res, bbdf[unlist(st_intersects(pts[i,], bbdf)),"file", drop=TRUE])
    #res[[i]] = subset(bbdf, xmin <= pts$geom[[i]][1] & xmax >= pts$geom[[i]][1] & ymin <= pts$geom[[i]][2] & ymax >= pts$geom[[i]][2])
  }

  to_check = as.data.frame(unique(do.call(rbind, res)), stringsAsFactors = FALSE)

  ## If we have no files to check, geopoints must be *way* outside mapped territory for this dataset
  #empty data frame indicates no match (throw in warning to try and be helpful)
  if(nrow(to_check) == 0){
    warning('hydrolinks::Supplied geopoints do not overlap ', dataset, ' dataset')
    ret = data.frame(MATCH_ID = rep(NA, 0))
    ret[,dinfo$id_column] = rep(NA, 0)
    return(ret)
  }

  # start the big matching loop
  colnames(to_check)[1] = "file"
  match_res = list()

  for(i in 1:nrow(to_check)){
    #get waterbody layer
    check_dl_file(dinfo$file_index_path, to_check[i, 'file'])

    nhd       = st_read(file.path(cache_get_dir(), "unzip", to_check[i,'file'], dinfo$shapefile_name), stringsAsFactors=FALSE)

    centroids = list()
    for(j in 1:nrow(nhd)){
      centroids[[j]] = st_point(c(nhd$centroid_x[j], nhd$centroid_y[j]))
    }

    nhd_data = nhd[,,drop=TRUE]
    nhd_data$geometry = NULL
    centroids = st_sf(nhd_data, geometry = st_sfc(centroids), crs = nhd_projected_proj)
    centroids_buffer = st_buffer(centroids, max_dist)
    matches = st_intersects(pts, centroids_buffer)

    #ok, deal with this sparse predicate structure
    # if nothing matches at all
    if(sum(sapply(matches, length) > 0) == 0){
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

    #carefully peel apart the matches and put the IDs where they need to go
    nhd_matched = centroids[unlist(matches),]
    nhd_matched$MATCH_ID = pts[which(lengths(matches) > 0),]$MATCH_ID
    st_geometry(nhd_matched) = NULL
    match_res[[i]] = data.frame(nhd_matched, stringsAsFactors = FALSE)
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value id
  # if(nrow(unique_matches) > 0)
  #   #return(unique_matches[!is.na(unique_matches[,dinfo$id_column]),])
  return(unique_matches)
  # else{
  #   return(NULL)
  # }
}
