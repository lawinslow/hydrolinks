#' @title Link geopoints to Waterbodies
#'
#' @description
#' Link geopoints to waterbodies in a geospatial dataset. Use the
#' point-in-polygon technique with user-selectable polygon buffer size.
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param dataset Character name of dataset to link against. Can be either "nhdh", "hydrolakes", or "nhdplusv2"
#' @param buffer Numeric width of polygon buffer in m
#'
#' @return Water body permanent IDs
#'
#' @import sf
#'
#' @examples
#' latlon = c(43.108728, -89.418293)
#' \dontrun{
#' #returns linked waterbody site information for that lat/lon
#' link_to_waterbodies(latlon[1], latlon[2], 'id1', dataset = 'nhdh')
#'
#' }
#' @export
link_to_waterbodies = function(lats, lons, ids, dataset = c("nhdh", "hydrolakes", "nhdplusv2"), buffer = 0){
  dataset = match.arg(dataset)

  #get dataset info and load bounding box cache
  dinfo = dataset_info(dataset, 'waterbody')
  bbdf = NULL #to prevent warnings
  load(file=dinfo$bb_cache_path) #will bring in a var named bbdf


  sites = data.frame(lats, lons, ids)
  pts = st_as_sf(sites, coords = c("lons", "lats"), crs = nhd_proj)
  pts = st_transform(pts, st_crs(nhd_projected_proj))

  res   = list()

  xmin = xmax = ymin = ymax = NULL
  for(i in 1:nrow(pts)){
    res[[i]] = subset(bbdf, xmin <= pts$geom[[i]][1] & xmax >= pts$geom[[i]][1] & ymin <= pts$geom[[i]][2] & ymax >= pts$geom[[i]][2])
  }

  to_check = unique(do.call(rbind, res))

  match_res = list()

  if(nrow(to_check) == 0){
    ret = data.frame(MATCH_ID = rep(NA, 0))
    ret[,dinfo$id_column] = rep(NA, 0)
    return(ret)
  }

  for(i in 1:nrow(to_check)){
    #get waterbody layer
    check_dl_file(dinfo$file_index_path, to_check[i, 'file'])

    shape         = st_read(file.path(local_path(), "unzip", to_check[i,'file'], dinfo$shapefile_name), stringsAsFactors=FALSE)
    #st_crs(shape) = nhd_projected_proj
    shape = st_transform(shape, nhd_projected_proj)

    if(tolower(dataset) == 'nhdh'){
      shape = shape[shape$FTYPE %in% c('390', '361', '436'), ]
    }

    if(buffer > 0){
      shape_buffer = st_buffer(shape, buffer)
      matches = st_intersects(pts, shape_buffer)
    }
    else{
      matches = st_intersects(pts, shape)
    }

    if(length(unlist(matches)) == 0){
      next
    }
    matches_multiple = which(lengths(matches) > 1)
    if(length(matches_multiple) > 0){
      for(j in 1:length(matches_multiple)){
        shape_rows = shape[matches[matches_multiple][[j]],]
        distance = st_distance(pts[matches_multiple[j], ], shape_rows)
        matches[matches_multiple][[j]] = which.min(distance[1,])
      }
    }
    matches[lengths(matches) == 0] = NA
    shape_matched = shape[unlist(matches),]
    shape_matched$MATCH_ID = sites$ids
    #shape_matched = shape_matched[,,drop = TRUE]
    shape_matched$geometry = NULL
    match_res[[i]] = as.data.frame(shape_matched)
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value id
  #return(unique_matches[!is.na(unique_matches[,dinfo$id_column]),])

  #return all matches
  return(unique_matches)
}
