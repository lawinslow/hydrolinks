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
#' @import units
#'
#' @examples
#' \dontrun{
#' latlon = c(42.703290, -73.702855)
#'
#' #should link to hudson river
#' link_to_flowlines(latlon[1], latlon[2], 'dummyid')
#'
#' }
#'
#' @export

link_to_flowlines = function(lats, lons, ids, max_dist = 100, dataset = c("nhdh", "nhdplusv2")){
  dataset = match.arg(dataset)

  dinfo = dataset_info(dataset, 'flowline')
  bbdf = NULL
  load(dinfo$bb_cache_path)

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

  #in keeping with "no match is data.frame of zero rows"
  if(nrow(to_check) == 0){
    ret = data.frame(MATCH_ID = rep(NA, 0))
    ret[,dinfo$id_column] = rep(NA, 0)
    return(ret)
  }

  for(i in 1:nrow(to_check)){
    #get nhd layer
    check_dl_file(dinfo$file_index_path, to_check[i, 'file'])
    shape       = st_read(file.path(local_path(), "unzip", to_check[i,'file'], dinfo$shapefile_name), stringsAsFactors=FALSE)
    #st_crs(shape) = nhd_projected_proj
    shape = st_transform(shape, nhd_projected_proj)

    #LAW: Ok, the buffer-based matching is very slow for a small lat/lon list. Conversely, simple distance is
    #slow for really long point lists. I'm trying to split the difference here and optimize for both.
    if(nrow(pts) > 300){ #magic number cutoff! Seems to balance performance

      shape_buffer = st_buffer(shape, max_dist)
      matches = st_intersects(pts, shape_buffer)
    }else{
      units(max_dist) = with(units::ud_units, m) #input max dist is defineda as meters

      matchmat = st_distance(shape, pts)
      mini = apply(matchmat, 2, which.min)
      matches = lapply(seq_along(mini), function(i){
          if(matchmat[mini[i], i] <= max_dist){
            return(mini[i])
          }else{
            return(double(length=0))
          }
        })
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
    st_geometry(shape_matched) = NULL
    match_res[[i]] = shape_matched
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value PREMANENT_ID
  #return(unique_matches[!is.na(unique_matches[,dinfo$id_column]), ])
  return(unique_matches)
}
