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
#' @import rgdal
#' @import sp
#' @importFrom rgeos gBuffer
#'
#' @export
link_waterbody_centroids = function(lats, lons, ids, dataset = "nhd", max_dist = 25){
  dl_file = ""
  id_column = ""
  bbdf = NULL
  if(tolower(dataset) == "nhd"){
    load(file=system.file('extdata/nhd_bb_cache.Rdata', package='hydrolinks'))
    dl_file = "extdata/nhdh.csv"
    id_column = "PERMANENT_"
  }
  else if(tolower(dataset) == "hydrolakes"){
    load(file=system.file('extdata/hydrolakes_bb_cache.Rdata', package='hydrolinks'))
    dl_file = "extdata/hydrolakes.csv"
    id_column = "Hylak_id"
  }
  else{
    stop("Invalid dataset name!")
  }
  wbd_bb = bbdf

  sites = data.frame(lats, lons, ids)
  res   = list()

  xmin = xmax = ymin = ymax = NULL
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
    check_dl_file(system.file(dl_file, package = "hydrolinks"), to_check[i, 'file'])

    shapefile_name = ""
    if(tolower(dataset) == "nhd"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else if(tolower(dataset) == "hydrolakes"){
      shapefile_name = "HydroLAKES_polys_v10.shp"
    }

    nhd       = readOGR(file.path(local_path(), "unzip", to_check[i,'file'], shapefile_name), stringsAsFactors=FALSE)


    #ids = rep(NA, length(sites$lats))

    not_na = which(!is.na(sites$lats) & !is.na(sites$lons))

    xy = cbind(sites$lons, sites$lats)

    pts = SpatialPoints(xy[not_na, , drop=FALSE], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    pts = spTransform(pts, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
    centroids = SpatialPointsDataFrame(data.frame(nhd$centroid_x, nhd$centroid_y), nhd@data, proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
    centroids = gBuffer(centroids, byid = TRUE, width = max_dist)
    matches = over(pts, centroids)
    #if(!is.null(matches)){
    #  for(j in 1:length(matches)){
    #    match_res[[j]] = matches[[j]]@data
    #  }
    #}
    #match_data = nhd@data[!is.na(matches),]
    matches$MATCH_ID = sites$ids
    match_res[[i]] = matches
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value id
  if(nrow(unique_matches) > 0)
    return(unique_matches[!is.na(unique_matches[,id_column]),])
  else{
    return(NULL)
  }
}

centroid_distance = function(shape, pts, max_dist, match_id){
  result = c()
  for(i in 1:nrow(pts@coords)){
    match = sqrt(abs(pts@coords[i, 1] - shape$centroid_x)^2 + abs(pts@coords[i, 2] - shape$centroid_y)^2) <= max_dist
    matching_features = shape[match,]
    if(nrow(matching_features) == 0){
      next
    }
    for(j in 1:nrow(matching_features)){
      matching_features$MATCH_ID = match_id[i]
      result = c(result, matching_features[j,])
    }
  }
  return(result)
}
