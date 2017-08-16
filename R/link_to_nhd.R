#' @title Link geopoints to Waterbodies
#' 
#' @description Link geopoints to waterbodies in a geospatial dataset.
#'
#' @param lats Vector of point latitudes
#' @param lons Vector of point longitudes
#' @param ids Vector of point identifiers (string or numeric)
#' @param dataset Character name of dataset to link against. Can be either "nhdh", "hydrolakes", or "nhdplusv2"
#'
#'
#' @return Water body permanent IDs
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#'
#' @export
link_to_waterbodies = function(lats, lons, ids, dataset = "nhdh", buffer = 0){
  dl_file = ""
  id_column = ""
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
  else{
    stop("Invalid dataset name!")
  }
  

  sites = data.frame(lats, lons, ids)
  not_na = which(!is.na(sites$lats) & !is.na(sites$lons))
  
  xy = cbind(sites$lons, sites$lats)
  
  pts = SpatialPoints(xy[not_na, , drop=FALSE], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  pts = spTransform(pts, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

  res   = list()
  
  xmin = xmax = ymin = ymax = NULL
  for(i in 1:nrow(sites)){
    res[[i]] = subset(wbd_bb, xmin <= pts@coords[i,1] & xmax >= pts@coords[i,1] & ymin <= pts@coords[i,2] & ymax >= pts@coords[i,2])
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
    if(tolower(dataset) == "nhdh" || tolower(dataset) == "nhdplusv2"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else if(tolower(dataset) == "hydrolakes"){
      shapefile_name = "HydroLAKES_polys_v10_projected.shp"
    }
    
    nhd       = readOGR(file.path(local_path(), "unzip", to_check[i,'file'], shapefile_name))
    
    if(buffer > 0){
      nhd = gBuffer(nhd, byid = TRUE, width = buffer)
    }

    ids = rep(NA, length(sites$lats))

    matches = over(pts, nhd, fn = NULL, returnList = FALSE)

    matches$MATCH_ID = sites$ids[not_na]
    match_res[[i]] = matches
  }

  unique_matches = unique(bind_rows(match_res))
  #return matches that have non-NA value id
  return(unique_matches[!is.na(unique_matches[,id_column]),])
}
