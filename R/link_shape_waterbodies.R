#' @title Link geoshapes to Waterbodies
#' 
#' @description Link geoshapes to waterbodies
#'
#'
#' @param shapedf SpatialPolygonsDataFrame of shapes to match
#'
#'
#' @return matching waterbody attributes
#'
#' @import rgdal
#' @importFrom rgeos gIntersects gConvexHull
#' @import sp
#'
#' @export
link_shape_waterbodies = function(shapedf){
  bbdf = NULL
  wbd_shapes = file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDWaterbody.shp')
  load(file=system.file('extdata/nhd_bb_cache.Rdata', package='hydrolinks'))
  wbd_bb = bbs_to_sp(bbdf)

  #project input to NHD
  shapedf = spTransform(shapedf, CRS(nhd_proj))
  shapeout = gConvexHull(shapedf)

  matches = gIntersects(shapeout, wbd_bb, byid = TRUE)
  filematches = rownames(matches)[matches]

  match_res = list()
  #TODO: Finish this
  for(i in 1:length(filematches)){
    #get nhd layer
    nhd       = readOGR(filematches[i])
    FTYPE = NULL
    nhd = subset(nhd, FTYPE %in% c('390', '361', '436'))

    tmp = gIntersects(shapedf, nhd, byid=TRUE, returnDense=FALSE)

    if(all(sapply(tmp, is.null))){
      next
    }

    match_indx = which(!sapply(tmp, is.null))
    nhd_indx = sapply(tmp[which(!sapply(tmp, is.null))], function(x){x[1]})

    poly_matches = data.frame(match_indx, PERMANENT_ID=nhd$PERMANENT_[nhd_indx])
    if(nrow(poly_matches) > 0){
      match_res[[length(match_res)+1]] = poly_matches
    }
  }

  unique_matches = unique(do.call(rbind, match_res))
  #return matches that have non-NA value PREMANENT_ID
  return(unique_matches[!is.na(unique_matches$PERMANENT_), ])
}



bbs_to_sp = function(bb){

  all_polys = list()
  for(i in 1:nrow(bb)){

    coords = matrix(c(bb$xmin[i], bb$ymin[i],
                      bb$xmin[i], bb$ymax[i],
                      bb$xmax[i], bb$ymax[i],
                      bb$xmax[i], bb$ymin[i],
                      bb$xmin[i], bb$ymin[i]),
                    ncol = 2, byrow = TRUE)
    all_polys[[i]] = Polygons(list(Polygon(coords)), ID=bb$file[i])
  }


  Ps1 = SpatialPolygons(all_polys, proj4string=CRS(nhd_proj))

  spdata = bb[,'file', drop=FALSE]
  rownames(spdata) = spdata$file

  return(SpatialPolygonsDataFrame(Ps1, spdata))
}
