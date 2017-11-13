link_shape_waterbodies = function(shapesf, dataset = c("nhdh", "nhdplusv2", "hydrolakes"), id_column){
  dataset = match.arg(dataset)
  dl_file = ""
  #id_column = ""
  bbdf = NULL
  bbdf_waterbody = NULL
  if(tolower(dataset) == "nhdh"){
    load(file=system.file('extdata/nhd_bb_cache_projected.Rdata', package='hydrolinks'))
    dl_file = "extdata/nhdh.csv"
    wbd_bb = bbdf
  }
  else if(tolower(dataset) == "hydrolakes"){
    load(file=system.file('extdata/hydrolakes_bb_cache_projected.Rdata', package='hydrolinks'))
    dl_file = "extdata/hydrolakes.csv"
    wbd_bb = bbdf
  }
  else if(tolower(dataset) == "nhdplusv2"){
    load(file=system.file('extdata/nhdplus_waterbody_bb_cache.rdata', package='hydrolinks'))
    dl_file = "extdata/nhdplusv2.csv"
    wbd_bb = bbdf_waterbody
  }

  shapesf = st_transform(shapesf, nhd_projected_proj)
  shapeout = st_convex_hull(shapesf)

  bb_pts = list()
  for(i in 1:nrow(wbd_bb)){
    pt1 = c(wbd_bb$xmin[i], wbd_bb$ymin[i])
    pt2 = c(wbd_bb$xmin[i], wbd_bb$ymax[i])
    pt3 = c(wbd_bb$xmax[i], wbd_bb$ymax[i])
    pt4 = c(wbd_bb$xmax[i], wbd_bb$ymin[i])
    bb_pts[[i]] = st_polygon(list(rbind(pt1, pt2, pt3, pt4, pt1)))
  }
  bb_pts = st_sf(wbd_bb$file, st_sfc(bb_pts), crs = nhd_projected_proj)
  matches = st_intersects(shapeout, bb_pts)
  filematches = wbd_bb$file[unique(unlist(matches))]

  if(length(filematches) == 0){
    ret = data.frame(MATCH_ID = sites$ids)
    ret$PERMANENT_ID = NA
    return(ret)
  }
  match_res = list()

  for(i in 1:length(filematches)){
    #get waterbody layer
    check_dl_file(system.file(dl_file, package = "hydrolinks"), filematches[i])

    shapefile_name = ""
    if(tolower(dataset) == "nhdh" || tolower(dataset) == "nhdplusv2"){
      shapefile_name = "NHDWaterbody_projected.shp"
    }
    else if(tolower(dataset) == "hydrolakes"){
      shapefile_name = "HydroLAKES_polys_v10_projected.shp"
    }

    nhd       = st_read(file.path(local_path(), "unzip", filematches[i], shapefile_name), stringsAsFactors=FALSE)
    st_crs(nhd) = nhd_projected_proj

    matches = st_intersects(shapesf, nhd)

    if(length(unlist(matches)) == 0){
      next
    }

    matches_data = list()
    matches_index = 1
   # st_crs(matches_data) = nhd_projected_proj
    for(j in 1:length(matches)){
      if(length(matches[[j]]) == 0){
        next
      }
      for(k in 1:length(matches[[j]])){
        match_row = nhd[matches[[j]][k], drop = TRUE]
        match_row$MATCH_ID = as.character(shapesf[j, id_column, drop = TRUE])
        match_row$geometry = NULL
        matches_data[[matches_index]] = data.frame(match_row, stringsAsFactors = FALSE)
        matches_index = matches_index + 1
      }
    }
    matches_data = bind_rows(matches_data)
    match_res[[i]] = matches_data
  }

  return(bind_rows(match_res))
}
