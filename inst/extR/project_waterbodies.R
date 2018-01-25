library(sf)
library(parallel)
library(hydrolinks)
library(tools)

shapes_nhdh = all_shapefiles(dataset = "nhdh", feature_type = "waterbody")
shapes_nhdplusv2 = all_shapefiles(dataset = "nhdplusv2", feature_type = "waterbody")
shapes_hydrolakes = all_shapefiles(dataset = "hydrolakes", feature_type = "waterbody")

project_waterbodies = function(waterbody, layer){
  #check_dl_file(system.file("extdata/nhdplusv2.csv", package = "hydrolinks"), basename(dirname(waterbody)))
  shape = st_read(waterbody)
  #shape = st_transform(shape, st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  #centroids = st_centroid(shape)
  #shape$centroid.x = st_coordinates(centroids)[,"X"]
  #shape$centroid.y = st_coordinates(centroids)[,"Y"]
  #if(file.exists(waterbody)){
  #  file.remove(Sys.glob("*"))
  #}
  #st_write(shape, dsn = dirname(waterbody), layer = layer, driver = "ESRI Shapefile", update=TRUE)
  ret = st_sf(file = basename(dirname(waterbody)),
              geometry=st_as_sfc(st_bbox(shape), crs=nhd_projected_proj), stringsAsFactors = FALSE)
  return(ret)
}

c1 = makePSOCKcluster(rep('localhost', 8))
parallel::clusterEvalQ(c1, {library(hydrolinks)
  library(sf)
  library(tools)})
bboxes = parLapplyLB(c1, shapes_hydrolakes, project_waterbodies, "HydroLAKES_polys_v10_projected")

# bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
# 
# for(i in 1:length(bboxes)){
#   bbdf[i,2:3] = bboxes[[i]][1]
#   bbdf[i,4:5] = bboxes[[i]][2]
#   bbdf[i,1] = basename(shapes[i])
# }
bbdf = do.call(rbind, bboxes)
save(bbdf, file = "inst/extdata/hydrolakes_bb_cache_projected.Rdata")

bboxes = parLapplyLB(c1, shapes_nhdh, project_waterbodies, "NHDWaterbody_projected")
bbdf = do.call(rbind, bboxes)
save(bbdf, file = "inst/extdata/nhd_bb_cache_projected.Rdata")

shapes_nhdh = all_shapefiles(dataset = "nhdh", feature_type = "flowline")
bboxes = parLapplyLB(c1, shapes_nhdh, project_waterbodies, "NHDFlowline_projected")
bbdf = do.call(rbind, bboxes)
save(bbdf, file = "inst/extdata/nhd_bb_streams_cache.Rdata")

nhd_path = "D:/lakes/"

dir.create(file.path(nhd_path, "zip"))
output_zip = file.path(nhd_path, "zip", basename(dirname(shapes_hydrolakes)))
for(i in 1:length(output_zip)){
  setwd(dirname(shapes_hydrolakes[i]))
  zip(output_zip[i], Sys.glob("*_projected.*"))
}

save(bbdf, file = "inst/extdata/nhd_bb_cache_projected.Rdata")
