library(rgdal)
library(rgeos)
library(parallel)

shapes = file.path(local_path(),"unzip", all_nhd_shapefiles())

project_waterbodies = function(waterbody){
  shape = readOGR(file.path(waterbody, "NHDWaterbody.shp"))
  shape = spTransform(shape, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  centroids = gCentroid(shape, byid = TRUE)
  shape$centroid.x = centroids@coords[,"x"]
  shape$centroid.y = centroids@coords[,"y"]
  if(file.exists(file.path(waterbody, "NHDWaterbody_projected.shp"))){
    file.remove(Sys.glob(file.path(waterbody, "NHDWaterbody_projected.*")))
  }
  writeOGR(shape, dsn = waterbody, layer = "NHDWaterbody_projected", driver = "ESRI Shapefile")
  return(shape@bbox)
}

c1 = makePSOCKcluster(rep('localhost', 8))
parallel::clusterEvalQ(c1, {library(rgdal)
  library(rgeos)})
bboxes = parLapplyLB(c1, shapes, project_waterbodies)

bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)

for(i in 1:length(bboxes)){
  bbdf[i,2:3] = bboxes[[i]][1,]
  bbdf[i,4:5] = bboxes[[i]][2,]
  bbdf[i,1] = basename(shapes[i])
}

save(bbdf, file = "inst/extdata/nhd_bb_cache_projected.Rdata")
