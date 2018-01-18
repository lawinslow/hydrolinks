library(rgeos)
library(rgdal)

hydrolakes_path = "hydrolakes/HydroLAKES_polys_v10_shp/"

hydrolakes = readOGR(file.path(hydrolakes_path, "HydroLAKES_polys_v10.shp"))
centroids = gCentroid(hydrolakes, byid = TRUE)
centroids = centroids[order(centroids$x)]

bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in c(1:50)){
  if(i < 50){
    slice = hydrolakes[rownames(centroids@coords[c((28000 * (i-1) + 1):(28000*i)),]),]
  }
  else{
    slice = hydrolakes[rownames(centroids@coords[c(1372001:1427688),]),]
  }
  dir.create(file.path(hydrolakes_path, paste0("hydrolakes_", i)))
  writeOGR(slice, dsn = file.path(hydrolakes_path, paste0("hydrolakes_", i)), layer = "HydroLAKES_polys_v10", 
           driver = "ESRI Shapefile")
  bbdf[i,2:3] = slice@bbox[1,]
  bbdf[i,4:5] = slice@bbox[2,]
  bbdf[i,1] = paste0("hydrolakes_", i, ".zip")
}

save(bbdf, file='inst/extdata/hydrolakes_bb_cache.Rdata')
