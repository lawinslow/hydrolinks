library(sf)
source("R/AAA.R")
source("inst/extR/general_functions.R")

hydrolakes_path = "D:/hydrolakes/HydroLAKES_polys_v10_shp"

id_table_output_path = "D:/hydrolinks_tables"

hydrolakes = st_read(file.path(hydrolakes_path, "HydroLAKES_polys_v10.shp"))

bboxes = list()
for(i in c(1:50)){
  if(i < 50){
    slice = hydrolakes[c((28000 * (i-1) + 1):(28000*i)),]
  }
  else{
    slice = hydrolakes[c(1372001:1427688),]
  }
  dir.create(file.path(hydrolakes_path, paste0("hydrolakes_", i)))
  slice = st_transform(slice, nhd_projected_proj)
  centroids = st_centroid(slice)
  slice$centroid.x = st_coordinates(centroids)[,"X"]
  slice$centroid.y = st_coordinates(centroids)[,"Y"]
  st_write(slice, dsn = file.path(hydrolakes_path, paste0("hydrolakes_", i)), layer = "HydroLAKES_polys_v10_projected", 
           driver = "ESRI Shapefile")
  bboxes[[i]] = st_sf(file = paste0("hydrolakes_", i, ".zip"), geometry=st_as_sfc(st_bbox(slice), crs=nhd_projected_proj), stringsAsFactors = FALSE) 
}

bbdf = do.call(rbind, bboxes)
save(bbdf, file='inst/extdata/hydrolakes_bb_cache.Rdata')

dir.create(file.path(hydrolakes_path, "zip"))
output_zip = file.path(hydrolakes_path, "zip", paste0("hydrolakes_", c(1:50), ".zip"))
for(i in 1:50){
  setwd(file.path(hydrolakes_path, paste0("hydrolakes_", i)))
  zip(output_zip[i], Sys.glob("*_projected.*"))
}

setwd(hydrolakes_path)
build_id_table(bbdf, "HydroLAKES_polys_v10_projected.shp", file.path(id_table_output_path, "hydrolakes_waterbody_ids.sqlite3"), c("Hylak_id", "Lake_name"), paste0("hydrolakes_", c(1:50)))

processed_shapes = upload_data(output_zip, "upload_conf.csv", "hydrolinks/hydrolakes")
write.csv(processed_shapes, "inst/extdata/hydrolakes.csv")
