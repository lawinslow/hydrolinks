library(hydrolinks)
library(sf)
library(parallel)
source("inst/extR/general_functions.R")
source("R/AAA.R")

nhdh_path = "E:/nhdh/Shape/"

zipfiles = Sys.glob(file.path(nhdh_path, '*Shape.zip'))
dest = file.path(nhdh_path, 'Shape_unzip')

id_table_output_path = "E:/hydrolinks_tables"

# unzip shapefiles

unzip_wbd = function(zipfile){

  subdest = file.path(dest, basename(zipfile))
  unzip(zipfile = zipfile, files = paste0('Shape/NHDWaterbody.', c('dbf', 'prj', 'shp', 'shx')),
        exdir = subdest)
  unzip(zipfile = zipfile, files = paste0(c('Shape/NHDFlowline.'), c('dbf', 'prj', 'shp', 'shx')),
        exdir = subdest)

}

unzip_flow = function(zipfile){
  subdest = file.path(dest, basename(zipfile))
  unzip(zipfile = zipfile, files = paste0('Shape/NHDFlow.dbf'), exdir = subdest)
}
  
lapply(zipfiles, unzip_wbd)
lapply(zipfiles, unzip_flow)

# project shapefiles and extract bounding boxes

shapefiles_lakes = Sys.glob(file.path(nhdh_path,'Shape_unzip', '*', 'Shape', 'NHDWaterbody.shp'))
shapefiles_streams = Sys.glob(file.path(nhdh_path, 'Shape_unzip', '*', 'Shape', 'NHDFlowline.shp'))

c1 = makePSOCKcluster(rep('localhost', 4))
parallel::clusterEvalQ(c1, {library(sf)})

lake_args = data.frame(shape_path = shapefiles_lakes, layer = rep("NHDWaterbody_projected", length(shapefiles_lakes)),
                        output_name = rep(NA, length(shapefiles_lakes)), stringsAsFactors = FALSE)
for(i in 1:length(shapefiles_lakes)){
  lake_args[i,3] = basename(dirname(dirname(shapefiles_lakes[i])))
}

stream_args = data.frame(shape_path = shapefiles_streams, layer = rep("NHDFlowline_projected", length(shapefiles_streams)),
                       output_name = rep(NA, length(shapefiles_streams)), stringsAsFactors = FALSE)
for(i in 1:length(shapefiles_streams)){
  stream_args[i,3] = basename(dirname(dirname(shapefiles_streams[i])))
}

bboxes_lakes = parApply(c1, lake_args, project_and_get_bb, MARGIN = 1)
#bboxes_lakes = parLapplyLB(c1, c(1:nrow(lake_args)), function(x){project_and_get_bb(lake_args[x,])})
bboxes_streams = parApply(c1, stream_args, project_and_get_bb, MARGIN = 1)
#bboxes_streams = parLapplyLB(c1, c(1:nrow(stream_args)), function(x){project_and_get_bb(stream_args[x,])})

bbdf = do.call(rbind, bboxes_lakes)
save(bbdf, file = "inst/extdata/nhd_bb_cache_projected.Rdata")

bbdf = do.call(rbind, bboxes_streams)
save(bbdf, file = "inst/extdata/nhd_bb_streams_cache.Rdata")

working_directory = getwd()
# save projected shapefiles

dir.create(file.path(nhdh_path, "zip"))
output_zip = file.path(nhdh_path, "zip", basename(dirname(dirname(shapefiles_lakes))))
for(i in 1:length(output_zip)){
  setwd(dirname(shapefiles_lakes[i]))
  zip(output_zip[i], Sys.glob("*_projected.*"))
}


# generate id lookup tables

setwd(dest)
build_id_table(bbdf, "Shape/NHDFlowline_projected.shp", file.path(id_table_output_path, "nhdh_flowline_ids.sqlite3"), c("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE"))

setwd(working_directory)
load("inst/extdata/nhd_bb_cache_projected.Rdata")
setwd(dest)
build_id_table(bbdf, "Shape/NHDWaterbody_projected.shp", file.path(id_table_output_path, "nhdh_waterbody_ids.sqlite3"), c("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE"))

#build flowtable
raw_tables = Sys.glob(file.path(nhdh_path, 'Shape_unzip', '*', 'Shape', 'NHDFlow.dbf'))
shape_directories = Sys.glob(file.path(nhdh_path, 'Shape_unzip', '*', 'Shape'))
format_flowtable(raw_tables, shape_directories, "WBAREA_PER", "FROM_PERMA", "TO_PERMANE", "PERMANENT_", file.path(id_table_output_path, "flowtable_nhdh"))

zip(file.path(id_table_output_path, "flowtable_nhdh.zip"), files = file.path(id_table_output_path, "flowtable_nhdh.sqlite3"))
flowtable_upload = gen_upload_file(file.path(id_table_output_path, "flowtable_nhdh.zip"), "hydrolinks/0.7")

processed_shapes = gen_upload_file(output_zip, "hydrolinks/0.8/nhdh")
write.csv(processed_shapes, "inst/extdata/nhdh.csv")
