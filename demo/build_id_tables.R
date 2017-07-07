library(rgdal)
library(dplyr)
source('R/check_dl_file.R')

load(file=system.file('extdata/nhd_bb_cache.Rdata', package='nhdtools'))

files = bbdf$file

waterbody_ids = data.frame()

for(i in 1:length(files)){
  shape = readOGR(file.path(local_path(), "unzip", files[i], "NHDWaterbody.shp"))
  waterbody_ids = rbind(waterbody_ids, data.frame(shape$PERMANENT_, shape$GNIS_ID, shape$GNIS_NAME, shape$REACHCODE, files[i]))
}
colnames(waterbody_ids) = c("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE", "file")

waterbody_ids_db = src_sqlite("waterbody_ids.sqlite3", create = T)
copy_to(waterbody_ids_db, waterbody_ids, overwrite = TRUE, temporary = FALSE, 
        indexes = list("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE"))
rm(waterbody_ids_db)
gc()

flowline_ids = data.frame()
for(i in 1:length(files)){
  shape = readOGR(file.path(local_path(), "unzip", files[i], "NHDFlowline.shp"))
  flowline_ids = rbind(flowline_ids, data.frame(shape$PERMANENT_, files[i]))
}
colnames(flowline_ids) = c("PERMANENT_", "file")
flowline_ids_db = src_sqlite("flowline_ids.sqlite3", create = TRUE)
copy_to(flowline_ids_db, flowline_ids, overwrite = TRUE, temporary = FALSE, indexes = list("PERMANENT_"))
rm(flowline_ids_db)
gc()
