library(foreign)
library(sf)
library(dplyr)
library(hydrolinks)

load("inst/extdata/nhdplus_flowline_bb_cache.Rdata")
changes = list()
for(i in 1:nrow(bbdf)){
  file = bbdf$file[i]
  check_dl_file(system.file("extdata/nhdplusv2.csv", package="hydrolinks"), fname = file)
  shape = st_read(file.path(cache_get_dir(), "unzip", file, "NHDFlowline_projected.shp"))
  waterbody = st_read(file.path(cache_get_dir(), "unzip", file, "NHDWaterbody_projected.shp"))
  shape = shape[!is.na(shape$WBAREACOMI),]
  shape = shape[shape$WBAREACOMI %in% waterbody$COMID,]
  change = data.frame(shape$COMID, shape$WBAREACOMI)
  changes[[i]] = change
}

nhd_path = "D:/tobi/nhdplus"

regions = c("NE_01", "MA_02", "SA_03N", "SA_03S", "SA_03W", "GL_04", "MS_05", "MS_06", "MS_07", "MS_08",
            "MS_10U", "MS_10L", "MS_11", "SR_09", "TX_12", "RG_13", "CO_14", "CO_15", "GB_16", "PN_17", "CA_18",
            "HI_20", "CI_21", "PI_22AS", "PI_22GU", "PI_22MP")

dest = file.path(nhd_path, "unzip")

for(i in 1:length(regions)){
  system("cmd.exe", input = paste0("\"C:\\Program Files\\7-Zip\\7z.exe\" e -o\"", file.path(dest, regions[i]), "\" ", 
                                   Sys.glob(file.path(nhd_path, paste0("NHDPlusV21_", regions[i],"_NHDPlusAttributes_*", ".7z")))))
}

flow_dbfs = file.path(dest, regions, "PlusFlow.dbf")

tables = list()

for(i in 1:length(regions)){
  tables[[i]] = read.dbf(flow_dbfs[i])
  changes_from = changes[[i]][changes[[i]]$shape.COMID %in% tables[[i]]$FROMCOMID, ]
  changes_to = changes[[i]][changes[[i]]$shape.COMID %in% tables[[i]]$TOCOMID, ]
  if(nrow(changes_from) == 0){
    next
  }
  for(j in 1:nrow(changes_from)){
    tables[[i]][tables[[i]]$FROMCOMID == changes_from$shape.COMID[j], ]$FROMCOMID = changes_from$shape.WBAREACOMI[j]
    tables[[i]][tables[[i]]$TOCOMID == changes_to$shape.COMID[j], ]$TOCOMID = changes_to$shape.WBAREACOMI[j]
  }
}

flowtable = bind_rows(tables)
save(flowtable, "nhdplusv2_flowtable_complete.RData")
flowtable = flowtable[,c(1,4)]

distances = list()
for(i in 1:length(bbdf$file)){
  check_dl_file(system.file("extdata/nhdplusv2.csv", package="hydrolinks"), fname = bbdf$file[i])
  shapes = st_read(file.path(cache_get_dir(), "unzip", bbdf$file[i], "NHDFlowline_projected.shp"))
  distances[[i]] = data.frame(shapes$COMID, shapes$LENGTHKM)
}

distances = bind_rows(distances)
colnames(distances) = c("FROMCOMID", "LENGTHKM")
flowtable = merge(flowtable, distances, by="FROMCOMID")
ids_db = src_sqlite("flowtable_nhdplusv2.sqlite3", create = TRUE)
copy_to(ids_db, flowtable, overwrite = TRUE, temporary = FALSE, indexes = list("FROMCOMID","TOCOMID"))

