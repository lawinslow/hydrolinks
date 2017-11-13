library(sf)
library(dplyr)
source('R/check_dl_file.R')

load(file=system.file('extdata/nhd_bb_cache.Rdata', package='hydrolinks'))
files = bbdf$file

id_lookup = data.frame()
id_lookup_rows = list()

for(i in 1:length(files)){
  check_dl_file(system.file("extdata/nhdh.csv", package = "hydrolinks"), fname = files[i])
  shape = st_read(file.path(cache_get_dir(), "unzip", files[i], "NHDWaterbody_projected.shp"))
  shape = shape[,,drop = TRUE]
  shape$geometry = NULL
  colnames(shape) = tolower(colnames(shape))
  id_lookup_rows[[i]] = data.frame(shape$permanent_, shape$gnis_id, shape$gnis_name, shape$reachcode, files[i])
}

id_lookup = bind_rows(id_lookup_rows)

colnames(id_lookup) = c("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE", "file")

waterbody_ids_db = src_sqlite("nhdh_waterbody_ids.sqlite3", create = T)
copy_to(waterbody_ids_db, id_lookup, overwrite = TRUE, temporary = FALSE,
        indexes = list("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE"))
rm(waterbody_ids_db)
gc()

id_lookup = data.frame()
id_lookup_rows = list()

for(i in 1:length(files)){
  shape = st_read(file.path(cache_get_dir(), "unzip", files[i], "NHDFlowline_projected.shp"))
  shape = shape[,,drop = TRUE]
  shape$geometry = NULL
  colnames(shape) = tolower(colnames(shape))
  id_lookup_rows[[i]] = data.frame(shape$permanent_, shape$gnis_id, shape$gnis_name, shape$reachcode, files[i], stringsAsFactors = FALSE)
}

id_lookup = bind_rows(id_lookup_rows)
colnames(id_lookup) = c("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE", "file")
flowline_ids_db = src_sqlite("nhdh_flowline_ids.sqlite3", create = TRUE)
copy_to(flowline_ids_db, id_lookup, overwrite = TRUE, temporary = FALSE, indexes = list("PERMANENT_", "GNIS_ID", "GNIS_NAME", "REACHCODE"))
rm(flowline_ids_db)
gc()


load(file=system.file('extdata/nhdplus_flowline_bb_cache.rdata', package='hydrolinks'))
bbdf = bbdf_flowline
files = bbdf$file

id_lookup = data.frame()
id_lookup_rows = list()

for(i in 1:length(files)){
  check_dl_file(system.file("extdata/nhdplusv2.csv", package = "hydrolinks"), fname = files[i])
  shape = st_read(file.path(cache_get_dir(), "unzip", files[i], "NHDWaterbody_projected.shp"))
  shape = shape[,,drop = TRUE]
  shape$geometry = NULL
  colnames(shape) = tolower(colnames(shape))
  id_lookup_rows[[i]] = data.frame(shape$comid, shape$gnis_id, shape$gnis_name, shape$reachcode, files[i])
}

id_lookup = bind_rows(id_lookup_rows)
colnames(id_lookup) = c("COMID", "GNIS_ID", "GNIS_NAME", "REACHCODE", "file")

waterbody_ids_db = src_sqlite("nhdplusv2_waterbody_ids.sqlite3", create = T)
copy_to(waterbody_ids_db, id_lookup, overwrite = TRUE, temporary = FALSE,
        indexes = list("COMID", "GNIS_ID", "GNIS_NAME", "REACHCODE"))
rm(waterbody_ids_db)
gc()

id_lookup = data.frame()
id_lookup_rows = list()

for(i in 1:length(files)){
  shape = st_read(file.path(cache_get_dir(), "unzip", files[i], "NHDFlowline_projected.shp"))
  shape = shape[,,drop = TRUE]
  shape$geometry = NULL
  colnames(shape) = tolower(colnames(shape))
  id_lookup_rows[[i]] = data.frame(shape$comid, shape$gnis_id, shape$gnis_name, shape$reachcode, files[i])
}

id_lookup = bind_rows(id_lookup_rows)
colnames(id_lookup) = c("COMID", "GNIS_ID", "GNIS_NAME", "REACHCODE", "file")
flowline_ids_db = src_sqlite("nhdplusv2_flowline_ids.sqlite3", create = TRUE)
copy_to(flowline_ids_db, id_lookup, overwrite = TRUE, temporary = FALSE, indexes = list("COMID", "GNIS_ID", "GNIS_NAME", "REACHCODE"))
rm(flowline_ids_db)
gc()


load(file=system.file('extdata/hydrolakes_bb_cache.Rdata', package='hydrolinks'))
files = bbdf$file

id_lookup = data.frame()
id_lookup_rows = list()

for(i in 1:length(files)){
  check_dl_file(system.file("extdata/hydrolakes.csv", package = "hydrolinks"), fname = files[i])
  shape = st_read(file.path(cache_get_dir(), "unzip", files[i], "HydroLAKES_polys_v10_projected.shp"))
  shape = shape[,,drop = TRUE]
  shape$geometry = NULL
  colnames(shape) = tolower(colnames(shape))
  id_lookup_rows[[i]] = data.frame(shape$hylak_id, shape$lake_name, files[i])
}

id_lookup = bind_rows(id_lookup_rows)
colnames(id_lookup) = c("Hylak_id", "Lake_name", "file")

waterbody_ids_db = src_sqlite("hydrolakes_waterbody_ids.sqlite3", create = T)
copy_to(waterbody_ids_db, id_lookup, overwrite = TRUE, temporary = FALSE,
        indexes = list("Hylak_id", "Lake_name"))
rm(waterbody_ids_db)
gc()
