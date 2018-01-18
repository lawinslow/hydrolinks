library(rgdal)
library(tools)
library(readr)
library(dplyr)
library(hydrolinks)

load("inst/extdata/nhd_bb_streams_cache.Rdata")
changes = list()
for(i in 1:length(bbdf_streams$file)){
  file = bbdf_streams$file[i]
  check_dl_file(system.file("extdata/nhdh.csv", package="nhdtools"), fname = file)
  shape = readOGR(file.path(local_path(), "unzip", file, "NHDFlowline_projected.shp"))
  waterbody = readOGR(file.path(local_path(), "unzip", file, "NHDWaterbody_projected.shp"))
  shape = shape[!is.na(shape$WBAREA_PER),]
  shape = shape[shape$WBAREA_PER %in% waterbody$PERMANENT_,]
  change = data.frame(shape$PERMANENT_, shape$WBAREA_PER)
  changes[[i]] = change
}
save(changes, file = "changes.RData")

zipfiles = Sys.glob(file.path("D:", "lakes", "NHD", "GDB", "*GDB.zip"))
dest = file.path("D:", "lakes", "NHD", "unzip")

unzip_gdb = function(zipfile){
  subdest = file.path(dest, basename(zipfile))
  unzip(zipfile, exdir = subdest)
}

lapply(zipfiles, unzip_gdb)

gdbs = Sys.glob(file.path("D:", "lakes", "NHD", "unzip", "*", "*GDB.gdb"))

extract_flowtable = function(gdb){
  filename = file_path_sans_ext(basename(gdb))
  system(paste0("ogr2ogr -f CSV ", path.expand(file.path("D:", "lakes", "NHD", "unzip", paste0(filename, ".csv"))), " ", gdb, " NHDFlow"))
}

lapply(gdbs, extract_flowtable)

tables = list()

for(i in 1:length(gdbs)){
  tables[[i]] = read_csv(paste0(file_path_sans_ext(dirname(gdbs[i])), ".csv"), 
                         col_types = list(col_character(), col_character(), col_character(), col_character()))
  changes_from = changes[[i]][changes[[i]]$shape.PERMANENT_ %in% tables[[i]]$From_Permanent_Identifier, ]
  changes_to = changes[[i]][changes[[i]]$shape.PERMANENT_ %in% tables[[i]]$To_Permanent_Identifier, ]
  for(j in 1:nrow(changes_from)){
    tables[[i]][tables[[i]]$From_Permanent_Identifier == changes_from$shape.PERMANENT_[j], ] = changes_from$shape.WBAREA_PER[j]
    tables[[i]][tables[[i]]$To_Permanent_Identifier == changes_to$shape.PERMANENT_[j], ] = changes_to$shape.WBAREA_PER[j]
  }
}

flowtable = rbindlist(tables)

flowtable = flowtable[,c(3,4)]
files = list()
# id_db = src_sqlite(file.path(local_path(), "unzip", "flowline_ids.zip", "flowline_ids.sqlite3"))
# files = sapply(flowtable$From_Permanent_Identifier, function(x){
#   shape = id_db %>%
#     tbl("flowline_ids") %>%
#     filter(PERMANENT_ %in% x) %>%
#     collect()
#   if(is.character(shape$file)){
#     files[[shape$file[1]]] = c(files[[shape$file[1]]], x)
#   }
#   return(files)
# })
load("inst/extdata/nhd_bb_streams_cache.Rdata")
distances = list()
for(i in 1:length(bbdf_streams$file)){
  check_dl_file(system.file("extdata/nhdh.csv", package="nhdtools"), fname = bbdf_streams$file[i])
  shapes = readOGR(file.path(local_path(), "unzip", bbdf_streams$file[i], "NHDFlowline_projected.shp"))
  distances[[i]] = data.frame(shapes$PERMANENT_, shapes$LENGTHKM)
}
distances = rbindlist(distances)
colnames(distances) = c("PERMANENT_", "LENGTHKM")
distances = rename(distances, PERMANENT_ = From_Permanent_Id)
flowtable = merge(flowtable, distances, by="From_Permanent_Identifier")
ids_db = src_sqlite("flowtable_nhdh.sqlite3", create = TRUE)
copy_to(ids_db, flowtable, overwrite = TRUE, temporary = FALSE, indexes = list("From_Permanent_Identifier","To_Permanent_Identifier"))
