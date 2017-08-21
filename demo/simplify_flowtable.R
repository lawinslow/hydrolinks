library(readr)
library(tools)
library(dplyr)

gdbs = Sys.glob(file.path("D:", "lakes", "NHD", "unzip", "*", "*GDB.gdb"))
dest = file.path("D:", "lakes", "NHD", "unzip")

tables = list()

load("changes.RData")

for(i in 1:length(gdbs)){
  tables[[i]] = read_csv(paste0(file_path_sans_ext(dirname(gdbs[i])), ".csv"), 
                         col_types = list(col_character(), col_character(), col_character(), col_character()))
  changes_from = changes[[i]][changes[[i]]$shape.PERMANENT_ %in% tables[[i]]$From_Permanent_Identifier, ]
  changes_to = changes[[i]][changes[[i]]$shape.PERMANENT_ %in% tables[[i]]$To_Permanent_Identifier, ]
  changes_from = rename(changes_from, From_Permanent_Identifier = shape.PERMANENT_)
  tables[[i]] = left_join(tables[[i]], changes_from, by="From_Permanent_Identifier")
  tables[[i]]$From_Permanent_Identifier[!is.na(tables[[i]]$shape.WBAREA_PER)] = as.character(tables[[i]]$shape.WBAREA_PER[!is.na(tables[[i]]$shape.WBAREA_PER)])
  tables[[i]]$shape.WBAREA_PER = NULL
  changes_to = rename(changes_to, To_Permanent_Identifier = shape.PERMANENT_)
  tables[[i]] = left_join(tables[[i]], changes_to, by="To_Permanent_Identifier")
  tables[[i]]$To_Permanent_Identifier[!is.na(tables[[i]]$shape.WBAREA_PER)] = as.character(tables[[i]]$shape.WBAREA_PER[!is.na(tables[[i]]$shape.WBAREA_PER)])
  tables[[i]]$shape.WBAREA_PER = NULL
  tables[[i]] = tables[[i]][tables[[i]]$From_Permanent_Identifier != tables[[i]]$To_Permanent_Identifier,]
}

flowtable = bind_rows(tables)


load("distances.RData")
distances = rename(distances, From_Permanent_Identifier = PERMANENT_)
flowtable = left_join(flowtable, distances, by="From_Permanent_Identifier")
flowtable = flowtable[,c(3,4,5)]
ids_db = src_sqlite("flowtable.sqlite3", create = TRUE)
copy_to(ids_db, flowtable, overwrite = TRUE, temporary = FALSE, indexes = list("From_Permanent_Identifier","To_Permanent_Identifier"))
