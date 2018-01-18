library(sf)
library(dplyr)

build_id_table = function(bbdf, file_name, index_columns){
  ids = list()
  
  for(i in 1:nrow(bbdf)){
    shape = st_read(bbdf$file[i])
    st_geometry(shape) = NULL
    ids[[i]] = shape
  }
  id_lookup = bind_rows(ids)
  db = src_sqlite(file_name, create = TRUE)
  copy_to(db, id_lookup, overwrite = TRUE, temporary = FALSE, indexes = index_columns)
}

load("inst/extdata/nhd_bb_cache.RData")

build_id_table()
