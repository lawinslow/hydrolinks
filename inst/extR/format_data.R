id_table_output_path = "D:/hydrolinks_tables"

dir.create(id_table_output_path)

source("inst/extR/unzip_nhd.R")
rm(list=ls())
source("inst/extR/unzip_nhdplus.R")
rm(list=ls())
source("inst/extR/slice_hydrolakes.R")
rm(list=ls())

id_table_output_path = "D:/hydrolinks_tables"
shape_id_caches = Sys.glob(file.path(id_table_output_path, "*_ids.sqlite3"))
shape_id_zips = sapply(shape_id_caches, function(x){
  outzip = paste0(tools::file_path_sans_ext(x), '.zip')
  utils::zip(outzip, files = x, flags='-j')
  return(outzip)
  })

source("inst/extR/general_functions.R")
id_cache_dl = gen_upload_file(shape_id_zips, "hydrolinks/0.8/shape_id_cache")
write.csv(id_cache_dl, file = "inst/extdata/shape_id_cache.csv", row.names=FALSE)
