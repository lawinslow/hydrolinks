
output_folder = file.path("D:/hydrolinks_upload_cache", packageVersion('hydrolinks'))
remote_path = file.path("http://cdn.bathybase.org/hydrolinks", basename(output_folder))

id_table_output_path = file.path(output_folder, 'shape_id_cache')

dir.create(id_table_output_path, recursive = TRUE)

source("inst/extR/unzip_nhd.R")
rm(list=ls())
source("inst/extR/unzip_nhdplus.R")
rm(list=ls())
source("inst/extR/slice_hydrolakes.R")
rm(list=ls())

shape_id_caches = Sys.glob(file.path(id_table_output_path, "*_ids.sqlite3"))
shape_id_zips = sapply(shape_id_caches, function(x){
  outzip = paste0(tools::file_path_sans_ext(x), '.zip')
  utils::zip(outzip, files = x, flags='-j')
  unlink(x)
  return(outzip)
  })

source("inst/extR/general_functions.R")
id_cache_dl = gen_upload_file(shape_id_zips, file.path(remote_path, 'shape_id_cache'))
write.csv(id_cache_dl, file = "inst/extdata/shape_id_cache.csv", row.names=FALSE, quote=FALSE)
flowtables = Sys.glob(file.path(output_folder, "flowtable_*.zip"))
flowtable_dl = gen_upload_file(flowtables, remote_path)
write.csv(flowtable_dl, file = "inst/extdata/flowtable.csv", row.names = FALSE, quote=FALSE)
