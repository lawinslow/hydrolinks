library(dplyr)
library(sf)
library(foreign)

# args is a named vector with 3 columns: shape path, output layer name, bounding box entry file name
project_and_get_bb = function(args){
  shape_path = args["shape_path"]
  layer = args["layer"]
  output_name = args["output_name"]
  shape = st_read(shape_path)
  shape = st_transform(shape, st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  centroids = st_centroid(shape)
  shape$centroid.x = st_coordinates(centroids)[,"X"]
  shape$centroid.y = st_coordinates(centroids)[,"Y"]
  st_write(shape, dsn = shape_path, layer = layer, driver = "ESRI Shapefile", update=TRUE)
  ret = st_sf(file = output_name,
              geometry=st_as_sfc(st_bbox(shape), crs=nhd_projected_proj), stringsAsFactors = FALSE)
  return(ret)
}

build_id_table = function(bbdf, layer, file_name, index_columns){
  ids = list()
  
  for(i in 1:nrow(bbdf)){
    shape = st_read(file.path(bbdf$file[i], layer))
    st_geometry(shape) = NULL
    shape = shape[,index_columns]
    shape$file = bbdf$file[i]
    ids[[i]] = shape
  }
  id_lookup = bind_rows(ids)
  db = src_sqlite(file_name, create = TRUE)
  copy_to(db, id_lookup, overwrite = TRUE, temporary = FALSE, indexes = c(index_columns, "file"))
}

format_flowtable = function(raw_tables, shape_directories, wbarea_column, from_column, to_column, id_column){
  changes = list()
  
  for(i in 1:length(shape_directories)){
    file = shape_directories[i]
    flowline = st_read(file.path(shape_directories[i], "NHDFlowline_projected.shp"))
    waterbody = st_read(file.path(shape_directories[i], "NHDWaterbody_projected.shp"))
    flowline = flowline[!is.na(flowline[,wbarea_column]),]
    flowline = flowline[flowline[,wbarea_column] %in% waterbody[,id_column],]
    change = data.frame(flowline[,id_column], flowline[,wbarea_column])
    changes[[i]] = change
  }
  
  tables = list()
  
  for(i in 1:length(raw_tables)){
    tables[[i]] = read.dbf(raw_tables[i])
    changes_from = changes[[i]][changes[[i]][,]]
  }
}

