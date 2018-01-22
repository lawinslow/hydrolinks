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

format_flowtable = function(raw_tables, shape_directories, wbarea_column, from_column, to_column, id_column, output_name){
  changes = list()
  
  for(i in 1:length(shape_directories)){
    file = shape_directories[i]
    flowline = st_read(file.path(shape_directories[i], "NHDFlowline_projected.shp"))
    waterbody = st_read(file.path(shape_directories[i], "NHDWaterbody_projected.shp"))
    st_geometry(flowline) = NULL
    st_geometry(waterbody) = NULL
    flowline = flowline[!is.na(flowline[,wbarea_column]),]
    flowline = flowline[flowline[,wbarea_column] %in% waterbody[,id_column],]
    change = data.frame(flowline[,id_column], flowline[,wbarea_column], stringsAsFactors = FALSE)
    colnames(change) = c("id_column", "wbarea_column")
    change$id_column = as.character(change$id_column)
    change$wbarea_column = as.character(change$wbarea_column)
    changes[[i]] = change
  }
  
  tables = list()
  
  for(i in 1:length(raw_tables)){
    tables[[i]] = read.dbf(raw_tables[i], as.is = TRUE)
    
    changes_from = changes[[i]][changes[[i]][,"id_column"] %in% tables[[i]][,from_column], ]
    changes_to = changes[[i]][changes[[i]][,"id_column"] %in% tables[[i]][,to_column], ]
    
    if(nrow(changes_from) == 0){
      next
    }
    
    for(j in 1:nrow(changes_from)){
      tables[[i]][which(tables[[i]][,from_column] %in% changes_from$id_column[j]), from_column] = changes_from$wbarea_column[j]
      tables[[i]][which(tables[[i]][,to_column] %in% changes_to$id_column[j]), to_column] = changes_to$wbarea_column[j]
    }
  }
  
  flowtable = bind_rows(tables)
  save(flowtable, file = paste0(output_name, "_complete.Rdata"))
  flowtable = flowtable[,c(from_column, to_column)]
  
  distances = list()
  
  for(i in 1:length(shape_directories)){
    flowline = st_read(file.path(shape_directories[i], "NHDFlowline_projected.shp"))
    st_geometry(flowline) = NULL
    distances[[i]] = data.frame(flowline[,id_column], flowline$LENGTHKM)
  }
  
  distances = bind_rows(distances)
  colnames(distances) = c(from_column, "LENGTHKM")
  flowtable = merge(flowtable, distances, by = from_column)
  ids_db = src_sqlite(paste0(output_name, ".sqlite3"), create = TRUE)
  copy_to(ids_db, flowtable, overwrite = TRUE, temporary = fALSE, indexes = list(from_column, to_column))
  
}

