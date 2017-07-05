library(nhdtools)
library(rgdal)
library(parallel)

zipfiles = Sys.glob(file.path(local_storage(), 'HU4', 'Shape', '*Shape.zip'))
dest = file.path(local_storage(), 'HU4', 'Shape_unzip')

unzip_wbd = function(zipfile){

  subdest = file.path(dest, basename(zipfile))
  unzip(zipfile = zipfile, files = paste0('Shape/NHDWaterbody.', c('dbf', 'prj', 'shp', 'shx')),
        exdir = subdest)
  unzip(zipfile = zipfile, files = paste0(c('Shape/NHDFlowline.'), c('dbf', 'prj', 'shp', 'shx')),
        exdir = subdest)
}
  
lapply(zipfiles, unzip_wbd)


shapefiles = Sys.glob(file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDWaterbody.shp'))
shapefiles_streams = Sys.glob(file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDFlowline.shp'))

getbb = function(shapefile){
  tmp = readOGR(shapefile)
  return(tmp@bbox)
}

getbb_streams = function(shapefile){
  tmp = readOGR(shapefile)
  tmp = spTransform(tmp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  writeOGR(tmp, dsn = dirname(shapefile), layer = "NHDFlowline_projected", driver = "ESRI Shapefile")
  return(tmp@bbox)
}

 
c1 = makePSOCKcluster(rep('localhost', 8))
parallel::clusterEvalQ(c1, {library(rgdal)})
bboxes = parLapplyLB(c1, shapefiles, getbb)
bboxes_streams = parLapplyLB(c1, shapefiles_streams, getbb_streams)

bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in 1:length(bboxes)){
  bbdf[i,2:3] = bboxes[[i]][1,]
  bbdf[i,4:5] = bboxes[[i]][2,]
  bbdf[i,1] = basename(dirname(dirname(shapefiles[i])))
}

bbdf_streams = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in 1:length(bboxes_streams)){
  bbdf_streams[i,2:3] = bboxes_streams[[i]][1,]
  bbdf_streams[i,4:5] = bboxes_streams[[i]][2,]
  bbdf_streams[i,1] = basename(dirname(dirname(shapefiles_streams[i])))
}
bbdf_streams$file = gsub("NHDFlowline.shp", "NHDFlowline_projected.shp", bbdf_streams$file)
save(bbdf_streams, file='inst/extdata/nhd_bb_streams_cache.Rdata')
save(bbdf, file='inst/extdata/nhd_bb_cache.Rdata')
