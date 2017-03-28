library(nhdtools)
library(rgdal)
library(parallel)

zipfiles = Sys.glob(file.path(local_storage(), 'HU4', 'Shape', '*Shape.zip'))
dest = file.path(local_storage(), 'HU4', 'Shape_unzip')

unzip_wbd = function(zipfile){

  subdest = file.path(dest, basename(zipfile))
  unzip(zipfile = zipfile, files = paste0('Shape/NHDWaterbody.', c('dbf', 'prj', 'shp', 'shx')),
        exdir = subdest)
}

lapply(zipfiles, unzip_wbd)


shapefiles = Sys.glob(file.path(local_storage(), 'HU4', 'Shape_unzip', '*', 'Shape', 'NHDWaterbody.shp'))

getbb = function(shapefile){
  tmp = readOGR(shapefile)
  return(tmp@bbox)
}


c1 = makePSOCKcluster(rep('localhost', 8))
parallel::clusterEvalQ(c1, {library(rgdal)})
bboxes = parLapplyLB(c1, shapefiles, getbb)


bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in 1:length(bboxes)){
  bbdf[i,2:3] = bboxes[[i]][1,]
  bbdf[i,4:5] = bboxes[[i]][2,]
  bbdf[i,1] = shapefiles[i]
}

save(bbdf, file='inst/extdata/nhd_bb_cache.Rdata')
