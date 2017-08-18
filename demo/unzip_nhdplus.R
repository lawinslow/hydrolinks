library(rgdal)
library(parallel)

nhd_path = "D:\\lakes\\NHDPlusV21\\Data"

regions = c("CA", "CI", "CO\\NHDPlus14", "CO\\NHDPlus15", "MS\\NHDPlus10L", "MS\\NHDPlus10U", "PI\\NHDPlus22MP",
            "GB", "GL", "HI", "MA", "NE", "PN", "RG", "SA\\NHDPlus03W", "SR", "TX")

dest = file.path(nhd_path, "unzip")

zipfiles = file.path(nhd_path, paste0("NHDPlus", regions))

for(i in 1:length(zipfiles)){
  system("cmd.exe", input = paste0("\"C:\\Program Files\\7-Zip\\7z.exe\" e -o\"", file.path(dest, regions[i]), "\" ", 
                Sys.glob(file.path(zipfiles[i], paste0("*NHDSnapshot_*", ".7z")))))
}

waterbody_shapes = file.path(dest, regions, "NHDWaterbody.shp")
flowline_shapes = file.path(dest, regions, "NHDFlowline.shp")

getbb = function(shapefile, layername){
  tmp = readOGR(shapefile)
  tmp = spTransform(tmp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  writeOGR(tmp, dsn = dirname(shapefile), layer = layername, driver = "ESRI Shapefile")
  return(tmp@bbox)
}

c1 = makePSOCKcluster(rep('localhost', 8))
parallel::clusterEvalQ(c1, {library(rgdal)})
bboxes_waterbody = parLapplyLB(c1, waterbody_shapes, getbb, "NHDWaterbody_projected")
bboxes_flowline = parLapplyLB(c1, flowline_shapes, getbb, "NHDFlowline_projected")

bbdf_waterbody = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in 1:length(bboxes_waterbody)){
  bbdf_waterbody[i,2:3] = bboxes_waterbody[[i]][1,]
  bbdf_waterbody[i,4:5] = bboxes_waterbody[[i]][2,]
  bbdf_waterbody[i,1] = paste0(basename(dirname(waterbody_shapes[i])), ".zip")
}

bbdf_flowline = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
for(i in 1:length(bboxes_flowline)){
  bbdf_flowline[i,2:3] = bboxes_waterbody[[i]][1,]
  bbdf_flowline[i,4:5] = bboxes_waterbody[[i]][2,]
  bbdf_flowline[i,1] = paste0(basename(dirname(flowline_shapes[i])), ".zip")
}

save(bbdf_waterbody, file = "inst/extdata/nhdplus_waterbody_bb_cache.rdata")
save(bbdf_flowline, file = "inst/extdata/nhdplus_flowline_bb_cache.rdata")

dir.create(file.path(nhd_path, "zip"))
output_zip = file.path(nhd_path, "zip", paste0(basename(regions), ".zip"))
for(i in 1:length(regions)){
  setwd(file.path(dest, regions[i]))
  zip(output_zip[i], Sys.glob("*_projected.*"))
}
