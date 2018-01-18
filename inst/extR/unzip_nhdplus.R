library(sf)
library(parallel)
library(dplyr)
source("R/AAA.R")

nhd_path = "D:\\tobi\\lakes\\NHDPlusV21"

regions = c("NE_01", "MA_02", "SA_03N", "SA_03S", "SA_03W", "GL_04", "MS_05", "MS_06", "MS_07", "MS_08",
            "MS_10U", "MS_10L", "MS_11", "SR_09", "TX_12", "RG_13", "CO_14", "CO_15", "GB_16", "PN_17", "CA_18",
            "HI_20", "CI_21", "PI_22AS", "PI_22GU", "PI_22MP")

dest = file.path(nhd_path, "unzip")

zipfiles = nhd_path

for(i in 1:length(regions)){
  system("cmd.exe", input = paste0("\"C:\\Program Files\\7-Zip\\7z.exe\" e -o\"", file.path(dest, regions[i]), "\" ", 
                Sys.glob(file.path(zipfiles, paste0("NHDPlusV21_", regions[i],"_NHDSnapshot_*", ".7z")))))
}

waterbody_shapes = file.path(dest, regions, "NHDWaterbody.shp")
flowline_shapes = file.path(dest, regions, "NHDFlowline.shp")

getbb = function(shapefile, layername){
  tmp = st_read(shapefile)
  tmp = st_transform(tmp, st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  tmp = st_zm(tmp)
  #centroids = st_centroid(tmp)
  #tmp$centroid.x = st_coordinates(centroids)[,"X"]
  #tmp$centroid.y = st_coordinates(centroids)[,"Y"]
  st_write(tmp, dsn = file.path(dirname(shapefile), paste0(layername, ".shp")))
  ret = st_sf(file = paste0(basename(dirname(shapefile)), ".zip"), 
              geometry=st_as_sfc(st_bbox(tmp), crs=nhd_projected_proj), stringsAsFactors = FALSE)
  return(ret)
}

c1 = makePSOCKcluster(rep('localhost', 12))
parallel::clusterEvalQ(c1, {library(sf)})
bboxes_waterbody = parLapplyLB(c1, waterbody_shapes, getbb, "NHDWaterbody_projected")
bboxes_flowline = parLapplyLB(c1, flowline_shapes, getbb, "NHDFlowline_projected")

# bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
# for(i in 1:length(bboxes_waterbody)){
#   bbdf[i,2:5] = bboxes_waterbody[[i]]
#   bbdf[i,1] = paste0(basename(dirname(waterbody_shapes[i])), ".zip")
# }
bbdf = do.call(rbind, bboxes_waterbody)
save(bbdf, file = "inst/extdata/nhdplus_waterbody_bb_cache.rdata")

# bbdf = data.frame(file=character(), xmin=numeric(), xmax=numeric(), ymin=numeric(), ymax=numeric(), stringsAsFactors = FALSE)
# for(i in 1:length(bboxes_flowline)){
#   bbdf[i,2:5] = bboxes_waterbody[[i]]
#   bbdf[i,1] = paste0(basename(dirname(flowline_shapes[i])), ".zip")
# }

bbdf = do.call(rbind, bboxes_flowline)
save(bbdf, file = "inst/extdata/nhdplus_flowline_bb_cache.rdata")

dir.create(file.path(nhd_path, "zip"))
output_zip = file.path(nhd_path, "zip", paste0(basename(regions), ".zip"))
for(i in 1:length(regions)){
  setwd(file.path(dest, regions[i]))
  zip(output_zip[i], Sys.glob("*_projected.*"))
}
