library(dataRetrieval)
library(nhdtools)

sites = whatWQPsites(siteType ="Lake, Reservoir, Impoundment")

nhd = link_to_waterbodies(sites$LatitudeMeasure, sites$LongitudeMeasure, sites$MonitoringLocationIdentifier)

conus = USAboundaries::cb_2014_us_state_20m[!USAboundaries::cb_2014_us_state_20m@data$geoid %in% c('72', '02', '15'),]
wqp_points = SpatialPoints(cbind(sites$LongitudeMeasure, sites$LatitudeMeasure),
                           proj4string = conus@proj4string)
wqp_conus = over(wqp_points, conus)
wqp_conus = wqp_conus[!is.na(wqp_conus$statefp),]
wqp_points = wqp_points[c(1:length(wqp_points)) %in% rownames(wqp_conus), ]


difference_sites = sites[!(sites$MonitoringLocationIdentifier %in% nhd$MATCH_ID),]
difference_points = SpatialPoints(cbind(difference_sites$LongitudeMeasure, difference_sites$LatitudeMeasure),
                                  proj4string = conus@proj4string)
difference_conus = over(difference_points, conus)
difference_conus = difference_conus[!is.na(difference_conus$statefp),]
difference_points = difference_points[c(1:length(difference_points)) %in% rownames(difference_conus),]

png('figures/us_wqp_sites.png', res=300, width=2400, height=1600)
plot(USAboundaries::cb_2014_us_state_20m[!USAboundaries::cb_2014_us_state_20m@data$geoid %in% c('72', '02', '15'),])
points(wqp_points, cex = 0.1)
dev.off()


png('figures/us_wqp_nomatch.png', res=300, width=2400, height=1600)
plot(USAboundaries::cb_2014_us_state_20m[!USAboundaries::cb_2014_us_state_20m@data$geoid %in% c('72', '02', '15'),])
points(difference_points, cex = 0.1)
dev.off()

