## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))
knitr::opts_chunk$set(eval = !is_check)


## ----packages_sites, eval=TRUE, echo=TRUE, warning=FALSE, include=TRUE, message=FALSE----
library(dataRetrieval)
library(hydrolinks)
library(USAboundaries)
#library(ggplot2)
#library(gridExtra)
library(sf)
hydrolinks::cache_set_dir(temppath = TRUE)

lsites = whatWQPsites(siteType ="Lake, Reservoir, Impoundment", statecode='US:55')

## ----link, eval=TRUE, echo=TRUE, warning=FALSE, include=TRUE, message=FALSE----

linked_lakes = link_to_waterbodies(lsites$LatitudeMeasure, lsites$LongitudeMeasure, lsites$MonitoringLocationIdentifier, dataset = 'nhdh', buffer = 0)

unlinked = subset(lsites, !(MonitoringLocationIdentifier %in% linked_lakes$MATCH_ID))
#now, try linking just the previously unlinked sites
linked_lcent = link_waterbody_centroids(unlinked$LatitudeMeasure, unlinked$LongitudeMeasure, unlinked$MonitoringLocationIdentifier, dataset = 'nhdh', buffer = 25)

## ----build_table, echo=FALSE, message=FALSE, warning=FALSE---------------
library(knitr)
library(kableExtra)
tbl = data.frame(Link=c('Point-in-polygon', 'Centroid', 'Unlinked', 'Total'), 
                 Count=c(length(unique(linked_lakes$MATCH_ID)), length(unique(linked_lcent$MATCH_ID)), nrow(subset(lsites, !(MonitoringLocationIdentifier %in% linked_lakes$MATCH_ID) & !(MonitoringLocationIdentifier %in% linked_lcent$MATCH_ID))), length(unique(lsites$MonitoringLocationIdentifier))))

kable(tbl, 'html') %>%   kable_styling()

## ----mapfigs, echo=TRUE, warning=FALSE, message=FALSE, fig.width=8, fig.height=8----
wi = USAboundaries::us_boundaries()[USAboundaries::us_boundaries()$geoid %in% c('55'),]
all_points = st_as_sf(lsites, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

par(mfrow=c(2,2), mar=c(0,0,0,0))
plot(st_geometry(wi))
plot(st_geometry(all_points), col='darkorange2', add=TRUE, pch=16)
legend('topright', legend=' ', title='a) all WQP points', bty='n')

pippoints = subset(all_points, MonitoringLocationIdentifier %in% linked_lakes$MATCH_ID)
plot(st_geometry(wi))
plot(st_geometry(pippoints), col='darkorchid3', add=TRUE, pch=16)
legend('topright', legend=' ', title='b) all linked point-in-poly', bty='n')

centpoints = subset(all_points, MonitoringLocationIdentifier %in% linked_lcent$MATCH_ID)
plot(st_geometry(wi))
plot(st_geometry(centpoints), col='darkorchid3', add=TRUE, pch=16)
legend('topright', legend=' ', title='c) all linked centroid', bty='n')


# gall = ggplot(all_points) + 
#   geom_sf(data=wi, fill='grey') +
#   geom_sf(size=0.5, color='darkorange2') + ylim(42, 47) + xlim(-93, -86.5) +
#   ggtitle('a) all WQP points')
# 
# glink = ggplot(subset(all_points, MonitoringLocationIdentifier %in% linked_lakes$MATCH_ID)) +
#   geom_sf(data=wi, fill='grey') +
#   geom_sf(size=0.5, color='dodgerblue') + ylim(42, 47) + xlim(-93, -86.5) +
#   ggtitle('b) all linked point-in-poly')
# 
# gcent = ggplot(subset(all_points, MonitoringLocationIdentifier %in% linked_lcent$MATCH_ID)) +
#   geom_sf(data=wi, fill='grey') +
#   geom_sf(size=0.5, color='darkorchid3') + ylim(42, 47) + xlim(-93, -86.5) +
#   ggtitle('c) all linked centroid')
# 
# 
# grid.arrange(gall, glink, gcent, nrow = 2)




