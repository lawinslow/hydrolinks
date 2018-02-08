## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=TRUE, echo=TRUE, warning=FALSE, include=FALSE------------------
library(hydrolinks)
library(sf)
linked_wb = link_to_waterbodies(39.086637, -120.052634, 'lakeobs1', dataset='nhdh')

## ----eval=TRUE, echo=FALSE-----------------------------------------------
print(linked_wb)

## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE, results='hide'----
lake_poly = get_shape_by_id(linked_wb$PERMANENT_, dataset = 'nhdh', feature_type = 'waterbody')
#Simple quick viz of polygon
plot(st_geometry(lake_poly), main='Lake Tahoe', col='Dodgerblue')


## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE, results='hide'----

upstream = traverse_flowlines(50, linked_wb$PERMANENT_, direction = 'in')
upstream_shp = get_shape_by_id(upstream$PERMANENT_, dataset = 'nhdh', feature_type = 'flowline')
plot(st_geometry(upstream_shp), col='palegreen')
plot(st_geometry(lake_poly), main='Lake Tahoe', col='Dodgerblue', add=TRUE)


## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE, results='hide'----

downstream = traverse_flowlines(2000, linked_wb$PERMANENT_, direction = 'out')
downstream_shp = get_shape_by_id(downstream$PERMANENT_, dataset = 'nhdh', feature_type = 'flowline')
downstream_lk_shp = get_shape_by_id(downstream$PERMANENT_, dataset = 'nhdh', feature_type = 'waterbody')
plot(st_geometry(downstream_shp), col='palegreen')
plot(st_geometry(downstream_lk_shp), main='Lake Tahoe', col='Dodgerblue', add=TRUE)
plot(st_geometry(lake_poly), main='Lake Tahoe', col='Dodgerblue', add=TRUE)


## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE, results='hide'----
id = link_to_waterbodies(43.112449, -89.429409, 'mendota')

hmm = traverse_flowlines(100, id$PERMANENT_, "in")

fls = get_shape_by_id(hmm$PERMANENT_, feature_type = 'flowline', dataset='nhdh')
wbs = get_shape_by_id(hmm$PERMANENT_, feature_type = 'waterbody', dataset='nhdh')

wb   = get_shape_by_id(id$PERMANENT_, feature_type = 'waterbody')

plot(st_geometry(fls), col='green')
plot(st_geometry(wb), col='orange', add=TRUE)
plot(st_geometry(fls), add=TRUE, col='green')
plot(st_geometry(wbs), add=TRUE, col='blue')

111

