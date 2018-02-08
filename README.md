hydrolinks R Package
====================

Hydrologic Network Linking Data and Tools for R

[![Build
Status](https://travis-ci.org/lawinslow/hydrolinks.svg?branch=master)](https://travis-ci.org/lawinslow/hydrolinks)
|
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/hydrolinks)](https://cran.r-project.org/package=hydrolinks)

Bug reports and feedback
------------------------

First things first, if you are having an issue, please consider
reporting bugs and submitting feedback via the [Github Issues
page](https://github.com/lawinslow/hydrolinks/issues).

Package Description
-------------------

This package provides tools for linking latitude and longitude
(geopoint) data with hydrologic networks, such as the [U.S. Geologic
Survey's National Hydrologic Dataset (NHD)](http://nhd.usgs.gov) or they
global [hydroLakes](http://www.hydrosheds.org/page/hydrolakes) dataset.
The function automates the access and download of these large,
cumbersome datasets and provides a number of techniques for geographic
data to the hydro networks.

The key functionality `hydrolinks` provides centers on three areas.

1.  Linking geopoint data (lat/lon) to mapped lakes, streams and rivers.
    This provides an unambigous ID-based link to mapped
    aquatic features.

2.  Providing on-demand access to mapped aquatic features based on ID.

3.  Enable traversal of the hydrologic network with full support for
    both lakes and streams.

hydrolinks is available on CRAN. Please install from there.

Example Usage
-------------

### Link to a waterbody feature

Lets say we have an observation `lakeobs1` that was taken at some point.
We want to see if there is a lake at that point, and get the
information, including ID, of that lake.

Lo and behold, this point is over top of Lake Tahoe. We can get further
information about this lake from the mapping layer we used to link.
National Hydrography Dataset Highres (NHDH) has some other useful
information included.

    ##   PERMANENT_      FDATE RESOLUTION  GNIS_ID  GNIS_NAME AREASQKM ELEVATION
    ## 1   44560536 2012-03-12          2 01654975 Lake Tahoe 498.1268      6229
    ##        REACHCODE FTYPE FCODE SHAPE_LENG SHAPE_AREA VISIBILITY centroid_x
    ## 1 16050101000339   390 39009   1.624393 0.05186653          0   -2037514
    ##   centroid_y MATCH_ID
    ## 1    2044681 lakeobs1

Now, lets generate map of the linked lake (Lake Tahoe). Using
hydrolinks, we don't need to dig through shapefiles. We can just use the
ID we got from linking.

    library(sf)
    lake_poly = get_shape_by_id(linked_wb$PERMANENT_, dataset = 'nhdh', feature_type = 'waterbody')
    #Simple quick viz of polygon
    plot(st_geometry(lake_poly), main='Lake Tahoe', col='Dodgerblue')

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

The polygon returned is fully functional, so it could be used for
quantiative analysis of the lake shape or location. We use the `sf`
package throughout for spatial data handling.

Network functionality
---------------------

The latest and greatest feature allows for the traversal of the
hydrologic network. Buildling on what we did above, we can quickly grab
very useful information from the hydrologic network.

For example, lets grab and plot all inflows into Lake Tahoe. We will
start from the lake and traverse up the hydrologic network. We will
specify a max traversal distance of 50km to prevent the traversal from
exploding (can happen with unlimited bounds going up the network, e.g.,
Mississippi).

    upstream = traverse_flowlines(50, linked_wb$PERMANENT_, direction = 'in')
    upstream_shp = get_shape_by_id(upstream$PERMANENT_, dataset = 'nhdh', feature_type = 'flowline')
    plot(st_geometry(upstream_shp), col='palegreen')
    plot(st_geometry(lake_poly), main='Lake Tahoe', col='Dodgerblue', add=TRUE)

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Boom! We have Tahoe and all its input tributaries. Again, the shape data
returned are the same as from the underlying hydrologic network
datasets, so they can be used in spatial and other analyses.
