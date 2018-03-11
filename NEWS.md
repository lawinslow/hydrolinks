# hydrolinks v0.5.0
* Added `NEWS.md` file to track changes
* Initial journal and CRAN submission
* Currently includes a few key datasets
* NHDH national hydrography dataset
* hydrolakes Global lake polygons
* NHDplusv2 Extended metadata 1:100k lake layer

# hydrolinks 0.6.3
* Bunch of improvements
* Fixed centroid linking. Normalized code across linking procedures

# hydrolinks 0.6.3
* Changed output for `cache_info` to include raw data and print more summary info
* Added initial WQP linking example Vignette
* Silenced the loading of spatial data in the linking procedures

# hydrolinks v0.7.1
* Improvements in response to Environmental Modeling and Software
* New and improved vignettes to highlight functionality
* Exposed several new functions to improve caching 
* Added cache handling functions to enable better cache transparency
* Many other minor bugfixes and improvements

# hydrolinks v0.9.1
* Fixed problem #42 with flowline traversal with nhdplusv2
* Improved workflow for data generation
* Faster network creation and more organized, unified file organization
* Normalized all data colnames to lowercase to fix #43 and potential future issues
* Some test and vignette cleanup based on above changes
* Minted new data release with all above changes incorporated