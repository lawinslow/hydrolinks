context("Testing simple lake linking for each of the current linking types")

test_that("Test non-buffer, buffer, and centroid lake linking", {

  skip_on_cran()

  expect_equal(nrow(link_to_waterbodies(30.35413, -84.3801, 'id1')), 0)

  expect_equal(nrow(link_to_waterbodies(30.35413, -84.3801, 'id1', buffer=25)), 1)

  expect_equal(nrow(link_to_waterbodies(33.655277, -117.834007, 'id1')), 0)

  expect_equal(nrow(link_waterbody_centroids(33.655277, -117.834007, 'id1')), 1)

})


test_that("test PIP linking across different datasets", {

  skip_on_cran()

  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'nhdh')), 1)
  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'hydrolakes')), 1)
  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'nhdplusv2')), 1)

})


test_that("test ID lookup of polygons for different datasets", {

  skip_on_cran()
  skip_on_travis()

  shape = get_shape_by_id('13293262', feature_type = 'waterbody', dataset='nhdplusv2')
  #should be a plot of mendota
  plot(st_geometry(shape))

  shape = get_shape_by_id('9086', feature_type = 'waterbody', dataset='hydrolakes')
  #should be a different plot of mendota
  plot(st_geometry(shape))

  shape = get_shape_by_id('143249470', feature_type = 'waterbody', dataset='nhdh')
  #should be a different plot of mendota
  plot(st_geometry(shape))

})
