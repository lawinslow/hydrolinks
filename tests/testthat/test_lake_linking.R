context("Testing simple lake linking for each of the current linking types")

test_that("Test non-buffer, buffer, and centroid lake linking", {

  expect_equal(nrow(link_to_waterbodies(30.35413, -84.3801, 'id1')), 0)

  expect_equal(nrow(link_to_waterbodies(30.35413, -84.3801, 'id1', buffer=25)), 1)

  expect_equal(nrow(link_to_waterbodies(33.655277, -117.834007, 'id1')), 0)

  expect_equal(nrow(link_waterbody_centroids(33.655277, -117.834007, 'id1')), 1)

})


test_that("test PIP linking across different datasets", {

  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'nhdh')), 1)
  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'hydrolakes')), 1)
  expect_equal(nrow(link_to_waterbodies(43.108728, -89.418293, 'id1', dataset = 'nhdplusv2')), 1)

})
