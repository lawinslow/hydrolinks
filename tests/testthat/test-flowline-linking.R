#test_flowline_linking

context("Testing stream linking")

test_that("tests stream linking, with varying buffers", {

  skip_on_cran()

  #hudson river near Albany/Troy NY area
  latlon = c(42.703290, -73.702855)

  expect_equal(nrow(link_to_flowlines(latlon[1], latlon[2], 'dummyid')), 1)

  badpt = c(42.738442, -74.029709) #it is kinda hard to find a point far from any stream
  expect_equal(nrow(link_to_flowlines(badpt[1], badpt[2], 'dummyid')), 0)

})
