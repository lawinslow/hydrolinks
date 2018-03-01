library(testthat)
library(hydrolinks)

test_that("traversal out has correct amount of steps and end", {
  skip_on_cran()
  traversal1 = traverse_flowlines(1000, "141329377", "out", "nhdh")
  expect_equal(nrow(traversal1), 2325)
  expect_equal(traversal1$PERMANENT_[2325], "{8B4EA914-C76E-4C60-BBFF-BB46578B1AEE}")
})

test_that("nhdh traversal in has correct amount of steps and end", {
  skip_on_cran()
  traversal_nhdh = traverse_flowlines(50, "149426016", "in", "nhdh")
  expect_equal(nrow(traversal_nhdh), 2624)
  expect_equal(traversal_nhdh$PERMANENT_[2624], "155439697")
})

test_that("nhdplusv2 traversal in has correct amount of steps and end", {
  skip_on_cran()
  traversal_nhdplus = traverse_flowlines(50, "166766701", "in", "nhdplusv2")
  expect_equal(nrow(traversal_nhdplus), 807)
  expect_equal(traversal_nhdplus$COMID[807], "6866189")
})

test_that("traversal returns NA for invalid start ID", {
  skip_on_cran()
  traversal2 = traverse_flowlines(1000, "asdf", "out", "nhdh")
  expect_equal(traversal2$PERMANENT_, NA)
})

test_that("traversal throws error for 0 start ID", {
  skip_on_cran()
  expect_error(traverse_flowlines(1000, "0", "out", "nhdh"), "Cannot traverse from node 0!")
})

test_that("traversing from virtual flowlines continues from enclosing waterbody", {
  skip_on_cran()
  expect_warning(traverse_flowlines(100, "55503985", "out", "nhdh"), "Start ID provided is a virtual flowline inside a waterbody. Continuing from 167745898")
})
