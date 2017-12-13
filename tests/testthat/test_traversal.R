library(testthat)
library(hydrolinks)

test_that("traversal out has correct amount of steps and end", {
  traversal1 = traverse_flowlines(1000, "141329377", "out")
  expect_equal(nrow(traversal1), 2325)
  expect_equal(traversal1$PERMANENT_[2325], "{8B4EA914-C76E-4C60-BBFF-BB46578B1AEE}")
})

test_that("traversal returns NA for invalid start ID", {
  traversal2 = traverse_flowlines(1000, "asdf", "out")
  expect_equal(traversal2$PERMANENT_, NA)
})

test_that("traversal throws error for 0 start ID", {
  expect_that(traverse_flowlines(1000, "0", "out"), throws_error("Cannot traverse from node 0!"))
})

test_that("traversing from virtual flowlines continues from enclosing waterbody", {
  expect_warning(traverse_flowlines(100, "55503985", "out"), "Start ID provided is a virtual flowline inside a waterbody. Continuing from 167745898")
})