context("operational errors")

test_that("missing functions", {
  expect_error(cache_op("sdfdk", "123"))
})
