context("remote operations")

test_that("load small csv", {
  # random csv file
  csv_url <- "https://cdn.rawgit.com/aronlindberg/Social-Network-Analysis-in-R/master/r_keyactorcentrality.csv"
  read_c <- function(url, ...) {
    catcher::cache_op("read.csv", url, ...)
  }
  dat <- read_c(csv_url)
  expect_equal(sum(dat[1]), 10)
  expect_equal(sum(dat[2]), 2)
  dat2 <- read_c(csv_url)
  expect_equal(dat, dat2)
})
