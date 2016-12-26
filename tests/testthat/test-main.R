context("local operations")

test_that("load nonced val", {
  set.seed(1)
  nonce <- paste0(sample(c(
    letters[1:26], LETTERS[1:26]
  ), 10, replace = T), collapse = "")

  library(digest)

  # see if cached md5 value is the same as value calculated on the fly
  digest_c <- function(x, algo = "md5", ...) {
    catcher::cache_op("digest", x, algo = algo, ...)
  }
  cached_val <- digest_c(nonce, overwrite = T)

  # check that it was saved locally
  key <- hash_query(paste0("digest", nonce, "md5"))
  expect_true(catcher:::exists_in_cache(key, 100))

  # load again
  rm(cached_val)
  cached_val <- digest_c(nonce)
  actual_val <- digest::digest(nonce, algo = "md5")
  expect_equal(cached_val, actual_val)
})

test_that("invert a matrix", {
  test_matrix <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3)
  solve_c <- function(x, ...) {
    catcher::cache_op("solve", x, ...)
  }
  inv_matrix <- solve_c(test_matrix, overwrite = T)
  key <- hash_query(paste0("solve", test_matrix))
  expect_true(catcher:::exists_in_cache(key, 100))
  expect_equal(inv_matrix, solve(test_matrix))
  inv_matrix2 <- solve_c(test_matrix)
  expect_equal(inv_matrix2, solve(test_matrix))
})
