test_that("load nonced val", {
  nonce <- paste0(sample(c(
    letters[1:26], LETTERS[1:26]
  ), 10, replace = T), collapse = "")

  library(digest)

  # see if cached md5 value is the same as value calculated on the fly
  digest_c <- function(x, algo = "md5", ...) {
    cache_op("digest", x, algo = algo, ...)
  }

  cached_val <- digest_c(nonce)
  rm(cached_val)
  cached_val <- digest_c(nonce)
  actual_val <- digest::digest(nonce, algo = "md5")
  expect_equal(cached_val, actual_val)
})

