context("test models")

set.seed(1)
x <- rnorm(n = 50, mean = 2, sd = 1)
y <- rnorm(n = 50, mean = 0, sd = 1) + 3

test_that("lm works", {
  lm_c <- function(formula, ...) {
    cache_op("lm", formula, ...)
  }
  lm_c(y ~ x, overwrite = T)
  key <- catcher:::hash_query(paste0("lm", y ~ x, collapse = ""))
  expect_true(catcher:::exists_in_cache(key, 100))
})
