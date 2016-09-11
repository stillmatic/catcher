if(!require(testthat)) {
  install.packages('testthat')
}
library(testthat)
library(cacheR)

test_check("cacheR")
