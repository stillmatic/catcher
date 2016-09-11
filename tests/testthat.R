if(!require(testthat)) {
  install.packages('testthat')
}
library(testthat)
library(catcher)

test_check("catcher")
