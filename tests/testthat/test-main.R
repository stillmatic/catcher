test_that("hashing works") {
  query <- "read_csvhttps://google.com"
  hashed <- hash_query(query)
  digested <- digest::sha1(query)
  expect_equal(hashed, digested)
}
