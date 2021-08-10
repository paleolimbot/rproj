
test_that("st_crs() transform works works", {
  ll <- sf::st_crs("EPSG:4326")
  ll2 <- sf::st_crs(proj_create("EPSG:4326"))
  expect_true(ll == ll2)
})
