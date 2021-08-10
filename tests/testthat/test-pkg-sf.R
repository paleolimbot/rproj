
test_that("st_crs() transform works works", {
  skip_if_not_installed("sf")

  ll <- sf::st_crs("EPSG:4326")
  ll2 <- sf::st_crs(proj_create("EPSG:4326"))
  expect_true(ll == ll2)

  ll_pj <- as_proj(sf::st_crs("EPSG:4326"))
  expect_true(proj_is_equivalent_to(ll_pj, proj_create("EPSG:4326")))

  ll_pj <- as_proj(sf::st_crs("OGC:CRS84"))
  expect_true(proj_is_equivalent_to(ll_pj, proj_create("OGC:CRS84")))
})
