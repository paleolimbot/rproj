
test_that("wk trans works", {
  tr <- wk::as_wk_trans(proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857"))
  expect_equal(
    wk::wk_transform(wk::xy(-64, 45), tr),
    wk::xy(-7124447.41076951, 5621521.48619207)
  )

  inv <- wk::wk_trans_inverse(tr)
  expect_equal(
    wk::wk_transform(wk::xy(-7124447.41076951, 5621521.48619207), inv),
    wk::xy(-64, 45)
  )

  invinv <- wk::wk_trans_inverse(inv)
  expect_equal(
    wk::wk_transform(wk::xy(-64, 45), invinv),
    wk::xy(-7124447.41076951, 5621521.48619207)
  )
})

test_that("wk_trans works with NA values in xy()", {
  tr <- wk::as_wk_trans(proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857"))
  expect_identical(
    wk::wk_transform(wk::xy(NA, NA), tr),
    wk::xy(NA, NA)
  )
})

test_that("wk_trans handles error transforms", {
  tr <- wk::as_wk_trans(proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857"))
  expect_identical(
    wk::wk_transform(wk::xy(100:101, 100:101), tr),
    wk::xy(c(NA, NA), c(NA, NA))
  )
})
