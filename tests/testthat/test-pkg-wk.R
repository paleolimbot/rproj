
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
