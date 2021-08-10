
test_that("the crs2crs engine works", {
  engine <- crs_engine_rlibproj()
  expect_identical(
    crs2crs::crs_transform(wk::xy(45, -64, crs = "EPSG:4326"), "OGC:CRS84", engine = engine),
    wk::xy(-64, 45, crs = "OGC:CRS84")
  )
})
