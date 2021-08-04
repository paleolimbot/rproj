
test_that("proj_create() works", {
  expect_s3_class(proj_create("+proj=noop"), "rlibproj_proj")
  expect_error(proj_create("+proj=not_a_proj"), "Unknown projection")
})

test_that("proj_info() works", {
  expect_identical(proj_info("+proj=noop")$id, "noop")
  expect_identical(proj_info("EPSG:4326")$id, NA_character_)
})

test_that("proj_get_name() works", {
  expect_identical(proj_get_type("+proj=noop"), "OTHER_COORDINATE_OPERATION")
  expect_identical(proj_get_type("EPSG:4326"), "GEOGRAPHIC_2D_CRS")
})

test_that("print and format methods work", {
  expect_match(format(proj_create("+proj=noop")), "<proj")
  p <- proj_create("+proj=noop")
  expect_output(expect_identical(print(p), p), "<proj")
})
