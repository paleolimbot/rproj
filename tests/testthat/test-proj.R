
test_that("proj_create() works", {
  expect_s3_class(proj_create("+proj=noop"), "rlibproj_proj")
  expect_error(proj_create("+proj=not_a_proj"), "Unknown projection")
})

test_that("proj_create_crs_to_crs() works", {
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84")
  expect_match(proj_info(noop)$definition, "noop")

  # check with area
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", area = wk::xy(0, 0))
  expect_match(proj_info(noop)$definition, "noop")

  # check with options
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", auth_name = "EPSG")
  expect_match(proj_info(noop)$definition, "noop")
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", allow_ballpark = TRUE)
  expect_match(proj_info(noop)$definition, "noop")
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", allow_ballpark = FALSE)
  expect_match(proj_info(noop)$definition, "noop")
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", accuracy = 1)
  expect_match(proj_info(noop)$definition, "noop")
})

test_that("proj_info() works", {
  expect_identical(proj_info("+proj=noop")$id, "noop")
  expect_identical(proj_info("EPSG:4326")$id, NA_character_)
})

test_that("proj_get_name() works", {
  expect_identical(proj_get_type("+proj=noop"), "OTHER_COORDINATE_OPERATION")
  expect_identical(proj_get_type("EPSG:4326"), "GEOGRAPHIC_2D_CRS")
})

test_that("proj_get_remarks() works", {
  expect_identical(proj_get_remarks("EPSG:4326"), "")
})

test_that("proj_get_scope() works", {
  expect_true(proj_get_scope("EPSG:4326") != "")
})

test_that("proj_is_crs() works", {
  expect_true(proj_is_crs("EPSG:4326"))
  expect_false(proj_is_crs("+proj=noop"))
})

test_that("proj_is_deprecated() works", {
  expect_false(proj_is_deprecated("+proj=noop"))
})

test_that("proj_get_area_of_use() works", {
  aou <- proj_get_area_of_use("EPSG:4326")
  expect_true(!is.na(aou$name))
  expect_identical(aou$area, wk::rct(-180, -90, 180, 90, crs = "OGC:CRS84"))

  expect_error(proj_get_area_of_use("+proj=noop"))
})

test_that("print and format methods work", {
  expect_match(format(proj_create("+proj=noop")), "<proj")
  p <- proj_create("+proj=noop")
  expect_output(expect_identical(print(p), p), "<proj")
})
