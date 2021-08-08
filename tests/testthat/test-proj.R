
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
  noop <- proj_create_crs_to_crs("OGC:CRS84", "OGC:CRS84", options="AUTHORITY=EPSG")
  expect_match(proj_info(noop)$definition, "noop")
})

test_that("proj_create_from_wkt() works", {
  wkt_good <- paste0(
    "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563],",
    "AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],",
    "UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AXIS[\"Longitude\",EAST],",
    "AXIS[\"Latitude\",NORTH]]"
  )

  pj <- expect_silent(proj_create_from_wkt(wkt_good))
  expect_s3_class(pj, "rlibproj_proj")
  expect_error(proj_create_from_wkt("invalid"), "reported error")

  # from https://github.com/OSGeo/PROJ/blob/master/test/unit/test_c_api.cpp#L316
  wkt_warn <- paste0(
    "PROJCS[\"test\",\n",
    "  GEOGCS[\"WGS 84\",\n",
    "    DATUM[\"WGS_1984\",\n",
    "        SPHEROID[\"WGS 84\",6378137,298.257223563]],\n",
    "    PRIMEM[\"Greenwich\",0],\n",
    "    UNIT[\"degree\",0.0174532925199433]],\n",
    "  PROJECTION[\"Transverse_Mercator\"],\n",
    "  PARAMETER[\"latitude_of_origin\",31],\n",
    "  UNIT[\"metre\",1]]"
  )

  expect_warning(
    expect_s3_class(proj_create_from_wkt(wkt_warn), "rlibproj_proj"),
    "reported warning"
  )

  # bad options
  expect_error(
    .Call(proj_c_create_from_wkt, proj_context(), "invalid", "NOT_AN_OPTION=NOPE"),
    "Unknown option"
  )
})

test_that("proj_get_(source|target)_crs() works", {
  pipe_good <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
  info_src <- proj_info(proj_get_source_crs(pipe_good))
  info_dst <- proj_info(proj_get_target_crs(pipe_good))
  expect_match(info_src$description, "WGS\\s*84")
  expect_match(info_dst$description, "Mercator")

  expect_error(proj_get_source_crs("OGC:CRS84"), "error")
  expect_error(proj_get_target_crs("OGC:CRS84"), "error")
})

test_that("proj_get_non_deprecated() works", {
  expect_identical(proj_get_non_deprecated("OGC:CRS84"), list())
  expect_length(proj_get_non_deprecated("EPSG:4226"), 2)
  expect_error(proj_get_non_deprecated("+proj=noop"), "error")
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
