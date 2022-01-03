
test_that("proj_create() works", {
  expect_s3_class(proj_create("+proj=noop"), "rproj_proj")
  expect_error(proj_create("+proj=not_a_proj"), "Unknown projection")
})

test_that("proj_clone() works", {
  ctx <- proj_context_create()
  p1 <- proj_create("+proj=noop", ctx)
  expect_true(proj_xptr_addr(proj_clone(p1)) != proj_xptr_addr(p1))
  expect_identical(proj_get_context(p1), ctx)
})

test_that("proj_get_context() works", {
  ctx <- proj_context_create()
  expect_identical(proj_get_context(proj_create("+proj=noop", ctx)), ctx)
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
  expect_s3_class(pj, "rproj_proj")
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
    expect_s3_class(proj_create_from_wkt(wkt_warn), "rproj_proj"),
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

test_that("proj_normalize_for_visualization() works", {
  crs84 <- proj_create("OGC:CRS84")
  expect_identical(
    proj_info(proj_normalize_for_visualization(crs84))$description,
    proj_info(crs84)$description
  )

  # proj_normalize_for_visualization uses the debug log level
  # instead of the error one, so sink() these messages until
  # the problem is fixed
  temp <- tempfile()
  tempf <- file(temp, open = "w")
  sink(tempf, type = "message")
  expect_error(proj_normalize_for_visualization("+proj=noop"), "error")
  sink(type = "message")
  close(tempf)
  unlink(temp)
})

test_that("proj_is_equivalent_to() works", {
  expect_true(proj_is_equivalent_to("EPSG:4326", "EPSG:4326", "strict"))
  expect_false(proj_is_equivalent_to("EPSG:4326", "OGC:CRS84", "strict"))
  expect_true(
    proj_is_equivalent_to(
      "EPSG:4326", "OGC:CRS84",
      "equivalent_except_axis_order_geogcrs"
    )
  )

  expect_error(proj_is_equivalent_to("WGS84", "WGS84", "fish"), "Invalid value")
})

test_that("proj_identify() works", {
  id <- proj_identify("OGC:CRS84", "OGC")
  expect_equal(nrow(id), 1)
  expect_identical(id$confidence, 100L)
  expect_equal(nrow(proj_identify("OGC:CRS84", auth_name = character())), 0)
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

test_that("proj_as_wkt() works", {
  expect_match(proj_as_wkt("EPSG:4326"), "^GEODCRS")
  wkt_oneline <- proj_as_wkt("EPSG:4326", options = "MULTILINE=NO")
  expect_false(grepl("\n", wkt_oneline))
  expect_error(proj_as_wkt("EPSG:4326", options = "NOT_AN_OPTION"), "error")
})

test_that("proj_as_proj_string() works", {
  expect_match(proj_as_proj_string("EPSG:4326"), "^\\+proj=longlat")
  multiline <- proj_as_proj_string(
    "EPSG:4326",
    options = c("MULTILINE=YES", "MAX_LINE_LENGTH=10")
  )
  expect_true(grepl("\n", multiline))
  expect_error(proj_as_proj_string("EPSG:4326", options = "NOT_AN_OPTION"), "error")
})

test_that("proj_as_projjson() works", {
  json <- proj_as_projjson("EPSG:4326")
  parsed <- jsonlite::fromJSON(json)
  expect_identical(parsed$id, list(authority = "EPSG", code = 4326L))
  oneline <- proj_as_projjson("EPSG:4326", options = "MULTILINE=NO")
  expect_false(grepl("\n", oneline))
  expect_error(proj_as_projjson("EPSG:4326", options = "NOT_AN_OPTION"), "error")
})

test_that("short definition generator works", {
  crs_with_id <- proj_create("OGC:CRS84")
  expect_identical(proj_make_compact_definition(crs_with_id), "OGC:CRS84")

  crs_without_id <- proj_create("WGS84")
  expect_identical(proj_make_compact_definition(crs_without_id), "EPSG:4326")

  crs_nomatch <- proj_create("EPSG:4326+5717")
  expect_match(proj_make_compact_definition(crs_nomatch), "^COMPOUNDCRS")

  pipeline <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
  expect_match(proj_make_compact_definition(pipeline), "^\\+proj=pipeline")
})

test_that("print format and str methods work", {
  p <- proj_create("+proj=noop")
  expect_match(format(p), "\\+proj=noop")
  expect_output(expect_identical(print(p), p), "<rproj")
  expect_output(expect_identical(str(p), p), "<rproj")

  compound <- proj_create("EPSG:4326+5717")
  expect_output(print(compound), "\\$components")

  pipeline <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
  expect_output(print(pipeline), "get_source_crs")
  expect_output(print(pipeline), "get_target_crs")

  conversion <- proj_create("+proj=noop")
  expect_output(print(conversion), "\\$method")
})

test_that("list()-like interface works", {
  p <- proj_create("EPSG:4326")
  expect_identical(
    proj_as_projjson_parsed(p)$id,
    list(authority = "EPSG", code = 4326L)
  )
  expect_identical(
    p$id,
    list(authority = "EPSG", code = 4326L)
  )
  expect_identical(
    p[["id"]],
    list(authority = "EPSG", code = 4326L)
  )
  expect_identical(length(p), length(proj_as_projjson_parsed(p)))
})
