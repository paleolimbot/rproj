
test_that("type lookup works", {
  types <- proj_type_name(0:28)
  expect_identical(proj_type_code(types), c(0:27, NA))
})

test_that("comparison criterion lookup works", {
  comp <- proj_comp_name(0:3)
  expect_identical(proj_comp_code(comp), c(0:2, NA))
})

test_that("wkt type lookup works", {
  wkt_type <- proj_wkt_type_name(0:6)
  expect_identical(proj_wkt_type_code(wkt_type), c(0:5, NA))
})

test_that("proj string type lookup works", {
  wkt_type <- proj_proj_string_type_name(0:2)
  expect_identical(proj_proj_string_type_code(wkt_type), c(0:1, NA))
})
