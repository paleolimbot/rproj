
test_that("type lookup works", {
  types <- proj_type_name(0:27)
  expect_identical(proj_type_code(types), 0:27)
})
