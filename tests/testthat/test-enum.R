
test_that("type lookup works", {
  types <- proj_type_name(0:27)
  expect_identical(proj_type_code(types), 0:27)
})

test_that("comparison criterion lookup works", {
  comp <- proj_comp_name(0:2)
  expect_identical(proj_comp_code(comp), 0:2)
})
