
test_that("proj_version() works", {
  expect_identical(proj_version(runtime = TRUE), package_version("8.1.0"))
  expect_identical(proj_version(runtime = FALSE), package_version("8.1.0"))
})
