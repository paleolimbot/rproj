
test_that("proj_version() works", {
  expect_s3_class(proj_version(runtime = TRUE), "package_version")
  expect_s3_class(proj_version(runtime = FALSE), "package_version")
})
