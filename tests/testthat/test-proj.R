
test_that("proj_create() works", {
  expect_s3_class(proj_create("+proj=noop"), "rlibproj_proj")
  expect_error(proj_create("+proj=not_a_proj"), "Unknown projection")
})
