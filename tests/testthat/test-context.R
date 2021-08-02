
test_that("proj_context() works", {
  expect_s3_class(proj_context(), "rlibproj_context")
  expect_match(format(proj_context()), "proj_context")
  expect_output(print(proj_context()), "proj_context")
})
