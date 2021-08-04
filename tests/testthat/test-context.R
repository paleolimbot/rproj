
test_that("proj_context() works", {
  expect_s3_class(proj_context(), "rlibproj_context")
  expect_match(format(proj_context()), "proj_context")
  expect_output(print(proj_context()), "proj_context")
})

test_that("with_proj_context() works", {
  current_net <- proj_context_is_network_enabled()
  with_proj_context(proj_context_create(network_enabled = !current_net), {
    expect_identical(proj_context_is_network_enabled(), !current_net)
  })
  expect_identical(proj_context_is_network_enabled(), current_net)
})

test_that("proj_context_clone() works", {
  expect_s3_class(proj_context_clone(), "rlibproj_context")
})

test_that("proj_context_create() works", {
  ctx_default <- proj_context()
  expect_false(proj_context_is_network_enabled(ctx_default))

  ctx <- proj_context_create(network_enabled = TRUE)
  expect_true(proj_context_is_network_enabled(ctx))

  expect_false(proj_context_is_network_enabled(ctx_default))
})
