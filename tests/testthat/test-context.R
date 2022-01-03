
test_that("proj_context() works", {
  expect_s3_class(proj_context(), "rproj_context")
  expect_match(format(proj_context()), "proj_context")
  expect_output(print(proj_context()), "proj_context")
})

test_that("proj_context_set_log_level() works", {
  new_ctx <- proj_context_create(log_level = 2L)
  expect_identical(proj_context_set_log_level(3, new_ctx), 2L)
  expect_identical(attr(new_ctx, "config")$log_level, 3L)
  expect_identical(proj_context_set_log_level(2, new_ctx), 3L)
  expect_identical(attr(new_ctx, "config")$log_level, 2L)
})

test_that("with_proj_context() works", {
  current_net <- proj_context_is_network_enabled()
  with_proj_context(proj_context_create(network_enabled = !current_net), {
    expect_identical(proj_context_is_network_enabled(), !current_net)
  })
  expect_identical(proj_context_is_network_enabled(), current_net)
})

test_that("proj_context_clone() works", {
  expect_s3_class(proj_context_clone(), "rproj_context")
})

test_that("proj_context_create() works", {
  ctx_default <- proj_context()
  expect_false(proj_context_is_network_enabled(ctx_default))

  ctx <- proj_context_create(network_enabled = TRUE)
  expect_true(proj_context_is_network_enabled(ctx))

  expect_false(proj_context_is_network_enabled(ctx_default))
})

test_that("proj_context() getters work", {
  expect_vector(proj_context_get_database_path(), character(), 1)
  expect_vector(proj_context_get_use_proj4_init_rules(), logical(), 1)
  expect_vector(proj_context_get_search_paths(), character())
  expect_vector(proj_context_get_url_endpoint(), character(), 1)
  expect_vector(proj_context_get_user_writable_directory(), character(), 1)
})
