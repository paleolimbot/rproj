
test_that("as_proj_area works", {
  expect_identical(as_proj_area(as.numeric(1:4)), as.numeric(1:4))
  expect_identical(as_proj_area(wk::xy()), c(-180, -90, 180, 90))

  expect_equal(
    as_proj_area(wk::rct(-1, -10, 2, 12)),
    c(-1, -10, 2, 12)
  )

  expect_equal(
    as_proj_area(wk::rct(179, -10, -178, 12)),
    c(179, -10, -178, 12)
  )

  expect_equal(
    as_proj_area(wk::xy(seq(-91, 91), 4)),
    c(-180, 4, 180, 4)
  )
})
