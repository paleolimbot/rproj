
test_that("as_proj_area works", {
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
