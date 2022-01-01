
test_that("assert functions work", {
  expect_identical(assert_chr1("fish"), "fish")
  expect_error(assert_chr1(1), "must be character")

  expect_identical(assert_dbl1(1), 1)
  expect_identical(assert_dbl1(1L), 1)
  expect_error(assert_dbl1("one"), "must be numeric")

  expect_identical(assert_int1(1L), 1L)
  expect_identical(assert_int1(1), 1L)
  expect_error(assert_int1(1.1), "must be an integer")

  expect_identical(assert_lgl1(TRUE), TRUE)
  expect_error(assert_lgl1(NA), "must be logical")
  expect_error(assert_lgl1("not true"), "must be logical")
})
