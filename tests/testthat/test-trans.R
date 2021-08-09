
test_that("trans works for matrix", {
  p <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3857")
  x <- proj_coord(-64, 45)
  expect_identical(proj_trans(x, p, "ident")[, 1:4, drop = FALSE], x)
  xout <- proj_trans(x, p, "fwd")
  xcpy <- proj_trans(xout, p, "inv")
  expect_equal(xcpy[, 1:4, drop = FALSE], x)

  # check logging with verbose output
  expect_silent(proj_trans(proj_coord(0, 100), p))

  temp <- tempfile()
  tempf <- file(temp, open = "w")
  sink(tempf, type = "message")

  proj_trans(proj_coord(0, 100), p, verbose = TRUE)

  sink(type = "message")
  close(tempf)

  expect_true(any(grepl("first error was", readLines(temp))))

  unlink(temp)
})
