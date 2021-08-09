
test_that("proj_list class works", {
  pl <- new_proj_list(list(proj_create("OGC:CRS84")))
  expect_s3_class(pl, "rlibproj_proj_list")
  expect_s3_class(pl[1], "rlibproj_proj_list")
  expect_identical(pl[[1]], unclass(pl)[[1]])

  pl[[1]] <- "EPSG:4326"
  expect_s3_class(pl[[1]], "rlibproj_proj")
  pl[1] <- "EPSG:3995"
  expect_s3_class(pl[[1]], "rlibproj_proj")

  expect_s3_class(data.frame(a = pl), "data.frame")
  expect_error(as.data.frame(pl), "data.frame")

  expect_vector(format(pl), character(), 1)
  expect_output(expect_identical(print(pl), pl), "proj_list")
})
