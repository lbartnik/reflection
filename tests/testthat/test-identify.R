context("identify")

test_that("can identify file", {
  r <- sample_repository()
  x <- identify_object(iris, r)

  expect_length(x, 1)
})
