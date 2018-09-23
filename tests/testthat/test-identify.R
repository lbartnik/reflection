context("identify")

test_that("can identify file", {
  r <- sample_repository()
  identify_object(iris, r)
})
