#' @importFrom imager is.cimg grayscale squeeze
#' @export
unwrap_image <- function (x, dAlpha = 1, dR = 1, missing = 1) {
  stopifnot(is.cimg(x))
  a <- as.array(squeeze(grayscale(x)))
  o <- unwrap_array(a, dAlpha, dR, missing = missing)
  # rotate 90 deg counter-clockwise
  o <- t(apply(o, 2, rev))
  as.cimg(o)
}

unwrap_array <- function (x, dAlpha = 1, dR = 1, rMax = NULL, missing = 0) {
  stopifnot(is.array(x), is.numeric(x))
  rMax <- ceiling(sqrt(sum((dim(x)/2)**2)))
  .Call("C_unwrap_array", x, as.numeric(dAlpha), as.numeric(rMax), as.numeric(dR), as.numeric(missing))
}


#' @export
compare_images <- function (a, b, cutoff = .5) {
  stopifnot(is.cimg(a), is.cimg(b))

  as_grayscale <- function (img) {
    if (identical(last(dim(img)), 1L)) return(img)
    squeeze(grayscale(img))
  }

  to_distances <- function (x) {
    x <- imgradient(x, "xy") %>% enorm %>% as_grayscale %>% as.array
    apply(x > quantile(as.numeric(x), cutoff), 1, function (c) which(c)/length(c))
  }

  diffs <- Map(cdf_diff, to_distances(a), to_distances(b))
  sum(unlist(diffs))
}


cdf_diff <- function (x, y) {
  .Call("C_cdf_diff", sort(as.numeric(x)), sort(as.numeric(y)))
}
