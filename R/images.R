#' @importFrom imager is.cimg grayscale squeeze
#' @export
unwrap_image <- function (x, dAlpha = 1, dR = 1) {
  stopifnot(is.cimg(x))
  a <- as.array(squeeze(grayscale(x)))
  unwrap_array(a, dAlpha, dR)
  invisible()
}


unwrap_array <- function (x, dAlpha = 1, dR = 1, rMax = NULL) {
  stopifnot(is.array(x), is.numeric(x))
  rMax <- ceiling(sqrt(sum((dim(x)/2)**2)))
  .Call("C_unwrap_array", x, as.numeric(dAlpha), as.numeric(rMax), as.numeric(dR))
}
