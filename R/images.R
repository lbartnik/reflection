#' @importFrom imager is.cimg grayscale squeeze
#' @export
unwrap_image <- function (x) {
  stopifnot(is.cimg(x))
  y <- squeeze(grayscale(x))
  .Call("C_unwrap_image", as.array(y))
  invisible()
}
