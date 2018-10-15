#' @importFrom png readPNG
#' @importFrom rlang is_character
#' @export
distmatrix <- function (...) {
  images <- lapply(list(...), function (item) {
    if (is_repository(item)) {
      return(dump_plots(item))
    }
    if (is_character(item) && all(file.exists(item))) {
      return(lapply(item, function (file) prepare_png(readPNG(file), basename(file))))
    }
    abort("do not know how to handle {item}")
  })
  images <- unlist(images, recursive = FALSE)

  compute_matrix(images)
}

compute_matrix <- function (images, dist = image_dist) {
  unwrapped <- lapply(images, function (img) unwrap_image(img$image, 0.01, 1))
  dists <- combn(unwrapped, 2, function(pair) dist(first(pair), second(pair), .95))

  distm <- matrix("", length(unwrapped), length(unwrapped))

  hide <- apply(rbind(combn(seq(length(unwrapped)), 2), dists), 2, function (x) {
    distm[x[1], x[2]] <<- round(x[3], 2)
  })

  diag(distm) <- "0.0"

  distm <- as.data.frame(distm)
  rownames(distm) <- names(distm) <- basename(map_chr(images, `[[`, 'path'))

  distm
}

#' @importFrom imager save.image mirror imrotate cimg resize
#' @importFrom tools file_path_sans_ext
prepare_png <- function (raw_png, file_name) {
  if (length(dim(raw_png)) == 3) {
    dim(raw_png) <- c(dim(raw_png)[1:2], 1, dim(raw_png)[3])
  } else {
    dim(raw_png) <- c(dim(raw_png), 1, 1)
  }
  img <- cimg(raw_png) %>% mirror("x") %>% imrotate(-90)

  tmp_path <- file.path(tempdir(), paste0(file_path_sans_ext(file_name), '.png'))
  save.image(resize(img, 50, 50), tmp_path)

  list(path = tmp_path, image = img)
}

#' @importFrom jsonlite base64_dec
#' @importFrom png readPNG
dump_plots <- function (repo) {
  arts <- as_artifacts(repo) %>% filter('plot' %in% class) %>% read_artifacts
  ans  <- arts %>% lapply(function (a) {
    prepare_png(readPNG(base64_dec(artifact_data(a)$png)), shorten(a$id))
  })
  names(ans) <- shorten(map_chr(arts, `[[`, 'id'))
  ans
}
