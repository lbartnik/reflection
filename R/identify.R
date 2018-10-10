#' @importFrom rlang UQ
#' @export
identify_object <- function (obj, repo) {
  id <- storage::compute_id(obj)
  q  <- as_artifacts(repo) %>% filter(id == UQ(id))

  ans <- q %>% summarise(n = n()) %>% first

  if (!ans) {
    warn("cannot match object in repository")
    return(NULL)
  }

  read_artifacts(q)
}

#' @importFrom png readPNG
#' @importFrom jsonlite base64_dec
#' @importFrom imager is.cimg
identify_image <- function (img, repo) {
  stopifnot(is.cimg(img))
  stopifnot(is_repository(repo))

  arts <- as_artifacts(repo) %>% filter('plot' %in% class) %>% read_artifacts

  new <- unwrap_image(img, 0.01, 1)

  known <- arts %>% lapply(function (a) {
    png <- readPNG(base64_dec(artifact_data(a)$png))
    img <- png_as_cimg(png)
    unwrap_image(img, 0.01, 1)
  })

  dists <- map_dbl(known, function (known) image_dist(known, new))
  i <- which.min(dists)

  nth(arts, i)
}

#' @importFrom imager cimg mirror imrotate
png_as_cimg <- function (raw) {
  if (length(dim(raw)) == 3) {
    dim(raw) <- c(dim(raw)[1:2], 1, dim(raw)[3])
  } else {
    dim(raw) <- c(dim(raw), 1, 1)
  }
  cimg(raw) %>% mirror("x") %>% imrotate(-90)
}




#' @export
identify_file <- function (path, repo) {
  stopifnot(file.exists(path))

  file <- new_file(path)
  identify_file_impl(file, repo)

  # TODO check file type
  #  1. if R data, try matching R objects
  #  2. if a plot, try matching a plot
  #  3. if csv, try matching data frames
  #  4. if none, probably fail
}


#' @importFrom tools file_ext
new_file <- function (path) {
  ext <- tolower(file_ext(path))

  method <- "identify_file_impl"
  exts <- substr(ls(pattern = method, envir = asNamespace("search")), nchar(method)+2, 0xBEEF)
  exts <- setdiff(exts, c("default", "")) # remove the main method and the default dispatch

  if (ext %nin% exts) {
    abort(glue("extension '{ext}' is not supported"))
  }

  structure(list(path = path, extension = ext), class = ext)
}


identify_file_impl <- function (x, repo, ...) UseMethod("identify_file_impl")

identify_file_impl.default <- function (x, repo, ...) {
  abort(glue("cannot call identify_file_impl for class {first(class(x))}"))
}

identify_file_impl.rds <- function (x, repo, ...) {
  ans <- tryCatch(readRDS(x$path), error = function (e)e)
  if (is_error(ans)) {
    abort(glue("readRDS failed; {x$path} is not an RDS file"))
  }
  identify_object(ans, repo)
}

identify_file_impl.rdata <- function (x, repo, ...) {
  ans <- tryCatch(local({ load(x$path); lapply(ls(), get) }), error = function (e)e)
  if (is_error(ans)) {
    abort(glue("load() failed; {x$path} is not a RData file"))
  }

  # TODO what to do with possible multiple matches?
  ans <- unlist(lapply(ans, identify_object, repo = repo), recursive = FALSE)
  structure(ans, class = 'container')
}

#' @importFrom imager load.image
identify_file_impl.png <- function (x, repo, ...) {
  identify_image(load.image(x$path), repo)
}
