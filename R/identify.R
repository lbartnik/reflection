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


#' @export
identify_file <- function (path, repo) {
  stopifnot(file.exists(path))

  # TODO check file type
  #  1. if R data, try matching R objects
  #  2. if a plot, try matching a plot
  #  3. if csv, try matching data frames
  #  4. if none, probably fail
}
