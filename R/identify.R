#' @importFrom rlang UQ
#' @export
identify_object <- function (obj, repo) {
  id <- storage::compute_id(obj)
  q  <- as_artifacts(repo) %>% filter(id == UQ(id))

  ans <- q %>% summary(n = n()) %>% first

  if (!ans) {
    warn("cannot match object in repository")
    return(NULL)
  }

  read_artifacts(q)
}


#' @export
identify_file <- function (path, repo) {
  stopifnot(file.exists(path))


}
