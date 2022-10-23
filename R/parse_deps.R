#' parse_deps
#'
#' Parse the deps
#'
#' @param deps
#'
#' @export
#'
parse_deps <- function(deps) {
  cran_deps <- list()
  github_deps <- list()
  cran_names <- character(0)
  github_names <- character(0)
  for (dep in deps) {

    if (identical(dep$Repository, "CRAN")) {
      cran_deps <- append(cran_deps, list(dep))
      cran_names <- append(cran_names, dep$Package)
    } else if (!is.null(dep$GithubRepo)) {
      github_deps <- append(github_deps, list(dep))
      github_names <- append(github_names, dep$Package)
    } else {
      stop(paste0(dep$Package, " is not a CRAN or GitHub package"), call. = FALSE)
    }

  }

  list(
    cran_deps = cran_deps,
    github_deps = github_deps,
    cran_names = cran_names,
    github_names = github_names
  )
}