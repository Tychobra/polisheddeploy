is_installed <- function(dep) {
  pkgs <- as.data.frame(installed.packages())

  pkg <- pkgs[pkgs$Package == dep$Package & pkgs$Version == dep$Version, ]

  if (nrow(pkg) == 1L) {
    out <- TRUE
  } else {
    out <- FALSE
  }

  out
}

#' @noRd
cran_install <- function(dep, ...) {


  if (!is_installed(dep)) {

    print(paste0("Installing ", dep$Package))

    remotes::install_version(
      dep$Package,
      version = dep$Version,
      upgrade = 'never',
      ...
    )

  }
}

#' @noRd
#'
#' @examples
#' test_dep <- list(
#'  Package = "covrpage",
#'  GithubUsername = "yonicd",
#'  GithubRepo = "covrpage",
#'  GithubRef = "HEAD",
#'  GithubSHA1 = "b5e63902e3a33fbc5f927ef7a300477589536854"
#' )
#'
#' github_install(test_dep)
#' github_install(test_dep, auth_token="gh_mypat")
github_install <- function(dep, ...) {

  remotes::install_github(
    paste0(dep$GithubUsername, "/", dep$Package),
    upgrade = "never",
    ref = dep$GithubSHA1,
    ...
  )
}

#' install_packages
#'
#' @param deps_path file path to the deps.json file.
#' @param ... additional arguments to pass to the install function.
#'
#' @export
#'
#' @importFrom jsonlite read_json
#'
#'
install_packages <- function(deps_path, ...) {
  # install the "remotes" R package from CRAN

  deps <- jsonlite::read_json(deps_path)

  parsed <- parse_deps(deps)

  cran_failures <- list()
  for (dep_ in parsed$cran_deps) {

    tryCatch({

      cran_install(dep_, ...)

    }, error = function(err) {

      print(err)
      # keep track of failed package installs.  We will attempt to install them
      # from different CRAN mirror after attempting to install all other packafes
      cran_failures <<- append(cran_failures, list(dep_))

    })
  }

  for (dep_ in cran_failures) {
    cran_install(dep_, repos = 'https://cran.rstudio.com/', ...)
  }

  for (dep_ in parsed$github_deps) {
    github_install(dep_, ...)
  }

  invisible(NULL)
}
