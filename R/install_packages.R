is_installed <- function(dep) {
  pkgs <- as.data.frame(installed.packages())

  pkg <- pkgs[pkgs$Package == dep$Package && version == dep$Version, ]

  if (nrow(pkg) == 1L) {
    out <- TRUE
  } else {
    out <- FALSE
  }

  out
}

#' @noRd
cran_install <- function(dep, minimal = FALSE, ...) {


  if (!is_installed(dep)) {

    print(paste0("Installing ", dep$Package))

    if (isTRUE(minimal)) {

      remotes::install_version(
        dep$Package,
        version = dep$Version,
        upgrade = 'never',
        ...
      )

    } else {
      tryCatch({
        remotes::install_version(
          dep$Package,
          version = dep$Version,
          upgrade = 'never',
          ...
        )
      }, error = function(err) {

        print(err)

        remotes::install_version(
          dep$Package,
          version = dep$Version,
          upgrade = 'never',
          repos = 'https://cran.rstudio.com/',
          ...
        )

      })
    }
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
#' github_install_string(test_dep)
#' github_install_string(test_dep, ncpus = 2)
#' github_install_string(test_dep, ncpus = 2, gh_pat="gh_mypat")
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
#' @param minimal T/F
#' @param gh_pat GitHub PAT
#' @param ... additional arguments to pass to the install function.
#'
#' @export
#'
#' @importFrom jsonlite read_json
#'
#'
install_packages <- function(deps_path, minimal = FALSE, gh_pat = NA, ...) {
  # install the "remotes" R package from CRAN

  deps <- jsonlite::read_json(deps_path)

  parsed <- parse_deps(deps)

  for (dep_ in parsed$cran_deps) {
    cran_install(dep_, Ncpus = ncpus, minimal = minimal)
  }

  for (dep_ in parsed$github_deps) {
    github_install(dep_, Ncpus = ncpus, auth_token = gh_pat)
  }

  invisible(NULL)
}