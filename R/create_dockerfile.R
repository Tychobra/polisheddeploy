sysreqs_in_base <- c(
  "gdebi-core",
  "git-core",
  "libcurl4-gnutls-dev",
  "wget"
)










#' @noRd
#'
#' @examples
#'
#' tlmgr_install_string(c("fancyhdr", "tabu"))
#'
#' tlmgr_install_string("fancyhdr")
tlmgr_install_string <- function(tlmgr_packages) {
  tlmgr_string <- paste0("c('", paste(tlmgr_packages, collapse = "', '"), "')")

  paste0("RUN R -e \"tinytex::tlmgr_install(", tlmgr_string, ")\"  \n")
}





#' create the dockerfile to run the Shiny app without using shiny-server
#'
#' This function creates the Dockerfile to run a shiny app without shiny-server.
#'
#' @param deps_yaml the "deps.yaml" file created by \code{automagic::make_deps_file()}
#' @param path the path to write the Dockerfile to.
#' @param app_dir Directory containing application. If \code{NA}, defaults to the directory
#' containing the "deps.yaml" file.
#' @param r_ver the version of R to use.  Defaults to "latest".
#' @param tlmgr character vector of TeX Live packages that should be included in the
#' installation of dependencies.
#' @param ncpus the number of parallel processes to use for a parallel install of
#' more than one source package. (See `Ncpus` argument from \code{utils::install.packages})
#' @param golem_package_name the name of the package as a character string.
#' @param minimal Defaults to \code{FALSE}. If \code{TRUE}, strips down the Dockerfile,
#' removing extra code such as \code{tryCatch}. (*NOTE:* For internal use)
#'
#' @return nothing
#'
#' @export
#'
#' @importFrom jsonlite read_json
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#' # run the following from the main folder of a Shiny app
#'
#'
#'   polishedapi::create_dockerfile("deps.yaml", tlmgr = "fancyhdr")
#'
#'   polishedapi::create_dockerfile("deps.yaml", tlmgr = c(
#' "fancyhdr",
#' "multicol",
#' "sectsty",
#' "xcolor",
#' "hyperref",
#' "booktabs",
#' "longtable",
#' "array",
#' "multirow",
#' "wrapfig",
#' "float",
#' "colortbl",
#' "pdflscape",
#' "tabu",
#' "threeparttable",
#' "threeparttablex",
#' "normalem",
#' "makecell",
#' "comment",
#' "amsmath",
#' "iftex",
#' "kvoptions",
#' "etoolbox",
#' "pdftexcmds",
#' "infwarerr",
#' "geometry",
#' "fancyvrb",
#' "framed",
#' "trimspaces",
#' "ulem",
#' "epstopdf-pkg"
#' ))
#' }
#'
#'
create_dockerfile <- function(
  deps_path,
  path = ".",
  app_dir = NA,
  r_ver = "latest",
  tlmgr = character(0),
  ncpus = 4,
  minimal = FALSE,
  golem_package_name = NA,
  gh_pat = NA,
  ...
) {

  if (is.character(deps_path) && identical(length(deps_path), 1L)) {

    if (is.na(app_dir)) {
      # assume app dir is the app containing the deps.yaml file
      app_dir <- dirname(deps_path)
      app_dir <- basename(deps_path)
    }

    # deps is the file path to the deps.yaml, not the list
    deps <- yaml::read_yaml(
      file = deps_path
    )

  } else {

    stop("invalid `deps_path`", call. = FALSE)
  }


  deps <- parse_deps(deps)


  package_names <- c(deps$cran_names, deps$github_names)

  #all_deps_found <- all(package_names %in% names(c(cran_deps, github_deps)))

  #if (!isTRUE(all_deps_found)) {
  #  missing_packages <- package_names[!(package_names %in% names(c(cran_deps)))]
  #  missing_packages_string <- paste(missing_packages, collapse = ", ")
  #  stop(paste0("could not find repository for ", missing_packages_string), call. = FALSE)
  #}
  file_path <- file.path(path, "Dockerfile")

  file.create(file_path)


  if (identical(length(tlmgr), 0L)) {
    base_image <- "r-ver"
  } else {
    base_image <- "verse"
  }

  write(paste0(
    "FROM rocker/", base_image, ":", r_ver, "\n\n",

    "EXPOSE 8080 \n"),
    file = file_path
  )

  # identify system dependencies for packages.  If user has if providing their gh PAT
  # then they have private repos, so only check the CRAN sys deps.
  # TODO: In future could distinguish private repos from public GH repos
  if (is.na(gh_pat)) {
    packages_to_sysreq <- package_names
  } else {
    packages_to_sysreq <- deps$cran_names
  }

  # R package dependencies of app package dependencies
  #dod <- unlist(remotes::package_deps(packages_to_sysreq)$package)

  # all R package dependencies
  #all_package_deps <- unique(c(deps$package_names, dod))

  hold_system_deps <- get_all_sysdeps(package_names)

  hold_system_deps <- fix_sysdeps(
    packages = package_names,
    sysdeps = hold_system_deps
  )

  hold_system_deps <- setdiff(hold_system_deps, sysreqs_in_base)

  # if there are any system dependencies
  if (length(hold_system_deps) > 0) {

    # write the Docker commands to install them
    system_deps_string <- paste(paste0("  ", hold_system_deps), collapse = " \\ \n")

    write(
      paste0("RUN apt-get update && apt-get install -y \\ \n", system_deps_string, "\n\n"),
      file = file_path,
      append = TRUE
    )

  }


  if (is.na(golem_package_name)) {
    copy_cmd <- paste0("
# Copy app into shiny server `shiny_app` directory
COPY '", app_dir, "' /srv/shiny_app
")

    final_cmd <- "CMD [\"Rscript\",\"-e\",\"shiny::runApp(appDir='/srv/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')\"]"
  } else {

    copy_cmd <- paste0("
# Install Golem app locally
RUN mkdir /build_zone
ADD 'shiny_app/' /build_zone
RUN R -e 'remotes::install_local(\"/build_zone\", upgrade=\"never\")'
RUN rm -rf /build_zone
")

    final_cmd <- paste0("
CMD [\"R\",\"-e\",\"options(shiny.port=8080,launch.browser=FALSE,shiny.host='0.0.0.0');", golem_package_name, "::run_app()\"]")

  }

  write(
    copy_cmd,
    file = file_path,
    append = TRUE
  )

  # install package deps
  write(
    paste0("RUN R -e 'install_package_deps(\"/srv/shiny_app/deps.json\")' \n\n"),
    file = file_path,
    append = TRUE
  )

  # if ((length(tlmgr) > 0)) {
  #
  #   # install specified LaTeX package deps
  #   tlmgr_cmd <- tlmgr_install_string(tlmgr)
  #
  #   write(
  #     tlmgr_cmd,
  #     file = file_path,
  #     append = TRUE
  #   )
  #
  # }



  write(
    final_cmd,
    file = file_path,
    append = TRUE
  )
}