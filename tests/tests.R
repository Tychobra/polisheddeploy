library(polisheddeploy)

deps_list <- jsonlite::read_json("deps.json")
hold_dep <- deps_list[[1]]
deps <- parse_deps(deps_list)

install_packages("deps.json", gh_pat = NULL)

polisheddeploy:::github_install(
  deps$github_deps[[1]]
)

polisheddeploy:::is_installed(hold_dep)


cp_dep <- list(
  "Package" = "covrpage",
  "GithubUsername" = "yonicd",
  "GithubRepo" = "covrpage",
  "GithubRef" = "HEAD",
  "GithubSHA1" = "b5e63902e3a33fbc5f927ef7a300477589536854"
)
polisheddeploy:::github_install(
  cp_dep, force = TRUE
)

list(
  "Package" = "shinyFeedback",
  "GithubUsername" = "merlinoa",
  "GithubRepo" = "shinyFeedback",
  "GithubRef" = "HEAD",
  "GithubSHA1" = "b741fedab6736b9047580734adcff9b97cc808c3"
)

polisheddeploy:::github_install(, Ncpus = ncpus, auth_token = NULL, force = TRUE)


github_install(dep_, Ncpus = ncpus, auth_token = gh_pat)