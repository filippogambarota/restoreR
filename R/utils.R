# get_cran_packages -------------------------------------------------------

#' Return all CRAN packages installed from the user
#'
#' @return A tibble with name, version and source

get_cran_packages <- function(){
  pkgs <- devtools::package_info()
  cran_pkgs <- pkgs[grepl("CRAN", pkgs$source), ]
  return(cran_pkgs)
}

# get_github_packages -------------------------------------------------------

#' Return all Github packages installed from the user.
#'
#' @return A tibble with name, version and source

get_github_packages <- function(){
  pkgs <- devtools::package_info()
  github_pkgs <- pkgs[grepl("Github", pkgs$source), ]
  github_pkgs$source <- get_github_source(github_pkgs)
  return(github_pkgs)
}

# get_github_source ---------------------------------------------------------

#' Transform the source column from get_github_packages for using the string
#' directly on \code{devtools::install_github()}
#'
#' @return a character vector with all github sources

get_github_source <- function(github_packages){
  repos <- regmatches(x$source, regexpr("\\(.*?\\)", x$source))
  repos <- substr(repos, 2, nchar(repos) - 1)
  return(repos)
}

# save_installed_packages ----------------------------------------------------

#' Save an \code{.rds} file with all packages in the specified path
#' @param file path to create the \code{rds} file
#' @return None
#' @export

save_installed_packages <- function(file = "pkgs_list.rds"){
  all_pksg <- list(
    cran = get_cran_packages(),
    github = get_github_packages()
  )

  saveRDS(all_pksg, file = file)
  cli::cli_alert_success("Packages list saved!")

}

# restore_pkgs ----------------------------------------------------

#' Install all packages contained in the \code{rds} file created with
#' \code{save_installed_packages()}
#' @param file the \code{rds} file with all packages
#' @return None
#' @export

restore_pkgs <- function(file){
  all_pksg <- readRDS(file)
  out <- sapply(all_pksg$cran$package, install.packages)
  cli::cli_alert_success("Packages from CRAN installed!")
  out <- sapply(all_pksg$github$source, devtools::install_github)
  cli::cli_alert_success("Packages from Github installed!")
}
