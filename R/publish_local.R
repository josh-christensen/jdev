#' Create or update a local repository
#'
#' Generate or update a directory structure that can be read by
#' `utils::install.packages`.
#'
#' The function currently only behaves as intended on the Windows operating
#' system. Create or update a directory structure that is recognized by
#' `utils::install.packages` as a package repository. The function publishes the
#' referenced package both as a source tarball and as a binary package for the
#' Windows operating system. As a result, installation of the package will fail
#' if attempted from a different operating system with no available toolchain
#' unless a separate binary is created and added.
#'
#' @param repository_path Folder in which to create the repository or update an
#'   existing one.
#' @param package_path Path to the parent directory of the package to be
#'   published.
#'
#' @returns NULL
#'
#' @export
#'
#' @examples
#' \dontrun{publish_local("C:/Path/To/Repo")}

publish_local <- function(
    repository_path,
    package_path = "."
) {
  # Extract the current version of R
  r_version <- paste0(R.version$major, ".", substr(R.version$minor, 1, 1))
  # Determine where the tarball should be located
  tarball_location <- file.path(repository_path, "src", "contrib")
  # Determine where the windows binary should be located
  windows_bin_location <- file.path(repository_path, "bin", "windows", "contrib", r_version)

  # Check if the repo already exists
  repo_dirs <- c(tarball_location, windows_bin_location)
  n_repo_dirs <- length(repo_dirs)
  repo_exists <- dir.exists(repo_dirs)
  # Create the repo structure if it doesn't exist yet
  for(i in 1:n_repo_dirs) {
    if(!repo_exists[i]) {
      dir.create(repo_dirs[i], recursive = TRUE)
    }
  }

  # Build the tarball
  pkgbuild::build(path = package_path, dest_path = tarball_location)
  # Build the binary
  pkgbuild::build(path = package_path, dest_path = windows_bin_location, binary = TRUE)
  # Add or update package directory metadata files
  tools::write_PACKAGES(dir = tarball_location, type = "source")
  tools::write_PACKAGES(dir = windows_bin_location, type = "win.binary")
}
