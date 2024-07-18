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

  # Archive old tarballs and delete old binaries
  current_packages <- matrix(utils::available.packages(paste0("file:", tarball_location))[, c("Package", "Version")], ncol = 2)

  # tarballs
  all_src_files <- list.files(tarball_location, pattern = ".tar.gz")
  for (i in 1:nrow(current_packages)) {
    current_pkg <- current_packages[i,]
    pkg_name <- current_pkg["Package"]
    pkg_version <- current_pkg["Version"]
    pkg_src_files <- grep(pkg_name, all_src_files, value = TRUE)
    old_src_files <- grep(pkg_version, pkg_src_files, value = TRUE, invert = TRUE)

    pkg_archive <- file.path(tarball_location, "Archive", pkg_name)
    if(!dir.exists(pkg_archive)) {
      dir.create(pkg_archive, recursive = TRUE)
    }

    if(length(old_src_files) > 0) {
      move_success <- file.rename(from = file.path(tarball_location, old_src_files), to = file.path(pkg_archive, old_src_files))
      if(!isTRUE(all(move_success))) warning("Archive failed")
    }
  }

  # binaries
  all_bin_files <- list.files(windows_bin_location, pattern = ".zip")
  for (i in 1:nrow(current_packages)) {
    current_pkg <- current_packages[i,]
    pkg_name <- current_pkg["Package"]
    pkg_version <- current_pkg["Version"]
    pkg_bin_files <- grep(pkg_name, all_bin_files, value = TRUE)
    old_bin_files <- grep(pkg_version, pkg_bin_files, value = TRUE, invert = TRUE)

    if(length(old_bin_files) > 0) {
      delete_success <- file.remove(file.path(windows_bin_location, old_bin_files))
      if(!isTRUE(all(delete_success))) warning("Binary deletion failed")
    }
  }
}
