#' Create or update a local repository
#'
#' Generate or update a directory structure that can be read by `utils::install.packages`.
#'
#' Create or update a system of subdirectories that is recognized by `utils::install.packages` as a
#' package repository. The function publishes the referenced package both as a source tarball and as
#' a binary package under the current operating system. As a result, installation of the package
#' will fail if attempted from a different operating system with no available toolchain.
#'
#' @param repository_path Folder in which to create the repository or update an existing one.
#' @param package_path Path to the parent directory of the package to be published.
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
  file.path(repository_path, "src", "contrib")
}
