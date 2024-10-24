#' Produce the pdf manual for a package
#'
#' @param package_location root directory for the package being documented
#' @param output_file file path for output
#'
#' @return The return value is an error code (0 for success), given the
#'   invisible attribute (so needs to be printed explicitly). If the command
#'   could not be run for any reason, the value is 127 and a warning is issued
#'   (as from R 3.5.0). Otherwise the value is the exit status returned by the
#'   command.
#' @export

make_manual <- function(package_location = getwd(), output_file = NULL) {
  if(is.null(output_file)) {
    file_sep <- .Platform$file.sep

    package_path_components <- strsplit(package_location, file_sep)[[1]]
    package_name <- utils::tail(package_path_components, n = 1)
    file_name <- paste0(package_name, "-manual.pdf")

    output_path_components <- c(utils::head(package_path_components, n = -1), file_name)
    output_file <- paste(output_path_components, collapse = file_sep)
  }

  command <- paste0(
    'R CMD Rd2pdf --output="',
    output_file,
    '" --quiet --force "',
    package_location,
    '"'
  )

  system(command)
}
