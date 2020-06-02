.pkg_check <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Please install package ", package, " to use this function.")
  }
}
