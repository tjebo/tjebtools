#' Show current package dependencies
#' @description Will \code{cat} out a cut/paste-able set of fields for a
#' \code{DESCRIPTION} file with minimum required versions for
#' each package based upon currently available package vesions
#' in CRAN.
#' @param pkg package description, can be path or package name
#' @param fields fields to get & report dependencies for
#' @note R and the R version is NOT added to \code{Depends}
#' @examples
#' \dontrun{
#' show_pkg_versions("qmethod")
#' show_pkg_versions("MASS")
#' # assumes you're in a pkg devel dir
#' show_pkg_versions()
#' }
#' @importFrom tibble as_tibble
#' @importFrom stringi stri_split_lines
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tools package_dependencies
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom devtools package_file
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso https://stackoverflow.com/a/38743324/7941188
#' @export
show_pkg_versions <- function(pkg = ".",
                             fields = c("Depends", "Imports", "LinkingTo", "Suggests")) {

  stopifnot(purrr::is_scalar_character(pkg), pkg != "")
  fields <- match.arg(fields, c("Depends", "Imports", "LinkingTo", "Suggests"),
    several.ok = TRUE
  )

  avail <- tibble::as_tibble(utils::available.packages())

  if (pkg == ".") {
    pkg_deps <- unclass(tibble::as_tibble(read.dcf(file.path(devtools::package_file(), "DESCRIPTION"))))
    pkg <- pkg_deps$Package
    purrr::map(fields, ~ stringi::stri_split_lines(pkg_deps[[.]])) %>%
      purrr::map(function(x) {
        if (length(x) > 0) {
          unlist(x) %>%
            stringi::stri_replace_all_regex(" \\(.*$|,", "") %>%
            purrr::discard(`%in%`, c("", "R"))
        } else {
          x
        }
      }) -> pkg_deps
    names(pkg_deps) <- fields
  } else {
    pkg_deps <- purrr::map(fields, ~ purrr::flatten_chr((tools::package_dependencies(pkg, which = .))))
    names(pkg_deps) <- fields
  }

  pkg_deps <- purrr::discard(pkg_deps, function(x) {
    length(x) == 0
  })

  purrr::map(pkg_deps, function(x) {
    non_base <- dplyr::filter(avail, Package %in% x)
    base <- setdiff(x, non_base$Package)

    non_base %>%
      dplyr::mutate(pv = sprintf("%s (>= %s)", Package, Version)) %>%
      dplyr::select("pv") %>%
      purrr::flatten_chr() -> pkg_plus_version

    sort(c(pkg_plus_version, base))
  }) -> pkg_deps

  cat("Package: ", pkg, "\n", sep = "")
  purrr::walk(names(pkg_deps), function(x) {
    cat(x, ":\n", sep = "")
    sprintf("    %s", pkg_deps[[x]]) %>%
      paste0(collapse = ",\n") %>%
      cat()
    cat("\n")
  })
}
 # show_pkg_versions()
