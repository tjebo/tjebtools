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
#' @importFrom rlang .data
#' @seealso https://stackoverflow.com/a/38743324/7941188
#' @export
show_pkg_versions <-
  function(pkg = ".", fields = NULL) {
    if (is.null(fields)) fields <- c("Depends", "Imports", "LinkingTo", "Suggests")
    stopifnot(purrr::is_scalar_character(pkg), pkg != "")
    fields <- match.arg(fields, c("Depends", "Imports", "LinkingTo", "Suggests"),
      several.ok = TRUE
    )
    avail <- tibble::as_tibble(utils::available.packages())

    if (pkg == ".") {
      pkg_deps <-
        unclass(tibble::as_tibble(read.dcf(file.path(
          devtools::package_file(),
          "DESCRIPTION"
        ))))
      pkg <- pkg_deps$Package
      mapped_fields <-
        purrr::map(fields, function(x) stringi::stri_split_lines(pkg_deps[[x]]))
      pkg_deps <- purrr::map(mapped_fields, function(x) {
        if (length(x) > 0) {
          x_vec <- unlist(x)
          x_regex_replaced <- stringi::stri_replace_all_regex(x_vec, " \\(.*$|,", "")
          purrr::discard(x_regex_replaced, `%in%`, c("", "R"))
        } else {
          x
        }
      })
      names(pkg_deps) <- fields
    } else {
      pkg_deps <-
        purrr::map(fields, function(x) {
          purrr::flatten_chr(tools::package_dependencies(pkg, which = x))
        })
      names(pkg_deps) <- fields
    }

    pkg_deps <- purrr::discard(pkg_deps, function(x) {
      length(x) == 0
    })

    purrr::map(pkg_deps, function(x) {
      non_base <- dplyr::filter(avail, .data$Package %in% x)
      base <- setdiff(x, non_base$Package)

      mutated <- dplyr::mutate(non_base,
        pv = sprintf("%s (>= %s)", .data$Package, .data$Version)
      )
      sel_mutated <- dplyr::select(mutated, "pv")
      pkg_plus_version <- purrr::flatten_chr(sel_mutated)

      sort(c(pkg_plus_version, base))
    }) -> pkg_deps
    cat("Package: ", pkg, "\n", sep = "")
    purrr::walk(names(pkg_deps), function(x) {
      cat(x, ":\n", sep = "")
      sprint_deps <- sprintf("    %s", pkg_deps[[x]])
      sprint_collapsed <- paste0(sprint_deps, collapse = ",\n")
      cat(sprint_collapsed)
      cat("\n")
    })
  }
