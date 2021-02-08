#' pivoting
#' @name wider
#' @description My own twist on tidyr::pivot_longer and tidyr::pivot_wider,
#'   just for convenience
#' @inheritParams tidyr::pivot_wider
#' @return data frame
#' @importFrom tidyr pivot_wider
#' @export
wider <- function(x, names_from, values_from, ...){
  tidyr::pivot_wider(x,
              names_from = names_from,
              values_from = values_from,
              ...)
}

#' pivoting
#' @name longer
#' @description My own twist on tidyr::pivot_longer and tidyr::pivot_wider,
#'   just for convenience
#' @inheritParams tidyr::pivot_longer
#' @return data frame
#' @importFrom tidyr pivot_longer
#' @export
longer <- function(x, cols, names_to, values_to, ...){
  colexpr <- rlang::enquo(cols)
  newcols <- tidyselect::eval_select(colexpr, data = x)
  tidyr::pivot_longer(x, cols = newcols,
              names_to = names_to,
              values_to = values_to,
              ...)
}
