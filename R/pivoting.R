#' pivoting
#' @name wider
#' @description My own twist on tidyr::pivot_longer and tidyr::pivot_wider,
#'   just for convenience
#' @inheritParams tidyr::pivot_wider
#' @return data frame
#' @importFrom tidyr pivot_wider
#' @export
wider <- function(data, names_from, values_from, ...){
  tidyr::pivot_wider(data,
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
#' @importFrom tidyselect eval_select
#' @importFrom rlang enquo
#' @export
longer <- function(data, cols, names_to, values_to, ...){
  colexpr <- rlang::enquo(cols)
  newcols <- tidyselect::eval_select(colexpr, data = data)
  tidyr::pivot_longer(data, cols = newcols,
              names_to = names_to,
              values_to = values_to,
              ...)
}
