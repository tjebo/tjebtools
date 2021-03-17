#' se_diff_means
#' @name se_diff_means
#' @description The function will compute the standard error of the
#'   differences of the means between two samples
#' @param x sample one - as numeric vector
#' @param y sample two
#' @param na.rm passed to sd
#' @importFrom stats sd
#' @return numeric value
#' @seealso
#'   https://stats.stackexchange.com/questions/302445/standard-error-of-difference
#'   https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one/7-t-tests
#' @export
se_diff_means <- function(x, y, na.rm = TRUE) {
  sd1 <- sd(x, na.rm = na.rm)
  sd2 <- sd(y, na.rm = na.rm)
  n1 <- length(x[!is.na(x)])
  n2 <- length(y[!is.na(y)])
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)) * sqrt(1 / n1 + 1 / n2)
}

