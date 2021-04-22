#' se_diff_means
#' @name se_diff_means
#' @description The function will compute the pooled standard error of the
#'   differences of the means between two independent samples,
#'   with equal variances (!)
#' @param x sample one - as numeric vector
#' @param y sample two
#' @param na.rm passed to sd
#' @importFrom stats sd
#' @return numeric vector of length 1
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

#' standard error (of the mean)
#' @name se_mean
#' @description Gets the standard error of the mean of a sample
#' @param x sample one
#' @param na.rm NA removed
#' @importFrom stats var
#' @return numeric vector of length 1
#' @seealso
#' https://stackoverflow.com/a/7220087/7941188
#' @export
se_mean <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

