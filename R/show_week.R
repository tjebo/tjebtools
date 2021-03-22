#' Show ceiling / floor days of a week
#' @name weeks
#' @description Takes vector of dates and outputs floor or ceiling date of the respective week.
#' The end of week can be defined. The output can be used e.g. for zoo aggregation.
#' This is inspired tremendously by Stackoverflow users G.Grothendieck and Joshua Ulrich
#' @param x Vector of dates
#' @param weekend what's the end of week? english (!) weekday names
#'   as character, cases are ignored
#' @param ceiling default TRUE: last day of the week for each weekend. if FALSE, first day of week
#' @return vector of dates. can be used in aggregations, e.g. with zoo.aggregate
#' @seealso https://stackoverflow.com/a/4351626/7941188 and
#'   https://stackoverflow.com/a/16193667/7941188
#' @examples
#' week_ceiling(as.Date("2021-03-24"))
#' week_ceiling(as.Date("2021-03-24"), weekend = "friday")
#' week_floor(as.Date("2021-03-24"))
#' @export
week_ceiling <- function(x, weekend = "sunday", ceiling = TRUE) {
  week_end <- tolower(weekend)
  week_days <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday",
                 "sunday")
  if (ceiling) {
    index <- which(week_days == week_end)
    vec <- 7 * ceiling(as.numeric(x - 1) / 7) +
    as.Date(index - 4, origin = "1970-01-01")
    vec
  }
  else {
    index <- which(week_days == week_end) + 1
    vec <- 7 * floor(as.numeric(x - 1) / 7) +
    as.Date(index - 4, origin = "1970-01-01")
  }
  vec
}

#' string of weekdays
#' @rdname weeks
#' @export
week_floor <- function(x, weekend = "sunday", ceiling = FALSE){
  week_ceiling(x = x, ceiling = ceiling, weekend = weekend)
}

#' names of days
#' @description a convenient function to create a vector of weekdays
#'   in the locale
#' @param abbreviate logical vector (possibly recycled).
#'   Should the names be abbreviated?
#' @rdname weeks
#' @export
days <- function(abbreviate = FALSE) {
  weekdays(as.Date(4, "1970-01-01", tz = "GMT") + 0:6,
           abbreviate = abbreviate)
}

