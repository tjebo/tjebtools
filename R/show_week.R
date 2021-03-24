#' Show ceiling / floor days of a week
#' @name weeks
#' @description Takes vector of dates and outputs floor or ceiling date of the respective week.
#' The end of week can be defined. The output can be used e.g. for zoo aggregation.
#' This is inspired tremendously by Stackoverflow users G.Grothendieck and Joshua Ulrich
#' @param x Vector of dates
#' @param weekend what's the end of week? Default to Sunday
#' @section Weekend:
#'  weekday names in locale (!!) If you
#'  want to pass in a different locale, use index instead. Also accepts
#'  index 0:6 (d0 = Sunday, d1 = Monday,..., d6 = Saturday)
#'
#' @return vector of dates. can be used in aggregations, e.g. with zoo.aggregate
#' @seealso https://stackoverflow.com/a/4351626/7941188 and
#'   https://stackoverflow.com/a/16193667/7941188
#' @examples
#' week_ceiling(as.Date("2021-03-24"))
#' week_ceiling(as.Date("2021-03-24"), weekend = "friday")
#' week_ceiling(as.Date("2021-03-24"), weekend = 1) # 1 = Monday
#' week_floor(as.Date("2021-03-24"))
#' week_floor(as.Date("2021-03-24"), weekend = 5) # 5 = Friday
#' @export
week_ceiling <- function(x, weekend = NULL) {
  index <- get_index(weekend)
  next_weekday(x, index, "ceiling")
}
#' string of weekdays
#' @rdname weeks
#' @export
week_floor <- function(x, weekend = NULL) {
  index <- get_index(weekend)
  next_weekday(x, index, "floor")
}

#' define index
#' @keywords internal
get_index <- function(weekend = NULL) {
  if (is.null(weekend)) weekend <- 0
  if (is.na(suppressWarnings(as.integer(weekend)))) {
    week_end <- tolower(weekend)
    weekend <- seq(0, 6)[tolower(days(0)) == week_end]
  }
  weekend
}

#' names of weekdays
#' @description a convenient function to create a vector of weekdays
#'   in the locale
#' @param abbreviate logical vector (possibly recycled).
#'   Should the names be abbreviated?
#' @param index with which day in the week 0:6 shall the vector start,
#'   d=0 is Sunday, d=1 is Monday, ..., d=6 is Saturday
#' @name days
#' @export
days <- function(index = 1, abbreviate = FALSE) {
  weekdays(as.Date(index - 4, "1970-01-01", tz = "GMT") + 0:6,
           abbreviate = abbreviate)
}

#' Next weekday to a given date
#' @description Gives the date of a given weekday to a given date
#' @param x date vector
#' @param index which day in the week 0:6,
#'   d=0 is Sunday, d=1 is Monday, ..., d=6 is Saturday
#' @param method "ceiling": last day of the week given the weekend.
#'   "floor": first day of week
#' @details
#' The idea is that relative to the UNIX Epoch that d-4 falls on day of
#' the week d where d=0 is Sunday, d=1 is Monday, ..., d=6 is Saturday
#' so any multiple of 7 days from that also falls on day of the week d.
#' @name next_weekday
#' @importFrom zoo as.Date
#' @export
next_weekday <- function(x, index, method = "ceiling") {
  if (method == "ceiling") {
    7 * ceiling(as.numeric(x - index + 4) / 7) + zoo::as.Date(index - 4)
  }
  else if (method == "floor") {
    7 * floor(as.numeric(x - index + 4) / 7) + zoo::as.Date(index - 4)
  }
}

