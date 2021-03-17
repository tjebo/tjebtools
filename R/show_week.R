#' Show ceiling / floor days of a week
#'
#' @description Takes vector of dates and outputs floor or ceiling date of the respective week.
#' The end of week can be defined. The output can be used e.g. for zoo aggregation.
#' This is inspired tremendously by Stackoverflow users G.Grothendieck and Joshua Ulrich
#' @param x Vector of dates Dataframe in which the vectors are found
#' @param week_end what's the end of week? Takes english names of weekdays as quoted string, capital letters allowed
#' @param ceiling default TRUE: last day of the week for each weekend. if FALSE, first day of week
#' @return vector of dates. can be used in aggregations, e.g. with zoo.aggregate
#' @seealso https://stackoverflow.com/a/4351626/7941188 and
#'   https://stackoverflow.com/a/16193667/7941188
#' @export

show_week <- function(x, week_end = "sunday", ceiling = TRUE) {
  week_end <- tolower(week_end)
  days.of.week <- tolower(weekdays(base::as.Date(3,
                                                 origin = "1970-01-01",
                                                 tz = "GMT"
  ) + 0:6))
  if (ceiling) {
    index <- which(days.of.week == week_end) - 1
    vec <- 7 * ceiling(as.numeric(x - index + 4) / 7) + base::as.Date(index -
                                                                        4, origin = "1970-01-01")
    vec
  }
  else {
    index <- which(days.of.week == week_end)
    vec <- 7 * floor(as.numeric(x - index + 4) / 7) + base::as.Date(index -
                                                                      4, origin = "1970-01-01")
  }
  vec
}
