#' UK_bankholidays
#' @description  modified function timeDate::holidayLONDON -
#'   for description see ?timeDate::holidayLONDON
#'
#'   includes names of bank holidays in output vector
#' @param year an integer value or vector of years, formatted as YYYY.
#' @return vector of POSIX.
#' @importFrom timeDate timeDate
#' @importFrom timeDate getRmetricsOptions
#' @importFrom timeDate ChristmasDay
#' @export
uk_bankholidays <- function(year = timeDate::getRmetricsOptions("currentYear")) {
  holidays <- NULL
  for (y in year) {
    if (y >= 1834 & y <= 1870) {
      dts <- c(paste0(y, "-05-01"), paste0(y, "-11-01"))
      holidays <- c(holidays, dts,
                    GoodFriday = as.character(timeDate::GoodFriday(y)),
                    ChristmasDay = as.character(timeDate::ChristmasDay(y))
      )
    }
    if (y >= 1871) {
      holidays <- c(holidays,
                    GoodFriday = as.character(timeDate::GoodFriday(y)), EasterMonday =
                      as.character(timeDate::EasterMonday(y))
      )
      if (y <= 1964) {
        holidays <- c(holidays, Easter = as.character(timeDate::Easter(
          y,
          50
        )))
        lon <- timeDate::timeDate(timeDate::.on.or.after(y, 8, 1, 1),
                                  zone = "London",
                                  FinCenter = "Europe/London"
        )
        holidays <- c(holidays, as.character(lon))
      }
      else {
        if (y == 2002) {
          dts <- c(paste0(y, "-06-03"), paste0(y, "-06-04"))
          holidays <- c(holidays, dts)
        }
        else if (y == 2012) {
          dts <- c(SpringBank = paste0(y, "-06-04"), QueenDiamond = paste0(y, "-06-05"))
          holidays <- c(holidays, dts)
        }
        else {
          lon <- timeDate::timeDate(timeDate::.last.of.nday(y, 5, 31, 1),
                                    zone = "London", FinCenter = "Europe/London"
          )
          holidays <- c(holidays, SpringBank = as.character(lon))
        }
        lon <- timeDate::timeDate(timeDate::.last.of.nday(y, 8, 31, 1),
                                  zone = "London",
                                  FinCenter = "Europe/London"
        )
        holidays <- c(holidays, SummerBank = as.character(lon))
      }
      if (y < 1970) {
        holidays <- c(holidays,
                      ChristmasDay = as.character(timeDate::ChristmasDay(y)),
                      BoxingDay = as.character(timeDate::BoxingDay(y))
        )
      }
      else {
        posix1 <- as.POSIXlt(timeDate::ChristmasDay(y))
        if (posix1$wday == 0) {
          holidays <- c(holidays, ChristmasDay = as.character(timeDate::ChristmasDay(y) +
                                                                (1:2) * 86400))
        }
        else if (posix1$wday == 6) {
          holidays <- c(holidays, ChristmasDay = as.character(timeDate::ChristmasDay(y) +
                                                                (2:3) * 86400))
        }
        else if (posix1$wday == 5) {
          holidays <- c(holidays, ChristmasDay = as.character(timeDate::ChristmasDay(y) +
                                                                c(0, 3) * 86400))
        }
        else {
          holidays <- c(holidays,
                        ChristmasDay = as.character(timeDate::ChristmasDay(y)),
                        BoxingDay = as.character(timeDate::BoxingDay(y))
          )
        }
      }
      if (y >= 1974) {
        posix1 <- as.POSIXlt(timeDate::NewYearsDay(y))
        if (posix1$wday == 0 | posix1$wday == 6) {
          lon <- timeDate::timeDate(timeDate::.on.or.after(y, 1, 1, 1),
                                    zone = "London",
                                    FinCenter = "Europe/London"
          )
          holidays <- c(holidays, NewYear = as.character(lon))
        }
        else {
          holidays <- c(holidays, NewYear = as.character(posix1))
        }
      }
      if (y >= 1978) {
        if (y == 1981) {
          dts <- paste0(y, "-07-29")
          holidays <- c(holidays, EarlyMay = dts)
        }
        if (y == 2011) {
          dts <- paste0(y, "-04-29")
          holidays <- c(holidays, EarlyMay = dts)
        }
        if (y == 1995) {
          dts <- paste0(y, "-05-08")
          holidays <- c(holidays, EarlyMay = dts)
        }
        else {
          lon <- timeDate::timeDate(timeDate::.on.or.after(y, 5, 1, 1),
                                    zone = "London",
                                    FinCenter = "Europe/London"
          )
          holidays <- c(holidays, EarlyMay = as.character(lon))
        }
      }
    }
  }
  holidays <- sort(holidays)
  ans <- timeDate::timeDate(format(holidays), zone = "London", FinCenter = "Europe/London")
  posix1 <- as.POSIXlt(ans, tz = "GMT")
  ans[!(posix1$wday == 0 | posix1$wday == 6)]
}
