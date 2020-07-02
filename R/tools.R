#' weekly
#'
#' @author tjebo - eclectic choice of functions, mainly with the help of SO::G.Grothendieck and SO::Joshua Ulrich
#' https://stackoverflow.com/a/4351626/7941188 and https://stackoverflow.com/a/16193667/7941188
#'
#' @description Takes vector of dates and outputs floor or ceiling date of the respective week.
#' The end of week can be defined. The output can be used e.g. for zoo aggregation
#' @param x Vector of dates Dataframe in which the vectors are found
#' @param week_end what's the end of week? Takes english names of weekdays as quoted string, capital letters allowed
#' @param ceiling default TRUE: last day of the week for each weekend. if FALSE, first day of week
#' @return vector of dates. can be used in aggregations, e.g. with zoo.aggregate
#' @export

weekly <- function(x, week_end = "sunday", ceiling = TRUE) {
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

#' UK_bankholidays
#'
#' @author  modified function timeDate::holidayLONDON - for description see ?timeDate::holidayLONDON
#' @description includes NAMES of bank holidays in output vector
#' @return vector of POSIX.
#' @export

uk_bankholidays <- function (year = getRmetricsOptions("currentYear")) {
  if(!require('timeDate'))
    stop('Please install the timeDate package')

  holidays <- NULL
  for (y in year) {
    if (y >= 1834 & y <= 1870) {
      dts <- c(paste0(y, "-05-01"), paste0(y, "-11-01"))
      holidays <- c(holidays, dts, GoodFriday = as.character(GoodFriday(y)),
                    as.character(ChristmasDay(y)))
    }
    if (y >= 1871) {
      holidays <- c(holidays, GoodFriday = as.character(GoodFriday(y)), EasterMonday =
                      as.character(EasterMonday(y)))
      if (y <= 1964) {
        holidays <- c(holidays, Easter = as.character(Easter(y,
                                                             50)))
        lon <- timeDate::timeDate(.on.or.after(y, 8, 1, 1), zone = "London",
                        FinCenter = "Europe/London")
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
          lon <- timeDate::timeDate(.last.of.nday(y, 5, 31, 1),
                          zone = "London", FinCenter = "Europe/London")
          holidays <- c(holidays, SpringBank = as.character(lon))
        }
        lon <- timeDate::timeDate(.last.of.nday(y, 8, 31, 1), zone = "London",
                        FinCenter = "Europe/London")
        holidays <- c(holidays, SummerBank = as.character(lon))
      }
      if (y < 1970) {
        holidays <- c(holidays, ChristmasDay = as.character(ChristmasDay(y)),
                      BoxingDay = as.character(BoxingDay(y)))
      }
      else {
        posix1 <- as.POSIXlt(ChristmasDay(y))
        if (posix1$wday == 0) {
          holidays <- c(holidays, ChristmasDay = as.character(ChristmasDay(y) +
                                                                (1:2) * 86400))
        }
        else if (posix1$wday == 6) {
          holidays <- c(holidays, ChristmasDay = as.character(ChristmasDay(y) +
                                                                (2:3) * 86400))
        }
        else if (posix1$wday == 5) {
          holidays <- c(holidays, ChristmasDay = as.character(ChristmasDay(y) +
                                                                c(0, 3) * 86400))
        }
        else {
          holidays <- c(holidays, ChristmasDay = as.character(ChristmasDay(y)),
                        BoxingDay = as.character(BoxingDay(y)))
        }
      }
      if (y >= 1974) {
        posix1 <- as.POSIXlt(NewYearsDay(y))
        if (posix1$wday == 0 | posix1$wday == 6) {
          lon <- timeDate::timeDate(.on.or.after(y, 1, 1, 1), zone = "London",
                          FinCenter = "Europe/London")
          holidays <- c(holidays, NewYear = as.character(lon))
        }
        else {
          holidays <- c(holidays, NewYear = as.character(posix1))
        }
      }
      if (y >= 1978) {
        if (y == 1981) {
          dts <- paste0(y, "-07-29")
          holidays <- c(holidays, EarlyMay =dts)
        }
        if (y == 2011) {
          dts <- paste0(y, "-04-29")
          holidays <- c(holidays, EarlyMay =dts)
        }
        if (y == 1995) {
          dts <- paste0(y, "-05-08")
          holidays <- c(holidays, EarlyMay =dts)
        }
        else {
          lon <- timeDate::timeDate(.on.or.after(y, 5, 1, 1), zone = "London",
                          FinCenter = "Europe/London")
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


#' every_nth
#'
#' @author  SO::adamdsmith
#'
#' @description creates vector with empty lables for custom major/minor ticks
#' @return char vector
#' @export
#'
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

#' anonymize
#' @name anonymize
#' @aliases anonymise
#' @description The function will anonymize your data
#' @author Modified from [this](https://stackoverflow.com/a/10455729/7941188) and [this](https://stackoverflow.com/a/10458688/7941188)
#' post on Stackoverflow from user Etienne Low-Décarie and Gavin Simpson
#' @param df data frame to anonymise
#' @param simple_names logical, column names will be converted to letters. If FALSE, colString will be used for renaming
#' @param replace_rownames if TRUE, replacement with string from rowString argument
#' @return Data frame
#' @export

anonymize <- function(df, simple_names = TRUE, replace_rownames = FALSE, colString = "var", rowString = "sample") {
  level.id.df <- function(df) {
    if (simple_names) {
      if (length(df) > 26) {
        LETTERS <- replicate(floor(length(df) / 26), {
          LETTERS <- c(LETTERS, paste(LETTERS, LETTERS, sep = ""))
        })
      }
      colnames(df) <- paste(LETTERS[1:length(df)])
    } else {
      colnames(df) <- paste(colString, seq_len(ncol(df)), sep = "")
    }
    level.id <- function(i) {
      if (class(df[, i]) == "factor" | class(df[, i]) == "character") {
        column <- paste(names(df)[i], as.numeric(as.factor(df[, i])), sep = "_")
      } else if (is.numeric(df[, i])) {
        column <- round(scale(df[, i]), 2)
      } else {
        column <- df[, i]
      }
      return(column)
    }
    DF <- data.frame(sapply(seq_along(df), level.id))
    colnames(DF) <- colnames(df)
    return(DF)
  }
  df <- level.id.df(df)
  if (replace_rownames) {
    rownames(df) <- paste(rowString, seq_len(nrow(df)), sep = "")
  }
  return(df)
}

#' anonymise
#' @rdname anonymize
#' @export
anonymise <- anonymize


#' csv
#' @description wrapper around write.csv with default 'row.names = FALSE'.
#' Will use the name of the data frame for the generated .csv file.
#' @name csv
#' @param x data frame
#' @param name Filename (Default: Name of dataframe). If provided,
#'  character string (.csv extension added automatically)
#' @family convenience functions
#' @return No return value, called for side effects (saving file)
#' @examples
#' \dontrun{
#' csv(amd)
#' }
#' @export
csv <- function(x, name = NULL) {
  if(is.null(name)){
    name <- deparse(substitute(x))
  }
  file = paste0(name, '.csv')
  utils::write.csv(x, file, row.names = F)
}

#' Probability contours
#' @description calculates 2d probability contours for use in ggplot2.
#' Modified from [this post](https://stackoverflow.com/a/59173290/7941188) by user crsh
#' @name prob_contour
#' @param data data frame x and y coordinates
#' @param x column with x coordinates (default first column)
#' @param y column with y coordinates (default second column)
#' @param prob probability to be estimated
#' @param n passed to [MASS::kde2d]
#' @param ... further parameters passed to [MASS::kde2d]
#' @family stats functions
#' @export
#' @examples
#'
#' library(ggplot2)
#' set.seed(1)
#' n=100
#' foo <- data.frame(x=rnorm(n, 0, 1), y=rnorm(n, 0, 1))
#'
#' df_contours <- dplyr::bind_rows(
#'   purrr::map(seq(0.2, 0.8, 0.2), function(p) prob_contour(foo, prob = p))
#' )
#'
#' ggplot() +
#'   geom_point(data = foo, aes(x = x, y = y)) +
#'   geom_polygon(data = df_contours, aes(x = x, y = y, color = prob), fill = NA) +
#'   scale_color_brewer(name = "Probs", palette = "Set1")

prob_contour <- function(data, x = NULL, y = NULL, n = 50, prob = 0.95, ...) {
  if (ncol(data) > 2 & missing(x) & missing(y)) {
    warning("Data frame has more than two columns. Default to first two columns", call. = FALSE)
  } else if (ncol(data) > 2 & missing(x)) {
    warning("Data frame has more than two columns and x not specified.
            Default x to column 1", call. = FALSE)
  } else if (ncol(data) > 2 & missing(y)) {
    warning("Data frame has more than two columns and y not specified.
            Default y to column 2", call. = FALSE)
  }

  if (missing(x)) x <- 1L
  if (missing(y)) y <- 2L

    post1 <- MASS::kde2d(data[[x]], data[[y]], n = n, ...)
    dx <- diff(post1$x[1:2])
    dy <- diff(post1$y[1:2])
    sz <- sort(post1$z)
    c1 <- cumsum(sz) * dx * dy
    levels <- sapply(prob, function(x) {
      approx(c1, sz, xout = 1 - x)$y
    })
    df <- as.data.frame(
      grDevices::contourLines(post1$x, post1$y, post1$z, levels = levels)[[1]])
    df$x <- round(as.numeric(df$x), 3)
    df$y <- round(as.numeric(df$y), 3)
    df$level <- round(as.numeric(df$level), 2)
    df$prob <- rep(as.character(prob), nrow(df))

    df
  }
