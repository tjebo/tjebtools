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

#' every_nth
#' @author  SO::adamdsmith
#' @description creates vector with empty lables for custom major/minor ticks
#' @param x vector
#' @param nth how many gaps
#' @param empty how to fill
#' @param inverse if TRUE removes every nth instead
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
#' @param colString how the columns are to be named
#' @param rowString how the rows are to be named
#' @param replace_rownames if TRUE, replacement with string from rowString argument
#' @return Data frame
#' @export

anonymize <- function(df, simple_names = TRUE,
                      replace_rownames = FALSE,
                      colString = "var",
                      rowString = "sample") {
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


#' csv
#' @description Because I am using split so often, in order to split by
#'   a single column.
#' @name mysplit
#' @param x data frame
#' @param col column to split by
#' @family convenience functions
#' @return list
#' @examples
#' mysplit(mtcars, cyl)
#' @export
mysplit <- function(x, col) {
  newcol <- deparse(substitute(col))
  split(x, x[[newcol]])
}

#' Title case
#' @description Converting to title case in a clever way
#' @name str_title
#' @param x string to convert to title case
#' @param strict FALSE: Abbreviations are kept all CAPS -
#'   in fact, all letters after the first are kept in their original case
#' @param except Exceptions to be kept in lower case. By default:
#'    except <- c("and", "or", "nor", "but", "a", "an", "the", "as", "at",
#'    "by", "for", "in", "of", "on", "per", "to")
#' @family convenience functions
#' @seealso [toupper]
#' @return string
#' @examples
#' str_title(c("using AIC for model selection"))
#' @export
str_title <- function(x, strict = FALSE, except = NULL) {
  if (is.null(except)) {
    except <- c(
      "and", "or", "nor", "but", "a", "an",
      "the", "as", "at", "by", "for", "in", "of", "on", "per", "to"
    )
  }

  cap <- function(s) {
    paste0(toupper(substring(s, 1, 1)), {
      s_2plus <- substring(s, 2)
      if (strict) tolower(s_2plus) else s_2plus
    })
  }
  sapply(x, function(x) {
    x <- as.character(x)
    split_s <- unlist(strsplit(x, split = "\\s+"))

    newstring <-
      sapply(
        split_s,
        function(x) {
          if (x %in% except) {
            return(x)
          } else {
            cap(x)
          }
        }
      )
    paste(newstring, collapse = " ")
  }, USE.NAMES = !is.null(names(x)))
}

#' Pipe operator
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
