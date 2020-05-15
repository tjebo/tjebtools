#' eyes
#' @name eyes
#' @description Looks for columns that identify patients and eyes and counts number of patients and eyes.
#' @param date dataframe where patient / eye information is included.
#' @param id id column. If not specified, automatically selected. In this case, the patient ID column name needs to contain the strings "pat" OR "id" ignore.case = TRUE.
#' @param eye eye colum. If not specified, automatically selected, in which case the eye column name needs to contain the string "eye" ignore.case = TRUE
#' @param text default = FALSE (TRUE will return text which can be pasted for example into a markdown document). As default a named vector will be returned
#' @examples
#' set.seed(42)
#' foo <- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c("r","l"))
#' eyes(foo)
#' @export
#'
eyes <- function(data, id = NULL, eye = NULL, text = FALSE) {
  x <- eval(data)

  if (missing(id)) {
    pat_col <- names(x)[grepl("pat|id", names(x), ignore.case = TRUE)]
  } else {
    pat_col <- id
  }

  if (missing(eye)) {
    eye_col <- names(x)[grepl("eye", names(x), ignore.case = TRUE, perl = TRUE)]
  } else {
    eye_col <- eye
  }

  if (length(eye_col) < 1 | identical(eye_col, character(0))) {
    warning("No eye column found in data nor specified: No eye count.", call. = FALSE)
  }

  if (length(pat_col) < 1) {
    stop("Patient and/or eye column(s) are missing.\n The patient ID column name needs to contain either of or both strings \"pat\" and \"ID\" at any location.\n The eye column name needs to contain the string \"eye\" (for both columns ignore.case = TRUE)")
  }

  if (length(eye_col) > 1 | length(pat_col) > 1) {
    stop("Patient and/or eye column(s) are not uniquely identified.")
  }

  n_pat <- length(unique(x[[pat_col]]))

  if (length(eye_col) == 1) {

    if(!all(tolower(unique(x[[eye_col]])) %in% c(NA, "r","l", "re","le","od","os")))
      stop("Eyes contain unclear categories.
           Must be \"r\", \"l\", \"re\", \"le\", \"od\", \"os\" - ignore cases!", call. = FALSE)
    eye_tab <- table(unique(x[,c(pat_col,eye_col)]))
    n_eyes <- sum(colSums(eye_tab))
    tab_r <- eye_tab[, tolower(colnames(eye_tab)) %in% c("r","re","od"), drop = FALSE]
    n_r <- sum(colSums(tab_r))
    tab_l <- eye_tab[, tolower(colnames(eye_tab)) %in% c("l","le","os"), drop = FALSE]
    n_l <- sum(colSums(tab_l))

    if (text) {
      return(paste(n_eyes, "eyes of", n_pat, "patients"))
    }
    return(c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l))
  } else {
    return(c(patients = n_pat))
  }
}

#' count_eyes
#' @rdname eyes
#' @export
counteyes <- eyes

#' count_eyes
#' @rdname eyes
#' @export
count_eyes <- eyes

#' get_age
#' @author SO::Moody_Mudskipper, mildly modified
#' @description calculates age in years, either as duration or as period
#' @param from_date start date
#' @param to_date end date
#' @param period default FALSE: output as a duration. If TRUE, output as a period
#' @export

get_age <- function(from_date, to_date = lubridate::now(), period = FALSE){
  if(!require('lubridate'))
    stop('Please install the lubridate package')
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (period) { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))
  age
  } else { age <- lubridate::interval(start = from_date, end = to_date)/lubridate::dyears(1)
  age
  }
}


#' show_stats
#'
#' @description pulls the most commonly used summary statistics
#' @param x either vector or list of vectors. Data frames supported. Character list elements (or columns) are removed!!
#' @param dec how many decimals are displayed
#' @param rownames logical. if FALSE, column with variable names will be created
#' @return named vector (for vector) or data frame (for list)
#' @examples
#' x = y = z = c(rnorm(20), NA)
#'
#' # named or unnamed list
#' mylist <- list(x = x, y = y, z = z)
#' show_stats(mylist)
#' # with a data frame
#' mydf <- data.frame(x, y, z)
#' show_stats(mydf)
#' #If aggregation by group, split the data frame first
#' mydf2 <- data.frame(group = rep(letters[1:2], each = 42), x, y, z)
#' lapply(split(mydf2, mydf2$group), show_stats, rownames = FALSE)
#' @export

show_stats <- function(x, dec = 1, rownames = TRUE) {
  if(!require('purrr'))
    stop('Please install the purrr package')
  funs <- list(
    mean = purrr::partial(mean, na.rm = T),
    sd = purrr::partial(sd, na.rm = T),
    n = length,
    median = purrr::partial(median, na.rm = TRUE),
    min = purrr::partial(min, na.rm = TRUE),
    max = purrr::partial(max, na.rm = TRUE)
  )

  if (is.atomic(x) == TRUE) {
    r1 <- lapply(funs, function(f) f(x))

    if(!rownames){
      cbind(var = rownames(unlist(r1)), data.frame(unlist(r1), row.names=NULL))
      } else {
      unlist(r1)
    }
  } else if (typeof(x) == "list") {
    x_num <- Filter(is.numeric, x)
    if(!identical(x, x_num)) warning("Character columns or list elements removed")
    result <- lapply(funs, mapply, x_num)
    list_res <- lapply(result, function(y) round(y, digits = dec))
    if(!rownames){
      cbind(var = rownames(data.frame(list_res)), data.frame(data.frame(list_res), row.names=NULL))
    } else {
      data.frame(list_res)
    }

  }
}


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

weekly <- function(x, week_end = 'sunday', ceiling = TRUE) {
  if(!require('zoo'))
    stop('Please install the zoo package')
  week_end <- tolower(week_end)
  days.of.week <- tolower(weekdays(as.Date(3,"1970-01-01",tz="GMT") + 0:6))
  if(ceiling) {
    index = which(days.of.week == week_end)-1
    vec <- 7 * ceiling(as.numeric(x - index + 4)/7) + zoo::as.Date(index - 4)
  vec
  } else {index = which(days.of.week == week_end)
  vec <- 7 * floor(as.numeric(x - index + 4)/7) + zoo::as.Date(index - 4)}
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
#' @author With the help of Stackoverflow Etienne Low-Décarie and G.Simpson, modified
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
#' @description wrapper around write.csv with default 'row.names = FALSE'
#' @name csv
#' @param x data frame
#' @param name Filename. Default: Name of dataframe to save as csv. Or character string (.csv extension added automatically)
#' @export

csv <- function(x, name = deparse(substitute(x))) {
  file = paste0(name, '.csv')
  write.csv(x, file, row.names = F)
}
