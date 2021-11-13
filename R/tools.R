#' A title case for AP stylebook
#' @description Converting to title case which should follow the AP stylebook
#'   in most cases.
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
#' @seealso https://en.wikipedia.org/wiki/Title_case
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

#' anonymize
#' @name anonymize
#' @aliases anonymise
#' @description The function will anonymize your data
#' @author Modified from Stackoverflow from user Etienne Low-Décarie and Gavin Simpson
#' @param df data frame to anonymise
#' @param simple_names logical, column names will be converted to letters. If FALSE, colString will be used for renaming
#' @param colString how the columns are to be named
#' @param rowString how the rows are to be named
#' @param replace_rownames if TRUE, replacement with string from rowString argument
#' @return Data frame
#' @seealso https://stackoverflow.com/a/10455729/7941188
#' and https://stackoverflow.com/a/10458688/7941188
#'
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


#' mysplit
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

#' every_nth
#' @author  Stackoverflow user adamdsmith
#' @description creates vector with empty lables for custom major/minor ticks
#' @param x vector
#' @param nth how many gaps
#' @param empty how to fill
#' @param inverse if TRUE removes every nth instead
#' @return char vector
#' @seealso https://stackoverflow.com/a/34533473/7941188
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

#' Partial string matching and replacing
#' @name match_part
#' @param x Vector. Values to be matched
#' @param table Vector. characters to be matched against
#' @return A list of indices of partial matches
#' @importFrom stats setNames
#' @export
match_part <- function(x, table){
  lu <- setNames(lapply(table, grep, x = x, ignore.case = TRUE), table)
  lu
}

#' Partial string matching and replacing
#' @rdname match_part
#' @param x Vector. Values to be matched
#' @param replace Vector of new labels if a partial match is found.
#'     If named vector, names are used. If more than one match is found,
#'     the last match will replace the original value.
#' @param no_match if there is no partial match
#' @return Character vector with replaced values
#' @importFrom utils stack
#' @examples
#' oceans <- c("pacific", "atlantic", "indian", "mediterranean")
#' oceans_long <- c("Eastern Central Atlantic", "Eastern Central Pacific", "Eastern Indian Ocean",
#'   "Mediterranean and Black Sea", "Northeast Central Atlantic",
#'   "Northeast Pacific", "Northwest Central Atlantic", "Northwest Pacific",
#'   "Southeast Central Atlantic", "Southeast Pacific", "Southwest Atlantic",
#'   "Southwest Pacific", "Western Central Atlantic", "Western Central Pacific",
#'   "Western Indian Ocean", "World", "Atlantic Pacific")
#'
#' match_replace(oceans_long, oceans, "Other")
#' @export
match_replace <- function(x, replace, no_match = NA_character_){
  lu <- stack(match_part(x, replace))
  x[lu$values] <- as.character(lu$ind)
  x[-lu$values] <- no_match
  x
}



