% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_week.R
\name{weeks}
\alias{weeks}
\alias{week_ceiling}
\alias{week_floor}
\title{Show ceiling / floor days of a week}
\usage{
week_ceiling(x, weekend = NULL)

week_floor(x, weekend = NULL)
}
\arguments{
\item{x}{Vector of dates}

\item{weekend}{what's the end of week? Default to Sunday}
}
\value{
vector of dates. can be used in aggregations, e.g. with zoo.aggregate
}
\description{
Takes vector of dates and outputs floor or ceiling date of the respective week.
The end of week can be defined. The output can be used e.g. for zoo aggregation.
This is inspired tremendously by Stackoverflow users G.Grothendieck and Joshua Ulrich
}
\section{Weekend}{

weekday names in locale (!!) If you
want to pass in a different locale, use index instead. Also accepts
index 0:6 (d0 = Sunday, d1 = Monday,..., d6 = Saturday)
}

\examples{
week_ceiling(as.Date("2021-03-24"))
week_ceiling(as.Date("2021-03-24"), weekend = "friday")
week_ceiling(as.Date("2021-03-24"), weekend = 1) # 1 = Monday
week_floor(as.Date("2021-03-24"))
week_floor(as.Date("2021-03-24"), weekend = 5) # 5 = Friday
}
\seealso{
https://stackoverflow.com/a/4351626/7941188 and
https://stackoverflow.com/a/16193667/7941188
}
