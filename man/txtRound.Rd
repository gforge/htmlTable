% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/txtFrmt_round.R, R/txtFrmt_round_data.frame.R
\name{txtRound}
\alias{txtRound}
\alias{txtRound.default}
\alias{txtRound.table}
\alias{txtRound.matrix}
\alias{txtRound.data.frame}
\title{A convenient rounding function}
\usage{
txtRound(x, ...)

\method{txtRound}{default}(
  x,
  digits = 0,
  digits.nonzero = NA,
  txt.NA = "",
  dec = getOption("htmlTable.decimal_marker", default = "."),
  scientific = NULL,
  txtInt_args = getOption("htmlTable.round_int", default = NULL),
  ...
)

\method{txtRound}{table}(x, ...)

\method{txtRound}{matrix}(x, digits = 0, excl.cols = NULL, excl.rows = NULL, ...)

\method{txtRound}{data.frame}(x, ..., digits = 0L)
}
\arguments{
\item{x}{The value/vector/data.frame/matrix to be rounded}

\item{...}{Passed to next method}

\item{digits}{The number of digits to round each element to. For \code{matrix}
or \code{data.frame} input you can provide a \code{vector}/\code{list}. An unnamed \code{vector}/\code{list}
must equal the length of the columns to round. If you provide a named vector you
can provide specify per column the number of digits, and then use \code{.default}
for those columns that we don't need to have separate values for.}

\item{digits.nonzero}{The number of digits to keep if the result is close to
zero. Sometimes we have an entire table with large numbers only to have a
few but interesting observation that are really interesting}

\item{txt.NA}{The string to exchange \code{NA} with}

\item{dec}{The decimal marker. If the text is in non-English decimal
and string formatted you need to change this to the appropriate decimal
indicator. The option for this is \code{htmlTable.decimal_marker}.}

\item{scientific}{If the value should be in scientific format.}

\item{txtInt_args}{A list of arguments to pass to \code{\link[=txtInt]{txtInt()}} if that is to be
used for large values that may require a thousands separator. The option
for this is \code{htmlTable.round_int}. If \code{TRUE} it will activate the \code{txtInt}
functionality.}

\item{excl.cols}{Columns to exclude from the rounding procedure when provided a matrix.
This can be either a number or regular expression. Skipped if \code{x} is a vector.}

\item{excl.rows}{Rows to exclude from the rounding procedure when provided a matrix.
This can be either a number or regular expression.}
}
\value{
\code{matrix/data.frame}
}
\description{
Regular round often looses trailing 0:s as these are truncated, this function
converts everything to strings with all 0:s intact so that tables have the
correct representation, e.g. \code{txtRound(1.01, digits = 1)} turns into \code{1.0}.
}
\section{Tidy-select with \code{data.frame}}{


The \code{txtRound} can use \code{data.frame} for input. This allows us to use
\href{https://tidyselect.r-lib.org/articles/tidyselect.html}{tidyselect}
patterns as popularized by \strong{dplyr}.
}

\examples{
# Basic usage
txtRound(1.023, digits = 1)
# > "1.0"

txtRound(pi, digits = 2)
# > "3.14"

txtRound(12344, digits = 1, txtInt_args = TRUE)
# > "12,344.0"


# Using matrix
mx <- matrix(c(1, 1.11, 1.25,
               2.50, 2.55, 2.45,
               3.2313, 3, pi),
             ncol = 3, byrow=TRUE)
txtRound(mx, digits = 1)
#> [,1]  [,2]  [,3]
#> [1,] "1.0" "1.1" "1.2"
#> [2,] "2.5" "2.5" "2.5"
#> [3,] "3.2" "3.0" "3.1"

# Using a data.frame directly
library(magrittr)
data("mtcars")
# If we want to round all the numerical values
mtcars \%>\%
  txtRound(digits = 1)

# If we want only want to round some columns
mtcars \%>\%
  txtRound(wt, qsec_txt = qsec, digits = 1)
}
\seealso{
Other text formatters: 
\code{\link{txtInt}()},
\code{\link{txtMergeLines}()},
\code{\link{txtPval}()}
}
\concept{text formatters}
