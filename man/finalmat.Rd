% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finalmat.R
\name{finalmat}
\alias{finalmat}
\title{finalmat:Create corr matrix with the p values and correlation coefficients together}
\usage{
finalmat(dat1, dat2, dat3 = NA, outputCSV = F, filename = "insertfilename")
}
\arguments{
\item{dat1}{data frame containing combined correlation coeficients and p values as stars}

\item{dat2}{data frame containing combined correlation coeficients and p values as stars}

\item{dat3}{data frame containing combined correlation coeficients and p values as stars}

\item{outputCSV}{Should R output csv files. Default is False}

\item{filename}{string containing the name for the output final file, including ".csv" at the end}
}
\value{
csv file with combine raw, log and rank correlation coef. and p value as stars
}
\description{
finalmat:Create corr matrix with the p values and correlation coefficients together
}
