% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare}
\alias{compare}
\title{Compare daily data and season Harvest Survey data}
\usage{
compare(daily_data, season_data, type = "line", ylim = NA)
}
\arguments{
\item{daily_data}{Daily data table}

\item{season_data}{Season data table}

\item{type}{Default is "line"
\itemize{
\item "line" - returns a scatter plot with lines to visualize the relationship between number of birds retrieved and number of days spent hunting when data are divided into 4 submission groups
\item "days" - returns the distribution of number of days hunted per submission group; bubbles represent number of birds retrieved and red lines indicate mean values
\item "retrieved" - returns the distribution of number of birds retrieved per submission group; bubbles represent number of birds retrieved and red lines indicate mean values
}}

\item{ylim}{Optional value to be supplied if an outlier stretches the y-axis; defaults to NA}
}
\description{
The \code{compare} function takes 2 data tables (daily_data and season_data) to plot 3 data fields (bag size, number of birds retrieved, number of days hunted) and breaks those data into 4 groups: season submitted, daily submitted, season non-submit, and daily non-submit.
}
\references{
\url{https://github.com/USFWS/migbirdHS}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
