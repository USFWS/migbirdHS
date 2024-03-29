% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrievedmap.R
\name{retrievedmap}
\alias{retrievedmap}
\title{Map mean retrieved birds per species and state}
\usage{
retrievedmap(data, output = "grid")
}
\arguments{
\item{data}{Daily data table}

\item{output}{Default is "grid"
\itemize{
\item "grid" - returns a single plot with a map for each species arranged in a grid
\item "series" - returns each individual species plot in the "Plots" pane; toggle using the GUI left and right arrows
}}
}
\description{
The \code{retrievedmap} function creates a hex bin map of the United States from daily Harvest Survey data. Maps are plotted in a grid to display all species. States that are red have the highest mean harvest, states that are yellow have the lowest mean harvest, and blank (or white) states have no data.
}
\references{
\url{https://github.com/USFWS/migbirdHS}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
