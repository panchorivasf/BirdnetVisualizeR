% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_birdnet.R
\name{import_birdnet}
\alias{import_birdnet}
\title{Import BirdNET data into R}
\usage{
import_birdnet(files, format = "csv", conf = 0.5, combined = TRUE)
}
\arguments{
\item{files}{Files to be imported}

\item{format}{Character. The file format; either "csv" or "txt".}

\item{conf}{Character. Filter by confidence level. Defaults to 0.5 (50%).}

\item{combined}{Logical. Whether to analyze a single combined CSV. Defaut is
TRUE.}
}
\value{
a tibble.
}
\description{
Import BirdNET data into R
}
\examples{
 \dontrun{
 csvs <- list_csvs()
 import_birdnet(csvs)
 }
}
