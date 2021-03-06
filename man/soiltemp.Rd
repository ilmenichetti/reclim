% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/re_crop.R
\name{soiltemp}
\alias{soiltemp}
\title{soiltemp
internal function for determining the soil temperature from the air temperature}
\usage{
soiltemp(L, GAI, date, temperature)
}
\arguments{
\item{L}{soil depth (in mm)}

\item{GAI}{ren area index daily values}

\item{date}{date vector (daily steps)}

\item{temperature}{(air temperature in degrees C)}
}
\value{
a vector with the daily soil temperature values
}
\description{
soiltemp
internal function for determining the soil temperature from the air temperature
}
\author{
Lorenzo Menichetti \email{ilmenichetti@gmail.com}
}
