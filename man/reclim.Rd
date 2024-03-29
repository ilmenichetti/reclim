% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclim.R
\name{reclim}
\alias{reclim}
\title{Wrapper for the functions calculating the ICBM climate scaling factors}
\usage{
reclim(
  weather,
  aboveground,
  sun.mode,
  latitude,
  altitude,
  depth,
  sand = NULL,
  clay = NULL,
  ave_SOC = NULL,
  porosity = NULL,
  wilting_point = NULL,
  field_capacity = NULL
)
}
\arguments{
\item{weather}{data matrix of weather data, must be exactly in the format of the  \link{weather_testdata} attached as example and contain the following headers:
("date", "year", "month", "day", "air_temp_deg_C", "precipitation_mm", "windspeed_kmh", "humidity_percent", "Rsolar_lang")}

\item{aboveground}{data matrix of weather data, must be exactly in the format of the  \link{aboveground_testdata} attached as example and contain the following headers:
("year", "crop_description", "crop_id", "treat", "variance", "seeding", "harvest", "harvest2", "tillage", "minimum_cover", "total_dm_kg_ha", "total_dm_kg_ha2" )
"harvest2" and "total_dm_kg_ha2" are optional and used in case of a double cut for leys}

\item{sun.mode}{mode of sun data, can be either "Rsolar" (expressed in Langleys) or "cloudiness" (expressed in percent of sunny time per day)}

\item{latitude}{the latitude, in degrees}

\item{altitude}{altitude in meters}

\item{depth}{depth considered in centimeters}

\item{sand}{sand, in \%. This is needed if porosity, wilting point and field capacity are not specified}

\item{clay}{clay, in \%. This is needed if porosity, wilting point and field capacity are not specified}

\item{ave_SOC}{average SOC over the whole period, in \%. This is needed if porosity, wilting point and field capacity are not specified}

\item{porosity}{soil porosity, as 0 to 1. If speciefied with wilting point and field capacity there's no need for other soil edaphic properties.}

\item{wilting_point}{wilting point, as mm over the total. If speciefied with porosity and field capacity there's no need for other soil edaphic properties.}

\item{field_capacity}{field capacity , as mm over the total. If speciefied with wilting point and porosity there's no need for other soil edaphic properties.}
}
\description{
This functions runs the re_clim calculation on a dataset (composed by two different tables, one daily for weather and one annual for aboveground biomass, please refer to the \link{aboveground_testdata} for the data structure)
The function is a wrapper, performing a few data checks and running functions to calculate several parameters
and hopefully runs without the user having to bother too much with intermediate steps.
}
\examples{

reclim_out<-reclim(weather=weather_testdata,
                  aboveground=aboveground_testdata,
                  latitude=44,
                  altitude=20,
                  sand=22,
                  clay=36,
                  ave_SOC=1.2,
                  depth=20,
                  sun.mode="Rsolar")

}
\author{
Lorenzo Menichetti \email{ilmenichetti@gmail.com}, Martin Bolinder, Olaf Andrén, Thomas Kätterer
}
