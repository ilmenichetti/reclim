% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/re_crop.R
\name{poros}
\alias{poros}
\title{Internal function for determining the soil porosity from texture.}
\usage{
poros(sand, clay, SOC)
}
\arguments{
\item{sand}{sand content\%}

\item{clay}{clay content \% (optional)}

\item{SOC}{SOC content \%}
}
\value{
a single numerical value with the soil porosity
}
\description{
If clay is present the function uses  Toth et al., 2015, otherwise  Kätterer et al., 2006
}
\references{
Tóth, B., M. Weynants, A. Nemes, A. Makó, G. Bilas, and G. Tóth. 2015. “New Generation of Hydraulic Pedotransfer Functions for Europe: New Hydraulic Pedotransfer Functions for Europe.” European Journal of Soil Science 66 (1): 226–38. https://doi.org/10.1111/ejss.12192.
Kätterer, T., O. Andrén, and P-E. Jansson. 2006. “Pedotransfer Functions for Estimating Plant Available Water and Bulk Density in Swedish Agricultural Soils.” Acta Agriculturae Scandinavica, Section B - Plant Soil Science 56 (4): 263–76. https://doi.org/10.1080/09064710500310170.
}
\author{
Lorenzo Menichetti \email{ilmenichetti@gmail.com}
}
