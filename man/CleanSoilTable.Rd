% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CleanSoilTable.r
\name{CleanSoilTable}
\alias{CleanSoilTable}
\title{Clean soil type tables written from arcpy.}
\usage{
CleanSoilTable(SoilTable)
}
\arguments{
\item{SoilTable}{data.table An table with soil types exported from arcpy}
}
\value{
A clean version of the soil type table with the correct dimension
for use in ALMaSS.
}
\description{
The function will remove commas used as thousand seperator, re-arrange columns and add missing ones.
}
