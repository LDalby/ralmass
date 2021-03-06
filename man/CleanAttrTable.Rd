% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CleanAttrTable.r
\name{CleanAttrTable}
\alias{CleanAttrTable}
\title{Clean attribute tables written from ArcGIS.}
\usage{
CleanAttrTable(AttrTable, Soiltype = TRUE)
}
\arguments{
\item{AttrTable}{data.table An attribute table containing at least the tree columns: COUNT, LINK & VALUE}

\item{Soiltype}{Logical. Should the output contain dummy variable for soiltype?}
}
\value{
A clean version of the attribute table with the correct dimension for use in ALMaSS.
}
\description{
The function will remove commas used as thousand seperator, re-arrange columns and add missing ones. 
The attribute table should contain (as a minimum):
\itemize{
  \item COUNT. The number of cells with the same VALUE
  \item LINK. This is the actual element type codes used in ALMaSS
  \item VALUE. The polygon ID
}
}
\examples{
data(polyref)
str(polyref)
tail(polyref)  # Note the issues with commas
poly2 = CleanAttrTable(polyref)
str(poly2)  # Only the three needed columns and in the right order
tail(poly2)  # Comma issue gone
}
