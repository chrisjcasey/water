% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataCreate.R
\name{BlackspotDrowningTable}
\alias{BlackspotDrowningTable}
\title{Tabulate Drownings Inside BLACKSPOT Polygons}
\usage{
BlackspotDrowningTable(tableName = "BlackSpotDrownings", writeToDB = TRUE)
}
\arguments{
\item{tableName}{Optional. Name of the database table to write the results to (default: \code{"BlackSpotDrownings"}).}

\item{writeToDB}{Logical. If TRUE, the function writes the output to the SQL database (default: \code{TRUE}).}
}
\value{
A data.frame of drowning events that fall within BLACKSPOT polygons. Includes DrowningID,
Latitude, Longitude, GeometryID, polygon Name, and Label.
}
\description{
This function identifies which drowning events fall inside polygons labeled as "BLACKSPOT"
from the Geometry and GeometryPoints tables in your database.
}
