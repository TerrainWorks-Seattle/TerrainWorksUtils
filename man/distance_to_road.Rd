% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surface_metrics.R
\name{distance_to_road}
\alias{distance_to_road}
\title{Distance to the nearest road in meters}
\usage{
distance_to_road(
  input_file = "nofile",
  raster = "nofile",
  dem = "none",
  road_shapefile = "nofile",
  radius = 0,
  scratch_dir = "none"
)
}
\arguments{
\item{input_file}{Character string: a "partial" input file (optional).
If no input file is specified and no other arguments are present,
a Windows Explorer window opens for file selection.}

\item{raster}{Character string: file name (full path) for an existing
contributing-area raster to read from disk (optional).}

\item{dem}{Character string: The file name (full path) of the dem
(elevation raster) for construction of an input file.}

\item{road_shapefile}{Character string: The file name (full path) to a
polyline shapefile for roads.}

\item{radius}{double: Distance in meters to extend the search for a road.}

\item{scratch_dir}{Character string: A scratch directory where temporary
files are written. If an input file for program partial is created,
it is written here.}
}
\value{
A \code{SpatRaster} of distance to the closest road for each DEM grid point.
}
\description{
Provide a \code{SpatRaster} giving the distance to the nearest road for each
cell of a DEM.
}
\details{
distance_to_road() operates in one of three modes, depending on the
input arguments:
\enumerate{
  \item As a wrapper for the Fortran "distanceToRoad" executable,
  with an existing "distanceToRoad" input_file.
  \item As a wrapper for program distanceToRoad, but with the input file
  constructed by distance_to_road.
  \item To read existing raster files from disk.
}
In modes 1 and 2, distance_to_road calls program distanceToRoad, which creates
the requested rasters and writes them to disk as floating point binary files.
These are then read and returned by distance_to_road as a \code{SpatRaster}
object. In mode 3, existing raster files are read directly from disk and
returned as a \code{SpatRaster} object.
}
