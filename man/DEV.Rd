% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surface_metrics.R
\name{DEV}
\alias{DEV}
\title{Deviation from mean elevation}
\usage{
DEV(
  input_file = "nofile",
  raster = "nofile",
  dem = "none",
  radius = 0,
  scratch_dir = "none"
)
}
\arguments{
\item{input_file}{Character string: a "LocalRelief" input file (optional).
If no input file is specified and no other arguments are present,
a Windows Explorer window opens for file selection.}

\item{raster}{Character string: file name (full path) for an existing
DEV raster to read from disk (optional).}

\item{dem}{Character string: The file name (full path) of the dem
(elevation raster) for construction of an input file.}

\item{radius}{double: Radius in meters to calculate DEV.}

\item{scratch_dir}{Character string: A scratch directory where temporary
files are written. If an input file for program partial is created,
it is written here.}
}
\value{
A \code{SpatRaster} of DEV values.
}
\description{
Provide a \code{SpatRaster} giving the deviation from mean elevation (DEV)
over a specified radius for each point of a DEM. DEV is defined for the
ith DEM grid point as
DEVi = (ei - mean(e))/sd(e), where ei is the elevation of the ith point,
mean(e) is the mean elevation of all DEM points within the specified radius,
and sd(e) is the standard deviation of those elevations.
}
\details{
DEV() operates in one of three modes, depending on the
input arguments:
\enumerate{
  \item As a wrapper for the Fortran "LocalRelief" executable,
  with an existing "LocalRelief" input_file.
  \item As a wrapper for program LocalRelief, but with the input file
  constructed by DEV.
  \item To read existing raster files from disk.
}
In modes 1 and 2, DEV calls program LocalRelief, which creates
the requested rasters and writes them to disk as floating point binary files.
These are then read and returned by DEV as a \code{SpatRaster}
object. In mode 3, existing raster files are read directly from disk and
returned as a \code{SpatRaster} object.
}
