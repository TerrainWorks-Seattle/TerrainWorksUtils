% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_file_utils.R
\name{align_input}
\alias{align_input}
\title{Create an input file for Fortran program align.}
\usage{
align_input(
  refDTM,
  alignDTM,
  refDSM = "nofile",
  alignDSM = "nofile",
  iterations,
  k,
  dampener,
  outDTM,
  tileNx,
  tileNy,
  overlap,
  radius,
  nslope,
  maxSlope,
  nAzimuth,
  outbins,
  outDif,
  outOutlier,
  scratch_dir
)
}
\arguments{
\item{refDTM}{Character: The file name (full path) for the input 
reference DTM.}

\item{alignDTM}{Character: The file name (full path) for the input}

\item{refDSM}{Character: The file name (full path) for the input
reference surface height raster (optional).}

\item{alignDSM}{Character: The file name (full path) for the input
DSM raster to align (optional)}

\item{iterations}{Numeric (int): Number of iterations to solve for the shift}

\item{k}{Numeric (dbl): Number of inter-quartile ranges to use with a
Tukey's fence to identify outliers}

\item{dampener}{Numeric (dbl): Dampener for the shift}

\item{outDTM}{Character: File name (full path) for the output aligned DTM}

\item{tileNx}{(int): number of tiles in the x direction}

\item{overlap}{(dbl): overlap between tiles}

\item{radius}{(dbl): radius in meters for measuring slope and aspect}

\item{nslope}{(int): number of gradient bins}

\item{maxSlope}{(dbl): maximum gradient for binning}

\item{nAzimuth}{(int): number of azimuth bins}

\item{outbins}{Character: output csv file of elevation differences
binned by slope and aspect}

\item{outDif}{Character: output elevation difference raster (.flt)}

\item{outOutlier}{Character: output outlier raster (.flt)}

\item{scratch_dir}{Charcter: scratch directory}

\item{tilesNy}{(int): number of tiles in the y direction}
}
\value{
There is no explicit return object, but an explicit side effect
  is writing to disk of the partial input file.
}
\description{
Program align co-registers two overlapping DTMs. It characterizes the 
frequency distribution of elevation differences as functions of 
slope gradient and aspect, uses that to determine slope- and aspect-dependent
thresholds for filtering elevation differences in setting up a set of linear
equations to solve for the optimal x-y-z shift of one DTM, solves for the
x-y-z shift, generates an aligned DTM, and outputs an elevation difference
raster. Inputs to program align are read from an ASCII input file using 
"Keyword: arguments" format, built here.
}
