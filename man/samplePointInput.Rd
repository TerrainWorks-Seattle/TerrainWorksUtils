% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_file_utils.R
\name{samplePointInput}
\alias{samplePointInput}
\title{Create an input file for Fortan program SamplePoints}
\usage{
samplePointInput(
  inRaster,
  areaPerSample,
  buffer_in,
  buffer_out,
  margin,
  ratio,
  nbins,
  R4rasters,
  I4rasters,
  minPatch,
  inPoints,
  outPoints,
  outMask,
  outInit,
  table,
  scratchDir
)
}
\arguments{
\item{inRaster}{Character: input initiation zone raster (.flt) generated by LS_poly.}

\item{areaPerSample}{Numeric (dbl): area in square meters for each sample point. This
determines the target number of points inside initiation zones.}

\item{buffer_in}{Numeric (dbl): distance in meters to buffer around each point inside initiation zones.}

\item{buffer_out}{Numeric (dbl): distance in meters to buffer around each point outside initiation zones.}

\item{margin}{Numeric (dbl): distance in meters to preclude point placement from the
edge of the initiation zones.}

\item{ratio}{Numeric (dbl): ratio of the number of points outside the initiation zones
to the number inside, i.e., ratio = #outside/#inside.}

\item{nbins}{Numeric (int): number of bins for each predictor.}

\item{R4rasters}{(list): a list of real single-precision rasters to use as predictors. 
Each element of the list includes a name to assign the raster, the raster-file, 
and lower and upper cutoff values.}

\item{I4rasters}{(list): a list of integer rasters to use as predictors. Each integer
represents a nominal class, e.g., landform type.}

\item{minPatch}{Numeric (dbl): minimum size in square meters for patches delineated in the raster mask.}

\item{inPoints}{Character: output shapefile name (full path) for the inside points.}

\item{outPoints}{Character: output shapefile name (full path) for the outside points.}

\item{outMask}{Character: output raster (.flt) name (full path) for the mask.}

\item{outInit}{Character: output raster (.flt) name (full path) for the initiation zones sampled.}

\item{table}{Character: output table name (full path) for the binning results.}

\item{scratchDir}{Character: scratch directory.}
}
\value{
returnCode, a value of zero indicates success
}
\description{
SamplePoints is written to generate point samples for zones inside and
outside of mapped landslide initiation zones. It is written specifically
for working with output files from LS_poly, which I've been developing for 
analysis of the Tongass landslide inventory on Wrangell Island. 
The primary output from LS_poly used here is a raster of landslide 
initiation zones. These are used with a series of specified input rasters
to define the range of predictor values found within the initiation patches.
A raster mask is then defined to delineate areas within that range. 
Random point samples are then generated inside the initiation zones and 
outside the zones within the area of the mask. A buffer is placed around
each point to preclude point placements too close to each other. 
Bins are generated for increments of each predictor for the entire 
initiation zone and mask area and for the inside and outside samples. 
These are used to compare the frequency distribution of values both 
between inside and outside the initiation zones and between the total area
and the sample points.
}
