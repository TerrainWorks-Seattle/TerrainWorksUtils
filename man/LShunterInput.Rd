% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_file_utils.R
\name{LShunterInput}
\alias{LShunterInput}
\title{Create an input file for Fortran program LShunter}
\usage{
LShunterInput(
  DoD,
  Outlier,
  threshold1,
  threshold2,
  Gradient,
  min1,
  min2,
  Accum,
  maxAccum1,
  maxAccum2,
  MinSize,
  OutPatch,
  ScratchDir
)
}
\arguments{
\item{DoD}{Character: The reference DoD (DEM of Difference) raster (.flt) created by Align.}

\item{Outlier}{Character: The outlier raster (.flt) created by Align.}

\item{threshold1}{Numeric (dbl): Maximum k value for a patch (e.g., -5.0), 1st round.}

\item{threshold2}{Numeric (dbl): Maximum k value for a patch (e.g., -1.5), 2nd round.}

\item{Gradient}{Character: An input gradient raster (.flt) created by program MakeGrids.}

\item{min1}{Numeric (dbl): Minimum gradient for a patch, 1st round.}

\item{min2}{Numeric (dbl): Minimum gradient for a patch, 2nd round.}

\item{Accum}{Character: Input flow accumulation raster (.flt) created by bldgrds.}

\item{maxAccum1}{Numeric (dbl): Maximum flow accumulation for a patch, 1st round.}

\item{maxAccum2}{Numeric (dbl): Maximum flow accumulation for a patch, 2nd round.}

\item{MinSize}{Numeric (dbl): Minimum size of a patch in square meters.}

\item{OutPatch}{Character: Output patch raster (.flt)}

\item{ScratchDir}{Character: Scratch directory.}
}
\value{
returnCode, a value of zero indicates success
}
\description{
Program LShunter identifies potential landslide sites using 
"outlier patches" on a DoD (DEM of Difference) delineated on
an "outlier" raster created by Program Align.
}
