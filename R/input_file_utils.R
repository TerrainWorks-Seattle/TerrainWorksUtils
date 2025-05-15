#' Load an ASCII input file
#'
#' TerrainWorks scripts read instructions and parameter values
#' from an ASCII file. These files are formatted using a "keyword: argument"
#' format. The keywords and associated arguments can come in any order
#' and each keyword may have any number of arguments, including no argument.
#' Arguments use a "parameter = value" format, with each argument separated
#' by a comma. For example, here is the keyword: argument input line for
#' specifying reach length in a synthetic channel network:
#' REACH LENGTH: FIXED=100., BREAK AT JUNCTIONS.
#' Here, "REACH LENGTH" is the keyword, there are two arguments, one of which
#' requires a value.
#'
#' @param infile File name with full path.
#'   If no file is specified, a window opens to choose a file interactively.
#'
#' @return A tibble with each row corresponding to one line in the input file.
#'
#' @examples get_input_file("c:\\data\\Umpqua\\input_bldgrds.txt")
#'   opens the specified file.
#'
#' @examples get_input_file() opens a Windows Explorer window for interactive
#'   file selection.
#'
#' @seealso
#' * [get_keyword()] extracts a keyword from an input file line.
#' * [get_args()] extracts the arguments from an input line.
#' * [parse_arg()] extracts the parameter and value from a single argument.
#'
#' @import tibble
#' @importFrom stringr str_detect

#' @export
#'
get_input_file <- function(infile = "nofile") {
  if (str_detect(infile, "nofile")) {
    input_file <- tibble(readLines(file.choose()))
  } else {
    if (!file.exists(infile)) {
      stop("Input file not found")
    } else {
      input_file <- tibble(readLines(infile))
    }
  }
  return(input_file)
}
#------------------------------------------------------
#' Get the keyword from an input-file line.
#'
#' @param infile, a tibble created by get_input_file.
#' @param line_num, line number in the input file.
#'
#' @return A character vector containing the keyword;
#'   NA if no keyword is present.
#'
#' @examples
#' * infile <- get_input_file() select input file interactively.
#' * keyword <- get_keyword(infile,3) gets the keyword from
#'   line 3 of the selected file.
#'
#' @export
#'
get_keyword <- function(infile, line_num) {
  loc_comment <- str_locate(infile[line_num,], "#")
  loc <- str_locate(infile[line_num,], ":")
  if (is.na(loc_comment[1,1]) == TRUE) {
    keyword <- str_sub(infile[line_num,], start = 1, end = loc[1,1] - 1)
  } else {
    keyword <- NA
  }

  return(keyword)
}
#-----------------------------------------------------
#' Get a character vector of arguments from an input-file line.
#'
#' @param infile; a tibble created by get_input_file.
#' @param line_num; the line of the input file.
#'
#' @return arguments; a character vector of arguments
#'
#' @export
#'
get_args <- function(infile, line_num) {
  loc <- str_locate(infile[line_num,], ":")
  arguments <- str_sub(infile[line_num,] ,start = loc[1,1] + 1, end = str_length(infile[line_num,]))
  return(arguments)
}
#-------------------------------------------------
#' Parse "parameter = value" pairs from a specified element
#'   in a list of arguments.
#'
#' @param arguments; a character vector of arguments output by get_args.
#' @param i; the element in the argument vector to parse.
#'
#' @return A 2-element character vector. The first element gives the
#'   parameter name; the second element gives the parameter value.
#'   If no equal sign is present in the argument, the parameter element
#'   is empty and the argument value is contained in the value element.
#'
#' @export
#'
parse_arg <- function(arguments, i = 1) {
  arg_list <- str_split(arguments, ",")
  this_arg <- arg_list[[1]][[i]]
  out_arg <- vector("list", 2)
  names(out_arg) <- c("Parameter", "Value")
  if (str_detect(this_arg, "=")) {
    param_value <- str_split(this_arg, "=")
    out_arg[["Parameter"]] <- param_value[[1]][[1]]
    out_arg[["Value"]] <- param_value[[1]][[2]]
  } else {
    out_arg[["Parameter"]] <- ""
    out_arg[["Value"]] <- this_arg
  }
  return(out_arg)
}
#-----------------------------------------------
#' Get a list of DEM raster files listed in an ASCII input file.
#'
#' @param infile, a tibble created by get_input_file.
#'
#' @return A list of character strings, each string gives a DEM file name
#'
#' @export
#'
get_dem <- function(infile) {
  # count instances of DEM keyword in input
  if (nrow(infile) == 0) stop("Input file is empty")

  n <- 0
  for (i in 1:nrow(infile)) {
    keyword <- get_keyword(infile, i)
    if (is.na(keyword)) {
      next
    }
    if (str_detect(keyword, "DEM") == TRUE) {
      n <- n + 1
    }
  }

  dem <- vector("list", n)
  n <- 0
  for (i in 1:nrow(infile)) {
    keyword <- get_keyword(infile, i)
    if (is.na(keyword)) {
      next
    }
    if (str_detect(keyword, "DEM") == TRUE) {
      argument <- get_args(infile, i)
      param_value <- parse_arg(argument)
      n <- n + 1
      dem[[n]] <- param_value[[2]]
    }
  }
  return(dem)
}
#--------------------------------------------------
convert_to_flt <- function(in_raster) {
  if (!grepl("\\.flt$", in_raster)) {
    # Strip file extension, if it has one
    in_raster_base <- gsub("\\.\\w+$", "", in_raster)
  }

  if (!file.exists(paste0(in_raster, ".flt"))) {
    # Create raster object from in_raster
    out_raster <- terra::rast(in_raster)

    # Create .flt filename
    out_name <- paste0(in_raster_base, ".flt")

    # Save .flt file
    terra::writeRaster(out_raster, out_name)

    # Convert GDAL BIL flt header file to binary floating point header.
    convert_hdr(paste0(out_name, ".hdr"))
  }
}
#--------------------------------------------------
#' Create an input file for Fortran program makegrids
#'
#' MakeGrids is called by functions in surface_metrics.
#' MakeGrids reads an ASCII input file with a "Keyword: arguments" format.
#'
#' @param dem: File name (full path) to input DEM.
#' @param length_scale: Diameter in meters over which to measure attributes.
#' @param scratch_dir: Directory for storing temporary files. The input
#'   file is written to the scratch_dir.
#' @param rasters: A character vector. Each element contains two strings
#'   separated by a comma. The first specifies the type of attribute,
#'   the second specifies the file name (full path) for the output floating
#'   point binary (.flt) raster.
#' @param overwrite: If TRUE, allows overwriting of an existing input file.
#'
#' @return There is no return object, but an explicit side effect is
#'   writing to disk of the makegrids input file.
#' @export
#'
makegrids_input <- function(dem,
                            length_scale,
                            scratch_dir,
                            rasters,
                            overwrite = TRUE) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)

  out_file <- paste0(scratch_dir, "\\makegrids_input.txt")
  if (file.exists(out_file)) {
    if (overwrite) {
      message("overwriting ", out_file)
    } else {
      stop(out_file, " exists. Set overwrite = TRUE to overwrite.")
    }
  }

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for makeGrids\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
  write_input("LENGTH SCALE: ", length_scale)

  for (i in 1:length(rasters)) {
    loc <- str_locate(rasters[[i]], ",")
    grid_type <- str_sub(rasters[[i]], 1, loc[1,1] - 1)
    grid_file <- str_sub(rasters[[i]], loc[1,1] + 1, -1)
    if (str_detect(grid_file, "OUTPUT FILE") == FALSE) {
      grid_file <- paste0("OUTPUT FILE = ", grid_file)
    }
    if (str_detect(grid_file, ".flt")) {
      grid_file <- paste0(grid_file, ".flt")
    }
    write_input(paste0("GRID: ", grid_type, ", ", grid_file))
  }
}
#---------------------------------------------------------
#' Create an input file for Fortran program Partial.
#'
#' Program Partial builds a raster giving the contributing area to each
#' DEM cell for a storm of specified duration. Inputs to Partial are read
#' from an ASCII input file using "Keyword: arguments" format.
#'
#' @param dem Character string: The file name (full path) for the input DEM.
#' @param k Numeric (dbl): Saturated hydraulic conductivity, in meters per hour.
#' @param d Numeric (dbl): Storm duration in hours.
#' @param length_scale Numeric (dbl): Diameter for smoothing the DEM.
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#' @param out_raster Character string: File name (full path) for the output
#'   binary floating point (.flt) raster.
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the partial input file.
#' @export
#'
accum_input <- function(dem,
                        k,
                        d,
                        length_scale,
                        scratch_dir,
                        out_raster) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)
  suppressWarnings(out_raster <- normalizePath(out_raster))

  out_file <- paste0(scratch_dir, "\\partial_input.txt")

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for partial\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("DURATION: ", d)
  write_input("CONDUCTIVITY: ", k)
  write_input("OUTPUT RASTER: ", out_raster)
}

#---------------------------------------------------------
#' Create an input file for Fortran program align.
#'
#' Program align co-registers two overlapping DTMs. It characterizes the 
#' frequency distribution of elevation differences as functions of 
#' slope gradient and aspect, uses that to determine slope- and aspect-dependent
#' thresholds for filtering elevation differences in setting up a set of linear
#' equations to solve for the optimal x-y-z shift of one DTM, solves for the
#' x-y-z shift, generates an aligned DTM, and outputs an elevation difference
#' raster. Inputs to program align are read from an ASCII input file using 
#' "Keyword: arguments" format, built here.
#'
#' @param refDTM Character: The file name (full path) for the input 
#' reference DTM.
#' @param alignDTM Character: The file name (full path) for the input
#' @param refDSM Character: The file name (full path) for the input
#' reference surface height raster (optional).
#' @param alignDSM Character: The file name (full path) for the input
#' DSM raster to align (optional) 
#' @param iterations Numeric (int): Number of iterations to solve for the shift
#' @param k Numeric (dbl): Number of inter-quartile ranges to use with a
#' Tukey's fence to identify outliers
#' @param dampener Numeric (dbl): Dampener for the shift
#' @param outDTM Character: File name (full path) for the output aligned DTM
#' @param tileNx (int): number of tiles in the x direction
#' @param tilesNy (int): number of tiles in the y direction
#' @param overlap (dbl): overlap between tiles
#' @param radius (dbl): radius in meters for measuring slope and aspect
#' @param nslope (int): number of gradient bins
#' @param maxSlope (dbl): maximum gradient for binning
#' @param nAzimuth (int): number of azimuth bins
#' @param outbins Character: output csv file of elevation differences
#' binned by slope and aspect
#' @param outDif Character: output elevation difference raster (.flt)
#' @param outOutlier Character: output outlier raster (.flt)
#' @param scratch_dir Charcter: scratch directory
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the partial input file.
#' @export
#'
align_input <- function(refDTM,
                        alignDTM,
                        refDSM = 'nofile',
                        alignDSM = 'nofile',
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
                        scratch_dir) {
  
  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }
  
  # Normalize paths
  refDTM <- normalizePath(refDTM)
  alignDTM <- normalizePath(alignDTM)
  if (!refDSM == 'nofile') {
    suppressWarnings(refDSM <- normalizePath(refDSM))
  }
  if (!alignDSM == 'nofile') {
    suppressWarnings(alignDSM <- normalizePath(alignDSM))
  }
  suppressWarnings(outDTM <- normalizePath(outDTM))
  suppressWarnings(outDif <- normalizePath(outDif))
  suppressWarnings(outbins <- normalizePath(outbins))
  suppressWarnings(outOutlier <- normalizePath(outOutlier))
  scratch_dir <- normalizePath(scratch_dir)
  
  # Do not include ".flt" in raster file names
  if (str_detect(refDTM, ".flt$") == TRUE) {
    n <- str_length(refDTM)
    refDTM <- str_sub(refDTM, 1, n[[1]]-4)
  }
  if (str_detect(alignDTM, ".flt$") == TRUE) {
    n <- str_length(alignDTM)
    alignDTM <- str_sub(alignDTM, 1, n[[1]]-4)
  }

  out_file <- paste0(scratch_dir, "\\input_align.txt")
    
  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }
  
  write_input("# Input file for align\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )
  
  write_input("REFERENCE DEM: ", refDTM)
  write_input("DEM TO ALIGN: ", alignDTM)
  if (!refDSM == 'nofile') {
    write_input("REFERENCE DSM: ", refDSM)
  }
  if (!alignDSM == 'nofile') {
    write_input("DSM TO ALIGN: ", alignDSM)
  }
  write_input("RADIUS: ", radius)
  write_input("ITERATIONS: ", iterations)
  write_input("K: ", k)
  write_input("DAMPENER: ", dampener)
  write_input("OUTPUT DEM: ", outDTM)
  write_input("TILES: X = ", tileNx, ", Y = ", tileNy, ", OVERLAP = ", overlap)
  write_input("BINS: SLOPE BINS=",nslope,",MAX SLOPE=",maxSlope,",AZIMUTH BINS=",nAzimuth,",OUTPUT=",outbins)
  write_input("OUTPUT DIFFERENCE RASTER: ", outDif)
  write_input("OUTPUT OUTLIER RASTER: ", outOutlier)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
}

#----------------------------------------------------------
#' Create an input file for Fortran program HuntLS
#' 
#' Program HuntLS reads an "outlier" raster created by program Align.
#' The "outlier" k values indicate the number of interquartile ranges
#' (q3-q1) a DoD de (delta elevation) value falls outside of the 
#' interquartile range: k = (de-q1)/(q3-a1) if k is less than q1 
#' and k = (de-q3)/(q3-q1) if k is greater than q3. Values of de
#' between q1 and q3 are given k values of zero. HuntLS associates 
#' k patches with field-mapped landslide locations surveyed for the
#' Post Mortem (Stewart et al., 2013) study. 
#' 
#' @param DEM Character: The reference DTM used for program Align.
#' @param Outlier Character: The input outlier raster (.flt) created by Align.
#' @param DoD Character: Input DoD raster (.flt) created by Align.
#' @param Accum Character: Input flow accumulation raster created by bldgrds.
#' @param AccumThreshold Numeric (dbl): Contributing area above which landslide
#' patches are precluded.
#' @param LSpnts Character: Input point shapefile of mapped landslide points.
#' @param IDfield Character: LSpnts attributes-table field name for record ID.
#' @param Radius Numeric (dbl): Search radius in meters for matching landslide
#' points to k-value patches.
#' @param AspectLength Numeric (dbl): Length in meters to calculate aspect.
#' @param GradLength Numeric (dbl): Length in meters to calculate gradient.
#' @param OutlierThreshold Numeric (dbl): Minimum absolute k value for a patch.
#' @param ScratchDir Character: Scratch directory.
#' @param OutPatch Character: Output patch raster (.flt).
#' @param OutGrad Character: Output gradient raster (.flt).
#' @param Outcsv Character: Output comma-delimited table.
#' 
#' @return returnCode, a value of zero indicates success
#' @export
#' 
 huntLSinput <- function(DEM,
                         Outlier,
                         DoD,
                         Accum,
                         AccumThreshold,
                         LSpnts,
                         IDfield,
                         Radius,
                         AspectLength,
                         GradLength,
                         OutlierThreshold,
                         ScratchDir,
                         OutPatch,
                         OutGrad,
                         Outcsv) {

  returnCode = -1
  
  if (!dir.exists(ScratchDir)) {
    stop("invalid scratch folder: ", ScratchDir)
  }

# Normalize paths
  suppressWarnings(DEM <- normalizePath(DEM))
  suppressWarnings(DoD <- normalizePath(DoD))
  suppressWarnings(Accum <- normalizePath(Accum))
  suppressWarnings(LSpnts <- normalizePath(LSpnts))
  suppressWarnings(OutPatch <- normalizePath(OutPatch))
  suppressWarnings(OutGrad <- normalizePath(OutGrad))
  suppressWarnings(Outcsv <- normalizePath(Outcsv))
  suppressWarnings(ScratchDir <- normalizePath(ScratchDir))
  
  # Do not include ".flt" in raster file names
  if (str_detect(DEM, ".flt$") == TRUE) {
    n <- str_length(DEM)
    DEM <- str_sub(DEM, 1, n[[1]]-4)
  }
  
  if (str_detect(DoD, ".flt$") == TRUE) {
    n <- str_length(DoD)
    DoD <- str_sub(DoD, 1, n[[1]]-4)
  }
  
  if (str_detect(Outlier, ".flt$") == TRUE) {
    n <- str_length(Outlier)
    Outlier <- str_sub(Outlier, 1, n[[1]]-4)
  }
  
  if (str_detect(Accum, ".flt$") == TRUE) {
    n <- str_length(Accum)
    Accum <- str_sub(Accum, 1, n[[1]]-4)
  }
  
  out_file <- paste0(ScratchDir, "\\input_huntls.txt")
  
  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }
  
  write_input("# Input file for HuntLS\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )
  
  write_input("DEM: ", DEM)
  write_input("INPUT OUTLIER RASTER: ", Outlier)
  write_input("INPUT ELEVATION DIFFERENCE RASTER: ", DoD)
  write_input("INPUT FLOW ACCUMULATION RASTER: ", Accum)
  write_input("ACCUMULATION THRESHOLD: ", AccumThreshold)
  write_input(paste0("INPUT LANDSLIDE POINT SHAPEFILE: ", LSpnts, ", ID FIELD = ", IDfield))
  write_input("SEARCH RADIUS: ", Radius)
  write_input("ASPECT LENGTH SCALE: ", AspectLength)
  write_input("GRADIENT LENGTH SCALE: ", GradLength)
  write_input("OUTLIER THRESHOLD: ", OutlierThreshold)
  write_input("SCRATCH DIRECTORY: ", ScratchDir)
  write_input("OUTPUT PATCH RASTER: ", OutPatch)
  write_input("OUTPUT GRADIENT RASTER: ", OutGrad)
  write_input("OUTPUT TABLE: ", Outcsv)
  
  returnCode = 0
  return(returnCode)
}
#----------------------------------------------------------
#' Create an input file for Fortran program LShunter
#' 
#' Program LShunter identifies potential landslide sites using 
#' "outlier patches" on a DoD (DEM of Difference) delineated on
#' an "outlier" raster created by Program Align. 
#' 
#' @param DoD Character: The reference DoD (DEM of Difference) raster (.flt) created by Align.
#' @param Outlier Character: The outlier raster (.flt) created by Align.
#' @param threshold1 Numeric (dbl): Maximum k value for a patch (e.g., -5.0), 1st round.
#' @param threshold2 Numeric (dbl): Maximum k value for a patch (e.g., -1.5), 2nd round.
#' @param Gradient Character: An input gradient raster (.flt) created by program MakeGrids.
#' @param min1 Numeric (dbl): Minimum gradient for a patch, 1st round.
#' @param min2 Numeric (dbl): Minimum gradient for a patch, 2nd round.
#' @param Accum Character: Input flow accumulation raster (.flt) created by bldgrds.
#' @param maxAccum1 Numeric (dbl): Maximum flow accumulation for a patch, 1st round.
#' @param maxAccum2 Numeric (dbl): Maximum flow accumulation for a patch, 2nd round.
#' @param MinSize Numeric (dbl): Minimum size of a patch in square meters.
#' @param OutPatch Character: Output patch raster (.flt)
#' @param ScratchDir Character: Scratch directory.
#' 
#' @return returnCode, a value of zero indicates success
#' @export
#' 
LShunterInput <- function(
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
  ScratchDir) {
 
 returnCode = -1
 
 if (!dir.exists(ScratchDir)) {
   stop("invalid scratch folder: ", ScratchDir)
 }
 
 # Normalize paths
 suppressWarnings(DoD <- normalizePath(DoD))
 suppressWarnings(Outlier <- normalizePath(Outlier))
 suppressWarnings(Gradient <- normalizePath(Gradient))
 suppressWarnings(Accum <- normalizePath(Accum)) 
 suppressWarnings(OutPatch <- normalizePath(OutPatch))
 suppressWarnings(ScratchDir <- normalizePath(ScratchDir))
 
 # Do not include ".flt" in raster file names
 if (str_detect(DoD, ".flt$") == TRUE) {
   n <- str_length(DoD)
   DoD <- str_sub(DoD, 1, n[[1]]-4)
 }
 
 if (str_detect(Outlier, ".flt$") == TRUE) {
   n <- str_length(Outlier)
   Outlier <- str_sub(Outlier, 1, n[[1]]-4)
 }
 
 if (str_detect(Gradient, ".flt$") == TRUE) {
   n <- str_length(Gradient)
   Gradient <- str_sub(Gradient, 1, n[[1]]-4)
 }
 
 if (str_detect(Accum, ".flt$") == TRUE) {
   n <- str_length(Accum)
   Accum <- str_sub(Accum, 1, n[[1]]-4)
 }
 
 out_file <- paste0(ScratchDir, "\\input_LShunter.txt")
 
 write_input <- function(...,
                         append = TRUE) {
   cat(..., "\n",
       file = out_file,
       sep = "",
       append = append
   )
 }
 
 write_input("# Input file for LShunter\n",
             "# Creating by input_file_utils.R\n",
             "# On ", as.character(Sys.time()),
             append = FALSE
 )
 
 write_input("DoD: ", DoD)
 write_input("OUTLIER: ", Outlier, ", THRESHOLD1 = ", threshold1, ", THRESHOLD2 = ", threshold2)
 write_input("GRADIENT: ", Gradient, ", MIN1 = ", min1, ", MIN2 = ", min2)
 write_input("FLOW ACCUMULATION: ", Accum, ", MAX1 = ", maxAccum1, ", MAX2 = ", maxAccum2)
 write_input("MINIMUM SIZE: ", MinSize)
 write_input("OUTPUT PATCH RASTER: ", OutPatch)
 write_input("SCRATCH DIRECTORY: ", ScratchDir)
 
 returnCode = 0
 return(returnCode)
}

#-------------------------------------------------------------------------
 #' Create an input file for Fortran program Quantiles.
#' 
#' Program Quantiles reads a raster file and computes quantiles
#' over a moving circular window. For each iteration of the window,
#' the interquartile range is determined and outliers, based
#' on a Tukey's fence with k = 1.5, are removed. Quartiles are then
#' recalculated using the remaining values and the minimum and 
#' maximum range 
#' 
#' @param in_raster Character: The file name (full path) for the input raster.
#' @param radius Numeric (dbl): radius in meters for the moving window
#' @param buffer Numeric (dbl): spacign between moving window center points,
#' in raster cells.
#' @param out_outlier Character: output file name (full path) for the 
#' outlier raster (optional)
#' @param out_q1 Character: output file name (full path) for the
#' first-quartile raster (optional)
#' @param out_q2 Character: output file name (full path) for the
#' median raster (optional)
#' @param out_q3 Character: output file name (full path) for the
#' third-quartile raster (optional)
#' @param out_mean Character: output file name (full path) for the mean raster
#' @param out_zscore Character: output file name (full path) for the 
#' z-score raster
#' @param out_prob Character: output file name (full path) for the
#' probability raster (optional)
#' @param scratch_dir Charcter: scratch directory
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the partial input file.
#' @export
#' 
quantiles_input <- function(in_raster = "nofile",
                            radius = 0.,
                            buffer = 0,
                            out_outlier = "nofile",
                            out_q1 = "nofile",
                            out_q2 = "nofile",
                            out_q3 = "nofile",
                            out_mean = "nofile",
                            out_zscore = "nofile",
                            out_prob = "nofile",
                            scratch_dir = "none") {
  
  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }
  
  # Normalize paths
  in_raster <- normalizePath(in_raster)
  
  if (!out_outlier == "nofile") {
    suppressWarnings(out_outlier <- normalizePath(out_outlier))
  }
  
  if (!out_q1 == "nofile") {
    suppressWarnings(out_q1 <- normalizePath(out_q1))
  }
  
  if (!out_q2 == "nofile") {
    suppressWarnings(out_q2 <- normalizePath(out_q2))
  }
  
  if (!out_q3 == "nofile") {
    suppressWarnings(out_q3 <- normalizePath(out_q3))
  }
  
  if (!out_mean == "nofile") {
    suppressWarnings(out_mean <- normalizePath(out_mean))
  }
  
  if (!out_zscore == "nofile") {
    suppressWarnings(out_zscore <- normalizePath(out_zscore))
  }
  
  if (!out_prob == "nofile") {
    suppressWarnings(out_prob <- normalizePath(out_prob))
  }

  scratch_dir <- normalizePath(scratch_dir)
  
  # Do not include ".flt" in raster file names
  if (str_detect(in_raster, ".flt$") == TRUE) {
    n <- str_length(in_raster)
    in_raster <- str_sub(in_raster, 1, n[[1]]-4)
  }
  
  out_file <- paste0(scratch_dir, "\\input_quantiles.txt")
  
  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }
  
  write_input("# Input file for quantiles\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )
  
  write_input("INPUT RASTER: ", in_raster)
  write_input("RADIUS: ", radius)
  write_input("BUFFER: ", buffer)
  if (!out_outlier == "nofile") {
    write_input("OUTPUT OUTLIER RASTER: ", out_outlier)
  }
  if (!out_q1 == "nofile") {
    write_input("OUTPUT Q1 RASTER: ", out_q1)
  }
  if (!out_q2 == "nofile") {
    write_input("OUTPUT Q2 RASTER: ", out_q2)
  }
  if (!out_q3 == "nofile") {
    write_input("OUTPUT Q3 RASTER: ", out_q3)
  }
  if (!out_mean == "nofile") {
    write_input("OUTPUT MEAN RASTER: ", out_mean)
  }
  if (!out_zscore == "nofile") {
    write_input("OUTPUT ZSCORE RASTER: ", out_zscore)
  }
  if (!out_prob == "nofile") {
    write_input("OUTPUT PROBABILITY RASTER: ", out_prob)
  }
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
}

#---------------------------------------------------------
#' Create an input file for Fortran program distanceToRoad.
#'
#' Program distanceToRoad builds a raster giving the distance in meters
#' to the nearest road for each DEM grid point.
#' Inputs to distanceToRoad are read from an ASCII input file
#' using "Keyword: arguments" format.
#'
#' @param dem Character string: The file name (full path) for the input DEM.
#' @param radius Double: radius in meters to extend from a road.
#' @param road_file Character string: input road polyline shapefile.
#' @param out_raster Character string: File name (full path) for the output
#'   binary floating point (.flt) raster.
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the input file.
#' @export
#'
distanceToRoad_input <- function(dem,
                                 radius = 1000.,
                                 road_file,
                                 out_raster,
                                 scratch_dir) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)
  suppressWarnings(out_raster <- normalizePath(out_raster))

  out_file <- paste0(scratch_dir, "\\input_distanceToRoad.txt")

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for distanceToRoad\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("ROAD SHAPEFILE: ", road_file)
  write_input("OUTPUT RASTER: ", out_raster)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
  write_input("RADIUS: ", radius)
}

#---------------------------------------------------------
#' Create an input file for Fortran program bldrds.
#'
#' Program bldgrds does flow routing for a DEM. This function
#' creates an input file specifying that the flow routing will
#' not involve delineation of channels. Therefore, flow directions
#' are entirely based on D-infinity.
#'
#' @param dem Character string: The file name (full path) for the input DEM.
#' @param aspect_length: double, length in meters over which aspect is measured.
#' @param plan_length: double, length in meters over which plan curvature is measured.
#' @param grad_length: double, length in meters over which gradient is measured.
#' @param out_raster Character string: File name (full path) for the output
#'   binary floating point (.flt) raster.
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the input file.
#' @export
#'
bldgrds_nochannels_input <- function(dem,
                                     aspect_length,
                                     plan_length,
                                     grad_length,
                                     out_raster,
                                     scratch_dir) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)
  suppressWarnings(out_raster <- normalizePath(out_raster))

  out_file <- paste0(scratch_dir, "\\input_bldgrds_nochannels.txt")

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for bldgrds_nochannels\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("USE SMOOTHED ASPECT: LENGTH SCALE = ", aspect_length)
  write_input("PLAN CURVATURE LENGTH SCALE: ", plan_length)
  write_input("GRADIENT LENGTH SCALE: ", grad_length)
  write_input("NO CHANNELS:")
  write_input("OUTPUT FLOW ACCUMULATION RASTER: ", out_raster)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
}

#---------------------------------------------------------
#' Function PFA_debris_flow_input;
#' create an input file for Fortran program PFA_debris_flow.
#'
#' PFA_debris_flow is one of a sequence of programs used to assemble data files
#' for the PFA recalibration of the landslide initiation and debris-flow-runout models.
#' The Quatro file PFA_runout describes calibration of a multinomial model for
#' estimating the probability of debris-flow scour or deposition using the ODF
#' 1996 Storm Study surveys. The coefficients for that model are inputs to PFA_debris_flow.
#' Here, the debris-flow runout tracks from the DOGAMI Special Paper 53 study are
#' associated with lidar-DEM flow paths and terrain attributes associated with runout
#' extent are assembled and output in a format for a Cox survival model with
#' time-dependent covariates.
#'
#' @param dem Character string: The file name (full path) for the input DEM.
#' @param init_points: string: File name for initiation-point shapefile.
#' @param geo_poly: string: File name for rock-type polygon shapefile.
#' @param stand_age: string: File name for LEMMA stand-age flt raster.
#' @param tracks: string: File name for DOGAMI debris-flow-track polyline shapefile.
#' @param radius: dbl: Search radius in meters for matching DEM flow path to DOGAMI track.
#' @param length_scale: dbl: Length in meters to measure elevation derivatives.
#' @param bulk_coef: dbl: Bulking coefficient
#' @param alpha: dbl: Proportion of debris-flow cross-sectional volume deposited per unit length.
#' @param uncensored: string: If TRUE, interpret all endpoint tracks as uncensored.
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#' @param out_track: string: Output track point shapefile.
#' @param out_surv: string: Output input file for Cox survival.
#' @param out_point: string: Output initiation point shapefile; includes fields for scour and deposit volume.
#' @param coef: list of dbl values specifying multinomial logistic regression coefficients
#'   for probability of scour, deposition, and transitional flow.
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the input file.
#' @export
#'
PFA_debris_flow_input <- function(dem,
                                  init_points,
                                  geo_poly,
                                  stand_age,
                                  tracks,
                                  radius,
                                  initRadius,
                                  length_scale,
                                  slope_intercept,
                                  slope_coef,
                                  bulk_coef,
                                  init_width,
                                  init_length,
                                  DF_width,
                                  alpha,
                                  uncensored,
                                  scratch_dir,
                                  out_surv,
                                  out_point,
                                  out_kaplanMeier,
                                  coef) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)

  out_file <- paste0(scratch_dir, "\\input_PFA_debris_flow.txt")

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]] - 4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for PFA_debris_flow\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("INITIATION POINT SHAPEFILE: ", init_points)
  write_input("TRACK LINE SHAPEFILE: ", tracks)
  write_input("ROCK TYPE POLYGON SHAPEFILE: ", geo_poly)
  write_input("STAND AGE RASTER: ", stand_age)
  write_input("RADIUS: ", radius)
  write_input("INITIATION POINT RADIUS: ", initRadius)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("SLOPE: INTERCEPT = ", slope_intercept, ", COEFFICIENT = ", slope_coef)
  write_input("BULKING FACTOR: COEFFICIENT = ", bulk_coef)
  write_input("INITIATION DIMENSION: WIDTH = ", init_width, ", LENGTH = ", init_length)
  write_input("TRACK WIDTH: ",DF_width)
  write_input("ALPHA: ", alpha)
  if (str_detect(uncensored,"TRUE")) {
    write_input("UNCENSORED:")
  }
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
  write_input("OUTPUT SURVIVAL FILE: ", out_surv)
  write_input("OUTPUT INITIATION POINT SHAPEFILE: ", out_point)
  write_input("")
  write_input("MULTINOMIAL LOGISTIC REGRESSION COEFFICIENTS LIST:")
  write_input("  SCOUR INTERCEPT: ", coef[[1]])
  write_input("  SCOUR GRADIENT: ", coef[[2]])
  write_input("  SCOUR NORMAL CURVATURE: ", coef[[3]])
  write_input("  SCOUR TANGENT CURVATURE: ", coef[[4]])
  write_input("  SCOUR STAND AGE: ", coef[[5]])
  write_input("  SCOUR ROCK TYPE: SEDIMENTARY = ", coef[[6]],", VOLCANIC = ", coef[[7]],
              ", IGNEOUS-METAMORPHIC = ", coef[[8]], ", VOLCANICLASTIC = ", coef[[9]],
              ", UNCONSOLIDATED = ", coef[[10]])
  write_input("  TRANSITION INTERCEPT: ", coef[[11]])
  write_input("  TRANSITION GRADIENT: ", coef[[12]])
  write_input("  TRANSITION NORMAL CURVATURE: ", coef[[13]])
  write_input("  TRANSITION TANGENT CURVATURE: ", coef[[14]])
  write_input("  TRANSITION STAND AGE: ", coef[[15]])
  write_input("  TRANSITION ROCK TYPE: SEDIMENTARY = ", coef[[16]],", VOLCANIC = ", coef[[17]],
              ", IGNEOUS-METAMORPHIC = ", coef[[18]], ", VOLCANICLASTIC = ", coef[[19]],
              ", UNCONSOLIDATED = ", coef[[20]])
  write_input("END LIST:")

}

#---------------------------------------------------------
#' Create an input file for Fortran program LocalRelief.
#'
#' Program LocalRelief builds a deviation-from-local-elevation (DEV) raster
#' (and other things as well, DEV is all we need here).
#'
#' @param dem Character string: The file name (full path) for the input DEM.
#' @param radius dbl: Radius in meters for calculating DEV.
#' @param out_raster Character string: File name (full path) for the output
#'   binary floating point (.flt) raster.
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#'
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk of the input file.
#' @export
#'
DEV_input <- function(dem,
                      radius,
                      out_raster,
                      scratch_dir) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)
  suppressWarnings(out_raster <- normalizePath(out_raster))

  out_file <- paste0(scratch_dir, "\\input_DEV.txt")

  # Do not include ".flt" in dem file name
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for LocalRelief\n",
              "#   getting DEV only.\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("DEM: ", dem)
  write_input("RADIUS: ", radius)
  write_input("DOWN SAMPLE: 1")
  write_input("SAMPLE INTERVAL: 1")
  write_input("OUTPUT DEV RASTER: ", out_raster)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
}

#---------------------------------------------------------
#' Create an input file for Fortran program resample
#'
#' Resample downsamples a raster using increments of the raster pixel size.
#' It uses no interpolation; output pixel corner locations match input pixel corners,
#' the pixels are just bigger. Resample is primarily used to downsample a DEM,
#' in which case instead of pixel corners, we're dealing with grid points or cells.
#' @param in_raster Character string
#' @param skip integer, number of cells to skip.
#' @param out_raster Character string, resampled input raster
#' @param scratch_dir Character string: Directory for temporary files.
#'   The input file is written to the scratch_dir.
#' @return There is no explicit return object, but an explicit side effect
#'   is writing to disk (in the scratch_dir) of the input file.
#' @export
#'
resample_input <- function(in_raster, skip, out_raster, scratch_dir) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }

  # Normalize paths
  dem <- normalizePath(in_raster)
  scratch_dir <- normalizePath(scratch_dir)
  suppressWarnings(out_raster <- normalizePath(out_raster))

  out_file <- paste0(scratch_dir, "\\input_resample.txt")
  if (str_detect(dem, ".flt$") == TRUE) {
    n <- str_length(dem)
    dem <- str_sub(dem, 1, n[[1]]-4)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
        file = out_file,
        sep = "",
        append = append
    )
  }

  write_input("# Input file for program resample\n",
              "# Creating by input_file_utils.R\n",
              "# On ", as.character(Sys.time()),
              append = FALSE
  )

  write_input("INPUT RASTER: ", in_raster)
  write_input("OUTPUT RASTER: ", out_raster)
  write_input("SKIP: ", skip)
  write_input("SCRATCH DIRECTORY: ", scratch_dir)
}
