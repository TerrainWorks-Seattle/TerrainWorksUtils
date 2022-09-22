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
#' @import stringr

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
#' @param dem The file name (full path) for the input DEM.
#' @param k Saturated hydraulic conductivity, in meters per hour.
#' @param d Storm duration in hours.
#' @param length_scale Diameter for smoothing the DEM.
#' @param scratch_dir Directory for temporary files. The input file is written
#'   to the scratch_dir.
#' @param out_file File name (full path) for the output binary floating point
#'   (.flt) raster.
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
                        out_file) {

  if (!dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }

  # Normalize paths
  dem <- normalizePath(dem)
  scratch_dir <- normalizePath(scratch_dir)

  out_file <- paste0(scratch_dir, "\\partial_input.txt")
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
  write_input("DURATION: ", d)
  write_input("CONDUCTIVITY: ", k)
  write_input("OUTPUT RASTER: ", out_file)
}
