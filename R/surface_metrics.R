# Functions for elevation derivatives and other things calculated from a DEM

#----------------------------------------------------------------
#' Elevation Derivatives
#'
#' Calculate gradient and curvature.
#' This function runs the makegrids program, reads the binary floating point
#' raster(s) created by makegrids, and translates these to a
#' \code{SpatRaster} (from the \href{https://rspatial.org/terra}{terra} package).
#'
#' @param input_file: Character string; makegrids input file. If no input file
#'   is specified, a Windows Explorer window opens for file selection.
#'
#' @return \code{SpatRaster}, one layer for each requested elevation derivative.
#' @export
#'
elev_deriv <- function(input_file = "nofile") {
  # elev_deriv reads a makegrids ASCII input file
  if (str_detect(input_file, "nofile")) {
    input_file <- file.choose()
    infile <- tibble(readLines(input_file))
  } else {
    if (!file.exists(input_file)) {
      stop("Input file not found")
    } else {
      infile <- tibble(readLines(input_file))
    }
  }
  if (nrow(infile) == 0) {
    stop("Input file is empty")
  }

  # Get the location of the Fortran compiled code for makegrids.exe
  executable_path <- get_executable_path()

  makeGrids <- file.path(executable_path, "MakeGrids.exe")
  command <- paste(makeGrids, input_file, sep = " ")
  output <- system(command, wait = TRUE)
  if (output != 0) {
    warning("Problem calculating elevation derivatives: error ", output)
  }

  # Get a list of output rasters
  output_list <- list()
  for (i in 1:nrow(infile)) {
    keyword <- get_keyword(infile, i)
    if (is.na(keyword)) {
      next
    }
    if (str_detect(keyword, "GRID") == TRUE) {
      argument <- get_args(infile, i)
      param_value <- parse_arg(argument, 2)
      output_list <- c(output_list, param_value[[2]])
    }
  }
  # Create a spatraster with one layer for each output grid
  out_grid <- rast(output_list[[1]])
  if (length(output_list) > 1) {
    for (i in 2:length(output_list)) {
    out_grid <- c(out_grid, rast(output_list[[i]]))
    }
  }

  return(out_grid)
}
