#' Get executable path
#'
#' Returns the filepath to the executable files for calculating elevation
#' derivatives. There must be a .zip file called "files.zip" in a directory
#' called "DEMutilities", OR the executables must be unzipped into a
#' "/DEMutilities/files/" folder.
#'
#'
#' @return The directory of the executable files.
#' @export
get_executable_path <- function() {
  utilities_dir <- system.file(dir = "/DEMUtilities", package = "TerrainWorksUtils")
  executable_zip <- file.path(utilities_dir, "files.zip")
  executable_dir <- file.path(utilities_dir, "files")
  print(executable_dir)
  
  if ((!dir.exists(executable_dir) | length(list.files(path = executable_dir)) == 0)
        & file.exists(executable_zip)) {
    utils::unzip(executable_zip, exdir = executable_dir)
  }
  if (!dir.exists(executable_dir) & !file.exists(executable_zip)) {
      print(paste0("Cannot find ", executable_zip))
      stop("Good Bye")
  }
  return(executable_dir)
}

# Write input file for MakeGrids.exe
write_input_file_MakeGrids <- function(DEM_path,
                                       length_scale,
                                       output_dir = getwd(),
                                       grad = TRUE,
                                       plan = TRUE,
                                       prof = TRUE,
                                       bcon = TRUE,
                                       filename = file.path(output_dir, "input_makeGrids.txt"),
                                       overwrite = TRUE,
                                       output_file_extension = paste0("_", length_scale)) {
  if (!dir.exists(output_dir)) {
    stop("invalid scratch folder: ", output_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if (file.exists(filename)) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  }

  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  output_dir <- normalizePath(output_dir)

  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
      file = filename,
      sep = "",
      append = append
    )
  }

  write_input("# Input file for makeGrids\n",
    "# Creating by surfaceMetrics.R\n",
    "# On ", as.character(Sys.time()),
    append = FALSE
  )


  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", output_dir)
  write_input("LENGTH SCALE: ", length_scale)

  if (grad) {
    write_input(
      "GRID: GRADIENT, OUTPUT FILE = ",
      paste0("grad", output_file_extension)
    )
  }

  if (plan) {
    write_input(
      "GRID: PLAN CURVATURE, OUTPUT FILE = ",
      paste0("plan", output_file_extension)
    )
  }

  if (prof) {
    write_input(
      "GRID: PROFILE CURVATURE, OUTPUT FILE = ",
      paste0("prof", output_file_extension)
    )
  }

  if (bcon) {
    write_input(
      "GRID: BCONTOUR, OUTPUT FILE = ",
      paste0("bcon", output_file_extension)
    )
  }
}

write_input_file_localRelief <- function(DEM_path,
                                         length_scale,
                                         output_dir = getwd(),
                                         resample = 2,
                                         interval = 2,
                                         filename = file.path(output_dir, "input_localRelief.txt"),
                                         overwrite = TRUE,
                                         output_file_extension = paste0("_", length_scale)) {
  if (!dir.exists(output_dir)) {
    stop("invalid scratch folder: ", output_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if (file.exists(filename)) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  }


  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  output_dir <- normalizePath(output_dir)

  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
      file = filename,
      sep = "",
      append = append
    )
  }

  write_input("# Input file for LocalRelief\n",
    "# Creating by surfaceMetrics.R\n",
    "# On ", as.character(Sys.time()),
    append = FALSE
  )
  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", output_dir)

  radius <- length_scale / 2
  write_input("RADIUS: ", radius)
  write_input("DOWN SAMPLE: ", resample)
  write_input("SAMPLE INTERVAL: ", resample)
  write_input("OUTPUT DEV RASTER: ", paste0("dev", output_file_extension))
}

write_input_file_buildGrids <- function(DEM_path,
                                        length_scale,
                                        output_dir = getwd(),
                                        grad_path,
                                        plan_path,
                                        bcon_path,
                                        slope_lo = "30.",
                                        slope_hi = "60.",
                                        plan_lo = "100000.15",
                                        plan_hi = "100000.3",
                                        filename = file.path(output_dir, "input_buildGrids.txt"),
                                        overwrite = TRUE,
                                        output_file_extension = paste0("_", length_scale)) {
  if (!dir.exists(output_dir)) {
    stop("invalid scratch folder: ", output_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if (file.exists(filename)) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  }


  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  output_dir <- normalizePath(output_dir)
  grad_path <- normalizePath(grad_path)
  plan_path <- normalizePath(plan_path)
  bcon_path <- normalizePath(bcon_path)

  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }
  # remove elev_ from DEM_path
  DEM_id <- gsub("^\\w+_", "", basename(DEM_path))

  # Get DEM units
  dem <- terra::rast(paste0(DEM_path, ".flt"))
  DEM_units <- ifelse(terra::linearUnits(dem) == 1, "m", "f")

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
      file = filename,
      sep = "",
      append = append
    )
  }

  write_input("# Input file for BuildGrids\n",
    "# Creating by surfaceMetrics.R\n",
    "# On ", as.character(Sys.time()),
    append = FALSE
  )

  write_input("DEM: ", DEM_path)
  write_input("DEMID: ", DEM_id)
  write_input("DEM UNITS: ", DEM_units)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("SCRATCH: ", output_dir)
  write_input("AREA SLOPE THRESHOLD LOW GRADIENT: ", slope_lo)
  write_input("AREA SLOPE THRESHOLD HIGH GRADIENT: ", slope_hi)
  write_input("PLAN CURVATURE THRESHOLD LOW GRADIENT: ", plan_lo)
  write_input("PLAN CURVATURE THRESHOLD HIGH GRADIENT: ", plan_hi)
  write_input("GRADIENT FILE: ", grad_path)
  write_input("PLAN CURVATURE FILE: ", plan_path)
  write_input("BCON FILE: ", bcon_path)
}


write_input_file_Partial <- function(DEM_path,
                                     length_scale,
                                     duration = 48,
                                     conductivity = 2,
                                     output_dir = getwd(),
                                     filename = file.path(output_dir, "input_partial.txt"),
                                     overwrite = TRUE,
                                     output_file_extension = paste0("_", length_scale)) {
  if (!dir.exists(output_dir)) {
    stop("invalid scratch folder: ", output_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if (file.exists(filename)) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  }


  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  output_dir <- normalizePath(output_dir)

  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
      file = filename,
      sep = "",
      append = append
    )
  }

  write_input("# Input file for Partial\n",
    "# Creating by surfaceMetrics.R\n",
    "# On ", as.character(Sys.time()),
    append = FALSE
  )
  write_input("DEM: ", DEM_path)
  write_input("SCRATCH DIRECTORY: ", output_dir)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("DURATION: ", duration)
  write_input("CONDUCTIVITY: ", conductivity)
  write_input("OUTPUT RASTER: ", paste0("pca_", duration, "hr", output_file_extension))
}

write_input_file_buildGrids <- function(DEM_path,
                                        length_scale,
                                        output_dir = getwd(),
                                        grad_path,
                                        plan_path,
                                        bcon_path,
                                        slope_lo = "30.",
                                        slope_hi = "60.",
                                        plan_lo = "100000.15",
                                        plan_hi = "100000.3",
                                        filename = file.path(output_dir, "input_buildGrids.txt"),
                                        overwrite = TRUE,
                                        output_file_extension = paste0("_", length_scale)) {
  if (!dir.exists(output_dir)) {
    stop("invalid scratch folder: ", output_dir)
  }
  if (!is.numeric(length_scale)) {
    stop("length_scale must be numeric")
  }
  if (file.exists(filename)) {
    if (overwrite) {
      message("overwriting ", filename)
    } else {
      stop(filename, " exists. Set overwrite = TRUE to overwrite.")
    }
  }


  # Normalize paths
  DEM_path <- normalizePath(DEM_path)
  output_dir <- normalizePath(output_dir)
  grad_path <- normalizePath(grad_path)
  plan_path <- normalizePath(plan_path)
  bcon_path <- normalizePath(bcon_path)

  # Do not include ".flt" in DEM_path
  if (grepl("\\.flt$", DEM_path)) {
    DEM_path <- gsub("\\.flt$", "", DEM_path)
  }
  # remove elev_ from DEM_path
  DEM_id <- gsub("^\\w+_", "", basename(DEM_path))

  # Get DEM units
  dem <- terra::rast(paste0(DEM_path, ".flt"))
  DEM_units <- ifelse(terra::linearUnits(dem) == 1, "m", "f")

  write_input <- function(...,
                          append = TRUE) {
    cat(..., "\n",
      file = filename,
      sep = "",
      append = append
    )
  }

  write_input("# Input file for BuildGrids\n",
    "# Creating by surfaceMetrics.R\n",
    "# On ", as.character(Sys.time()),
    append = FALSE
  )

  write_input("DEM: ", DEM_path)
  write_input("DEMID: ", DEM_id)
  write_input("DEM UNITS: ", DEM_units)
  write_input("LENGTH SCALE: ", length_scale)
  write_input("SCRATCH: ", output_dir)
  write_input("AREA SLOPE THRESHOLD LOW GRADIENT: ", slope_lo)
  write_input("AREA SLOPE THRESHOLD HIGH GRADIENT: ", slope_hi)
  write_input("PLAN CURVATURE THRESHOLD LOW GRADIENT: ", plan_lo)
  write_input("PLAN CURVATURE THRESHOLD HIGH GRADIENT: ", plan_hi)
  write_input("GRADIENT FILE: ", grad_path)
  write_input("PLAN CURVATURE FILE: ", plan_path)
  write_input("BCON FILE: ", bcon_path)
}
