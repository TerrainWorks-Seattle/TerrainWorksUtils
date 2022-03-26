# https://gis.stackexchange.com/questions/140389/converting-dem-to-floating-point-raster-flt-using-qgis

convert_hdr <- function(input_hdr,
                        output_hdr = input_hdr,
                        overwrite = TRUE) {

  if (file.exists(output_hdr)) {
    if (overwrite) {
      message("overwriting ", output_hdr)
    } else {
      stop(output_hdr, " exists")
    }
  }

  lines <- readLines(input_hdr)
  extract_value <- function(pattern) {
    val <- lines[grepl(pattern, lines)]
    # Get all non-space characters preceeding end of string
    regmatches(val, regexpr("\\S+$", val))
  }

  nrow <- extract_value("NROWS")
  ncol <- extract_value("NCOLS")
  nodata_value <- extract_value("NODATA")

  # Specified only for byteorder = I (Intel)
  byteorder <- extract_value("BYTEORDER")
  if (byteorder != "I") {
    stop("Cannot convert with bytorder ", byteorder)
  }
  byteorder <- "LSBFIRST"

  # assuming Intel, only one cellsize is supported so get
  # rid of XDIM, YDIM and replace with CELLSIZE
  cellsize <- extract_value("XDIM")

  # BIL is upper left referenced but FLT is lower left referenced so
  # you need to reduce ulymap by NROWS * CELLSIZE to obtain YLLCORNER.
  xllcorner <- extract_value("ULXMAP")
  ulymap <- extract_value("ULYMAP")
  yllcorner <- as.numeric(ulymap) - (
    (as.numeric(nrow) * as.numeric(cellsize))
  )

  write_line <- function(key, value, append = TRUE) {
    # key and value with space between
    cat(sprintf("%-15s%s\n", key, value),
        file = output_hdr,
        append = append)
  }
  write_line("ncols", ncol, append = FALSE)
  write_line("nrows", nrow)
  write_line("xllcorner", xllcorner)
  write_line("yllcorner", yllcorner)
  write_line("cellsize", cellsize)
  write_line("NODATA_value", nodata_value)
  write_line("BYTEORDER", byteorder)
}
