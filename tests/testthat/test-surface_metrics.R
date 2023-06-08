test_that("elev_deriv: build input file, all derivatives", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")

  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "/grad"),
                     paste0("PLAN CURVATURE,", scratch_dir, "/plan"),
                     paste0("PROFILE CURVATURE,", scratch_dir, "/prof"),
                     paste0("NORMAL SLOPE CURVATURE,", scratch_dir, "/norm"),
                     paste0("TANGENTIAL CURVATURE,", scratch_dir, "/tan"))


  out_raster <- elev_deriv(rasters = rasters_input,
                           length_scale = 15,
                           dem = dem_path,
                           scratch_dir = scratch_dir)

  expect_equal(nlyr(out_raster), 5)
  expect_equal(names(out_raster), c("GRADIENT",
                                    "PLAN CURVATURE",
                                    "PROFILE CURVATURE",
                                    "NORMAL SLOPE CURVATURE",
                                    "TANGENTIAL CURVATURE"))
  expect_true(file.exists(paste0(scratch_dir,"/grad.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/plan.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/prof.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/norm.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/tan.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/makegrids_input.txt")))
  unlink(paste0(scratch_dir,"\\*"))
})

test_that("elev_deriv: build input file, 1 derivative", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")


  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "/grad"))


  out_raster <- elev_deriv(rasters = rasters_input,
                           length_scale = 15,
                           dem = dem_path,
                           scratch_dir = scratch_dir)

  expect_equal(nlyr(out_raster), 1)
  expect_equal(names(out_raster), c("GRADIENT"))
  expect_true(file.exists(paste0(scratch_dir,"/grad.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/makegrids_input.txt")))
  unlink(paste0(scratch_dir,"\\*"))
})

test_that("elev_deriv: length scale too small (<= 0)", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")


  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "/grad"))


  expect_error(elev_deriv(rasters = rasters_input,
                           length_scale = -1,
                           dem = dem_path,
                           scratch_dir = scratch_dir),
               "Length scale not specified or out-of-bounds")
  expect_error(elev_deriv(rasters = rasters_input,
                          length_scale = 0,
                          dem = dem_path,
                          scratch_dir = scratch_dir),
               "Length scale not specified or out-of-bounds")
})

test_that("elev_deriv: existing input file, no other args", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  input_file <- paste0(data_path, "\\makegrids_input_sample.txt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  unlink(paste0(scratch_dir,"\\*"))

  out_raster <- elev_deriv(input_file = input_file)

  expect_equal(nlyr(out_raster), 5)
  expect_equal(names(out_raster), c("GRADIENT",
                                    "PLAN CURVATURE",
                                    "PROFILE CURVATURE",
                                    "NORMAL SLOPE CURVATURE",
                                    "TANGENTIAL CURVATURE"))
  expect_true(file.exists(paste0(scratch_dir,"/grad.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/plan.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/prof.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/norm.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/tan.flt")))
  unlink(paste0(scratch_dir,"\\*"))
})

test_that("elev_deriv: existing input file, ignore other inputs", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  input_file <- paste0(data_path, "\\makegrids_input_sample.txt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  unlink(paste0(scratch_dir,"\\*"))

  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "/grad"),
                     paste0("PLAN CURVATURE,", scratch_dir, "/plan"),
                     paste0("PROFILE CURVATURE,", scratch_dir, "/prof"))

  out_raster <- elev_deriv(input_file = input_file,
                           rasters = rasters_input)

  expect_equal(nlyr(out_raster), 5)
  expect_equal(names(out_raster), c("GRADIENT",
                                    "PLAN CURVATURE",
                                    "PROFILE CURVATURE",
                                    "NORMAL SLOPE CURVATURE",
                                    "TANGENTIAL CURVATURE"))
  expect_true(file.exists(paste0(scratch_dir,"/grad.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/plan.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/prof.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/norm.flt")))
  expect_true(file.exists(paste0(scratch_dir,"/tan.flt")))
  unlink(paste0(scratch_dir,"\\*"))
})

test_that("elev_deriv: non-existent, empty, or bad input file", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  nonexist_file <- tempfile(tmpdir = scratch_dir)

  expect_error(elev_deriv(input_file = nonexist_file), "Input file not found")

  file.create(nonexist_file)
  expect_error(elev_deriv(input_file = nonexist_file), "Input file is empty")

  writeLines("bad format", nonexist_file)
  expect_error(elev_deriv(input_file = nonexist_file), "Bad input file format")

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("elev_deriv: non-existent scratch dir", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")

  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "/grad"))

  expect_error(elev_deriv(rasters = rasters_input,
                          length_scale = 1,
                          dem = dem_path,
                          scratch_dir = paste0(scratch_dir,"bad")),
               "Scratch directory does not exist")

})

test_that("elev_deriv: no derivatives given", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")

  rasters_input <- list()

  expect_error(elev_deriv(rasters = rasters_input,
                          length_scale = 1,
                          dem = dem_path,
                          scratch_dir = scratch_dir),
               "Must provide at least one derivative to calculate")

})

test_that("elev_deriv: read existing rasters", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")

  rasters_input <- c(paste0("GRADIENT,", data_path, "/elev_scottsburg.flt"))

  expect_error(elev_deriv(rasters = list(),
                          length_scale = 1,
                          scratch_dir = scratch_dir),
               "Must provide a DEM or an existing raster file to read")

  out_raster <- elev_deriv(rasters = rasters_input)

  expect_equal(nlyr(out_raster), 1)
  expect_equal(names(out_raster), "GRADIENT")

  expect_error(elev_deriv(rasters = paste0("GRADIENT,", data_path, "/bad.flt"),
                          length_scale = 100),
               "All given files must exist to run in read mode")

})

test_that("contributing_area: build input file", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")

  out_raster <- contributing_area(raster = paste0(scratch_dir,"/pca.flt"),
                                  dem = dem_path,
                                  length_scale = 15,
                                  k = 1,
                                  d = 5,
                                  scratch_dir = scratch_dir)

  expect_equal(nlyr(out_raster), 1)
  expect_true(file.exists(paste0(scratch_dir, "/pca.flt")))
  expect_true(file.exists(paste0(scratch_dir, "/partial_input.txt")))

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("contributing_area: existing input file, no args", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  input_file <- paste0(data_path, "\\partial_input_sample.txt")

  out_raster <- contributing_area(input_file = input_file)

  expect_equal(nlyr(out_raster), 1)
  expect_true(file.exists(paste0(scratch_dir, "/pca.flt")))

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("contributing_area: existing input file, no args", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  input_file <- paste0(data_path, "\\partial_input_sample.txt")

  out_raster <- contributing_area(input_file = input_file)

  expect_equal(nlyr(out_raster), 1)
  expect_true(file.exists(paste0(scratch_dir, "/pca.flt")))

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("contributing_area: existing input file, ignore args", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  input_file <- paste0(data_path, "\\partial_input_sample.txt")

  out_raster <- contributing_area(input_file = input_file,
                                  raster = paste0(scratch_dir, "/fake"),
                                  k = 0,
                                  d = -1)

  expect_equal(nlyr(out_raster), 1)
  expect_true(file.exists(paste0(scratch_dir, "/pca.flt")))

  out_raster2 <- contributing_area(raster = paste0(scratch_dir, "/pca.flt"))
  expect_equal(nlyr(out_raster2), 1)

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("contributing_area: nonexistent, empty, and bad format input", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  nonexist_file <- tempfile(tmpdir = scratch_dir)

  expect_error(contributing_area(input_file = nonexist_file), "Input file not found")

  file.create(nonexist_file)
  expect_error(contributing_area(input_file = nonexist_file), "Input file is empty")

  writeLines("bad format", nonexist_file)
  expect_error(contributing_area(input_file = nonexist_file), "Bad input file format")

  unlink(paste0(scratch_dir,"\\*"))
})

test_that("contributing_area: out of bounds length_scale, k, d", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")


  raster_input <- c(paste0(scratch_dir, "/pca.flt"))


  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = -1,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Length scale not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 0,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Length scale not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Length scale not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 k = 0,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Saturated hydraulic conductivity not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 k = -1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Saturated hydraulic conductivity not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 d = 6,
                                 scratch_dir = scratch_dir),
               "Saturated hydraulic conductivity not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 k = 1,
                                 d = 0,
                                 scratch_dir = scratch_dir),
               "Storm duration not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 k = 1,
                                 d = -1,
                                 scratch_dir = scratch_dir),
               "Storm duration not specified or out-of-bounds")
  expect_error(contributing_area(raster = raster_input,
                                 dem = dem_path,
                                 length_scale = 15,
                                 k = 1,
                                 scratch_dir = scratch_dir),
               "Storm duration not specified or out-of-bounds")
})

test_that("contributing_area: bad dem, no scratch directory", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elevation.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")

  raster_input <- c(paste0(scratch_dir, "/pca.flt"))


  expect_error(contributing_area(raster = raster_input,
                                 dem = paste0(data_path, "/fakedem.flt"),
                                 length_scale = 15,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "DEM does not exist")
  expect_error(contributing_area(raster = raster_input,
                                 length_scale = 15,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = scratch_dir),
               "Must provide a DEM or an existing raster file to read")
  expect_error(contributing_area(raster = raster_input,
                                 dem= dem_path,
                                 length_scale = 15,
                                 k = 1,
                                 d = 5,
                                 scratch_dir = paste0(data_path, "/fakedir")),
               "Scratch directory does not exist or is not specified")
  expect_error(contributing_area(raster = raster_input,
                                 dem= dem_path,
                                 length_scale = 15,
                                 k = 1,
                                 d = 5),
               "Scratch directory does not exist or is not specified")


})
