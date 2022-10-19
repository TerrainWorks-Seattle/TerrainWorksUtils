test_that("no input file, all derivatives", {
  data_path <- system.file("examples", package = "TerrainWorksUtils")
  dem_path <- paste0(data_path, "\\elev_scottsburg.flt")
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  unlink(paste0(scratch_dir,"\\*"))


  rasters_input <- c(paste0("GRADIENT,", scratch_dir, "\\grad"),
                     paste0("PLAN CURVATURE,", scratch_dir, "\\plan"),
                     paste0("PROFILE CURVATURE,", scratch_dir, "\\prof"),
                     paste0("NORMAL SLOPE CURVATURE,", scratch_dir, "\\norm"),
                     paste0("TANGENTIAL CURVATURE,", scratch_dir, "\\tan"))


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
})

test_that("existing input file, no other args", {
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
})

test_that("bad or empty input file", {
  scratch_dir <- system.file("scratch", package = "TerrainWorksUtils")
  nonexist_file <- tempfile(tmpdir = scratch_dir)

  expect_error(elev_deriv(input_file = nonexist_file), "Input file not found")

  file.create(nonexist_file)
  expect_error(elev_deriv(input_file = nonexist_file), "Input file is empty")

  writeLines("bad format", nonexist_file)
  expect_error(elev_deriv(input_file = nonexist_file), "Bad input file format")
})

