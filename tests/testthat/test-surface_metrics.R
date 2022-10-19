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
})

