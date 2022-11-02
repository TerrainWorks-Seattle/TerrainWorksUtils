test_that("extract_range expansion expands correctly", {
  points <- sample_points(50, analysis_region)
  values <- terra::extract(analysis_region, points)$elevation
  max <- max(values, na.rm = TRUE)
  min <- min(values, na.rm = TRUE)

  range <- extract_range(
    raster = analysis_region,
    extraction_locations = points,
    expansion_factor = 1
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal(min, range[, "min"])
  expect_equal(max, range[, "max"])

  range <- extract_range(
    raster = analysis_region,
    extraction_locations = points,
    expansion_factor = 0
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal(range[, "min"], range[, "max"])
  expect_equal(range[, "min"], mean(c(min, max)))

  range <- extract_range(
    raster = analysis_region,
    extraction_locations = points,
    expansion_factor = 1.5
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal((max - min) * 1.5, (range[, "max"] - range[, "min"]))
})

test_that("mask_by_range", {
  points <- sample_points(20, analysis_region_mask)
  range_mx <- extract_range(
    vars_raster,
    points
  )

  mask <- mask_by_range(vars_raster, range_mx)


  expect_setequal(unique(terra::values(mask)), c(NA, 1))
  ranges <- lapply(vars_raster, function(r) {
    range(terra::values(terra::mask(r, mask)), na.rm = TRUE)
  })

  names(ranges) <- names(vars_raster)
  for (var in rownames(range_mx)) {

    expect_gte(ranges[[var]][[1]], range_mx[var, "min"])
    expect_lte(ranges[[var]][[2]], range_mx[var, "max"])
  }
})


test_that("mask_by_range for subset of variables", {
  points <- sample_points(20, analysis_region_mask)
  range_mx <- extract_range(
    terra::subset(vars_raster, c("elevation", "inverse")),
    points
  )

  mask <- mask_by_range(vars_raster, range_mx)
  expect_setequal(unique(terra::values(mask)), c(NA, 1))
  ranges <- lapply(vars_raster, function(r) {
    range(terra::values(terra::mask(r, mask)), na.rm = TRUE)
  })

  names(ranges) <- names(vars_raster)
  for (var in rownames(range_mx)) {
    expect_gte(ranges[[var]][[1]], range_mx[var, "min"])
    expect_lte(ranges[[var]][[2]], range_mx[var, "max"])
  }
})
