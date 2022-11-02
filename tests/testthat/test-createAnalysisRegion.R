test_that("extractRange expansion expands correctly", {
  points <- sample_points(50, analysis_region)
  values <- terra::extract(analysis_region, points)$elevation
  max <- max(values, na.rm = TRUE)
  min <- min(values, na.rm = TRUE)

  range <- extractRange(
    raster = analysis_region,
    extractionLocations = points,
    expansionFactor = 1
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal(min, range[, "min"])
  expect_equal(max, range[, "max"])

  range <- extractRange(
    raster = analysis_region,
    extractionLocations = points,
    expansionFactor = 0
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal(range[, "min"], range[, "max"])
  expect_equal(range[, "min"], mean(c(min, max)))

  range <- extractRange(
    raster = analysis_region,
    extractionLocations = points,
    expansionFactor = 1.5
  )

  expect_equal(mean(range), mean(c(min, max)))
  expect_equal((max - min) * 1.5, (range[, "max"] - range[, "min"]))
})

test_that("maskByRange", {
  points <- sample_points(20, analysis_region_mask)
  rangeMx <- extractRange(
    vars_raster,
    points
  )

  mask <- maskByRange(vars_raster, rangeMx)


  expect_setequal(unique(terra::values(mask)), c(NA, 1))
  ranges <- lapply(vars_raster, function(r) {
    range(terra::values(terra::mask(r, mask)), na.rm = TRUE)
  })

  names(ranges) <- names(vars_raster)
  for (var in rownames(rangeMx)) {

    expect_gte(ranges[[var]][[1]], rangeMx[var, "min"])
    expect_lte(ranges[[var]][[2]], rangeMx[var, "max"])
  }
})


test_that("maskByRange for subset of variables", {
  points <- sample_points(20, analysis_region_mask)
  rangeMx <- extractRange(
    terra::subset(vars_raster, c("elevation", "inverse")),
    points
  )

  mask <- maskByRange(vars_raster, rangeMx)
  expect_setequal(unique(terra::values(mask)), c(NA, 1))
  ranges <- lapply(vars_raster, function(r) {
    range(terra::values(terra::mask(r, mask)), na.rm = TRUE)
  })

  names(ranges) <- names(vars_raster)
  for (var in rownames(rangeMx)) {
    expect_gte(ranges[[var]][[1]], rangeMx[var, "min"])
    expect_lte(ranges[[var]][[2]], rangeMx[var, "max"])
  }
})
