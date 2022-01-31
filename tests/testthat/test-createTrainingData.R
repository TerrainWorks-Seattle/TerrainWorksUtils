test_that("samplePoints with default params", {
  sample <- samplePoints(
    20,
    analysisRegion
  )

  expect_length(sample, 20)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "polygons")

  # Check that points at non-NA locations
  sample_values <- terra::extract(analysisRegion, sample)
  expect_equal(sum(is.na(sample_values$elevation)), 0)

  # check that buffer is (approximately) correct size
  areas <- sapply(seq_along(sample), function(i) terra::expanse(sample[i]))
  expect_true(all(areas > pi * 15^2 - 15 / 3))
  expect_true(all(areas < pi * 15^2 + 15 / 3))
})

test_that("samplePoints with buffer = FALSE", {
  sample <- samplePoints(10,
    analysisRegion,
    buffer = FALSE
  )
  expect_length(sample, 10)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "points")

  sample2 <- samplePoints(10,
    analysisRegion,
    buffer = FALSE
  )

  # Check that samples are different!
  expect_failure(expect_setequal(
    terra::geom(sample)[, "x"],
    terra::geom(sample2)[, "x"]
  ))
})

test_that("samplePoints with many NA", {
  sample <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  expect_length(sample, 10)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "points")

  # Check that points at non-NA locations
  sample_values <- terra::extract(analysisRegionMask, sample)
  expect_equal(sum(is.na(sample_values$elevation)), 0)

  # Check that samples are different!
  sample2 <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  expect_failure(expect_setequal(
    terra::geom(sample)[, "x"],
    terra::geom(sample2)[, "x"]
  ))
})

test_that("samplePoints with buffer", {
  sample <- samplePoints(20,
    analysisRegion,
    buffer = TRUE,
    radius = 15
  )
  expect_length(sample, 20)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "polygons")

  # check that buffer is (approximately) correct size
  areas <- sapply(seq_along(sample), function(i) terra::expanse(sample[i]))
  expect_true(all(areas > pi * 15^2 - 15 / 3))
  expect_true(all(areas < pi * 15^2 + 15 / 3))

  # Different number of points and different radius
  sample <- samplePoints(15,
    analysisRegion,
    buffer = TRUE,
    radius = 25
  )
  expect_length(sample, 15)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "polygons")

  # check that buffer is (approximately) correct size
  areas <- sapply(seq_along(sample), function(i) terra::expanse(sample[i]))
  expect_true(all(areas > pi * 25^2 - 25 / 3))
  expect_true(all(areas < pi * 25^2 + 25 / 3))
})

test_that("sampleNegativePoints with default params", {
  positivePoints <- samplePoints(20,
    analysisRegion,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(
    positivePoints,
    analysisRegion
  )
  expect_s4_class(allPoints, "SpatVector")
  expect_equal(terra::geomtype(allPoints), "polygons")
  expect_setequal(unique(allPoints$class), c("positive", "negative"))
  expect_equal(sum(allPoints$class == "negative"), 20)
  expect_equal(sum(allPoints$class == "positive"), 20)
})

test_that("sampleNegativePoints with proportion of 2", {
  positivePoints <- samplePoints(20,
    analysisRegion,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(positivePoints,
    analysisRegion,
    negativeProportion = 2
  )
  expect_equal(sum(allPoints$class == "negative"), 40)
})

test_that("sampleNegativePoints with buffer = FALSE", {
  positivePoints <- samplePoints(20,
    analysisRegion,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(positivePoints,
    analysisRegion,
    buffer = FALSE
  )
  expect_s4_class(allPoints, "SpatVector")
  expect_equal(terra::geomtype(allPoints), "points")
  expect_equal(
    terra::geom(allPoints)[allPoints$class == "positive", "x"],
    terra::geom(positivePoints)[, "x"]
  )
})

test_that("sampleNegativePoints distance", {
  positivePoints <- samplePoints(20,
    analysisRegionMask,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(positivePoints,
    analysisRegionMask,
    bufferRadius = 1000,
    buffer = FALSE,
    negativeProportion = 0.5
  )
  negativePoints <- allPoints[allPoints$class == "negative"]
  positivePoints <- allPoints[allPoints$class == "positive"]
  expect_gt(min(terra::distance(negativePoints, positivePoints)), 1000)
})


test_that("extractValues from points", {
  positivePoints <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(positivePoints,
    analysisRegion,
    buffer = FALSE
  )

  values <- extractValues(varsRaster, allPoints, xy = TRUE)

  expect_equal(nrow(values), length(allPoints))
  expect_equal(terra::geom(allPoints)[, "x"], values$x)
  expect_equal(terra::geom(allPoints)[, "y"], values$y)
  expect_equal(allPoints$class, values$class)
  expect_setequal(
    names(values),
    c(names(varsRaster), "x", "y", "class")
  )
})

test_that("extractValues no xy", {
  positivePoints <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  allPoints <- sampleNegativePoints(positivePoints,
    analysisRegion,
    buffer = FALSE
  )

  values <- extractValues(varsRaster, allPoints, xy = FALSE)
  values_xy <- extractValues(varsRaster, allPoints, xy = TRUE)

  expect_equal(nrow(values), length(allPoints))
  expect_equal(allPoints$class, values$class)
  expect_equal(values$elevation, values_xy$elevation)
  expect_setequal(
    names(values),
    c(names(varsRaster), "class")
  )
})

test_that("extractValues from polygons centroid", {
  positivePoints <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  polygons <- sampleNegativePoints(positivePoints,
    analysisRegion,
    buffer = TRUE,
    bufferRadius = 500
  )
  values <- extractValues(varsRaster,
    polygons,
    extractionMethod = "centroid",
    xy = TRUE
  )

  expect_equal(nrow(values), length(polygons))
  expect_equal(polygons$class, values$class)

  # Convert values to vector for distance measure
  valueVect <- terra::vect(values, geom = c("x", "y"))
  terra::crs(valueVect) <- terra::crs(polygons)
  distances <- terra::distance(polygons, valueVect, pairwise = TRUE)
  expect_true(all(distances == 0))

  expect_setequal(
    names(values),
    c(names(varsRaster), "x", "y", "class")
  )
})

test_that("extractValues from polygons min/max", {
  positivePoints <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )
  polygons <- sampleNegativePoints(positivePoints,
    analysisRegion,
    buffer = TRUE,
    bufferRadius = 500
  )

  expect_error(
    {
      values <- extractValues(varsRaster,
        polygons,
        extractionMethod = "max",
        extractionLayer = "test"
      )
    },
    "could not find test"
  )

  # --- max ---#
  values_max <- extractValues(varsRaster,
    polygons,
    extractionMethod = "max",
    extractionLayer = "elevation",
    xy = TRUE
  )

  expect_equal(nrow(values_max), length(polygons))
  expect_equal(polygons$class, values_max$class)

  # Convert values to vector for distance measure
  valueVect <- terra::vect(values_max, geom = c("x", "y"))
  terra::crs(valueVect) <- terra::crs(polygons)
  distances <- terra::distance(polygons, valueVect, pairwise = TRUE)
  expect_true(all(distances == 0))


  expect_setequal(
    names(values_max),
    c(names(varsRaster), "x", "y", "class")
  )

  # --- min ---#
  values_min <- extractValues(varsRaster,
    polygons,
    extractionMethod = "min",
    extractionLayer = "elevation",
    xy = TRUE
  )

  expect_equal(nrow(values_min), length(polygons))
  expect_equal(polygons$class, values_min$class)

  # Convert values to vector for distance measure
  valueVect <- terra::vect(values_min, geom = c("x", "y"))
  terra::crs(valueVect) <- terra::crs(polygons)
  distances <- terra::distance(polygons, valueVect, pairwise = TRUE)
  expect_true(all(distances == 0))


  expect_setequal(
    names(values_min),
    c(names(varsRaster), "x", "y", "class")
  )

  expect_true(all(values_max$elevation >= values_min$elevation))


  # --- rand ---#
  values_min <- extractValues(varsRaster,
    polygons,
    extractionMethod = "min",
    extractionLayer = "random",
    na.rm = FALSE,
    xy = TRUE
  )
  values_max <- extractValues(varsRaster,
    polygons,
    extractionMethod = "max",
    extractionLayer = "random",
    na.rm = FALSE,
    xy = TRUE
  )

  expect_true(all(values_max$random >= values_min$random))
  # Convert values to vector for distance measure
  valueVect <- terra::vect(values_min, geom = c("x", "y"))
  terra::crs(valueVect) <- terra::crs(polygons)
  distances <- terra::distance(polygons, valueVect, pairwise = TRUE)
  expect_true(all(distances == 0))
})


test_that("createTrainingDataFromPoints default params", {
  positivePoints <- samplePoints(10,
    analysisRegionMask,
    buffer = FALSE
  )

  trainingData <- createTrainingDataFromPoints(
    positivePoints = positivePoints,
    predictorsRaster = varsRaster,
    analysisRegionMask = analysisRegionMask
  )

  expect_setequal(names(trainingData), c(names(varsRaster), "class"))
  expect_equal(nrow(trainingData), 2 * nrow(positivePoints))
})

test_that("createTrainingDataFromPolygons default params", {
  wetlands <- testPolygons[sample(length(testPolygons), 3, replace = FALSE)]
  data <- createTrainingDataFromPolygons(
    polygons = wetlands,
    analysisRegion = analysisRegionPolygon,
    predictorsRaster = varsRaster
  )
  expect_setequal(unique(data$class), c("positive", "negative"))
})

test_that("createTrainingDataFromPolygons sample rate", {
  wetlands <- testPolygons[sample(length(testPolygons), 3, replace = FALSE)]
  data <- createTrainingDataFromPolygons(
    polygons = wetlands,
    analysisRegion = analysisRegionPolygon,
    predictorsRaster = varsRaster,
    sampleRate = 0.5
  )

  # Get expected count. First have to determine allowable region
  region <- all(predictorsRaster)
  region <- terra::crop(region, analysisRegionPolygon)
  expected_count <- terra::expanse(region) / (1000 * 1000) * 0.5
  # Leave room for some loss due to NA
  expect_gt(nrow(data), expected_count - expected_count * 0.1)
  expect_lte(nrow(data), expected_count + expected_count * 0.1)
})
