test_that("sample_points with default params", {
  sample <- sample_points(
    20,
    analysis_region
  )

  expect_length(sample, 20)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "polygons")

  # Check that points at non-NA locations
  sample_values <- terra::extract(analysis_region, sample)
  expect_equal(sum(is.na(sample_values$elevation)), 0)

  # check that buffer is (approximately) correct size
  areas <- sapply(seq_along(sample), function(i) terra::expanse(sample[i]))
  expect_true(all(areas > pi * 15^2 - 15 / 3))
  expect_true(all(areas < pi * 15^2 + 15 / 3))
})

test_that("sample_points with buffer = FALSE", {
  sample <- sample_points(10,
    analysis_region,
    buffer = FALSE
  )
  expect_length(sample, 10)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "points")

  sample2 <- sample_points(10,
    analysis_region,
    buffer = FALSE
  )

  # Check that samples are different!
  expect_failure(expect_setequal(
    terra::geom(sample)[, "x"],
    terra::geom(sample2)[, "x"]
  ))
})

test_that("sample_points with many NA", {
  sample <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  expect_length(sample, 10)
  expect_s4_class(sample, "SpatVector")
  expect_equal(terra::geomtype(sample), "points")

  # Check that points at non-NA locations
  sample_values <- terra::extract(analysis_region_mask, sample)
  expect_equal(sum(is.na(sample_values$elevation)), 0)

  # Check that samples are different!
  sample2 <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  expect_failure(expect_setequal(
    terra::geom(sample)[, "x"],
    terra::geom(sample2)[, "x"]
  ))
})

test_that("sample_points with buffer", {
  sample <- sample_points(20,
    analysis_region,
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
  sample <- sample_points(15,
    analysis_region,
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

test_that("sample_negative_points with default params", {
  positive_points <- sample_points(20,
    analysis_region,
    buffer = FALSE
  )
  all_points <- sample_negative_points(
    positive_points,
    analysis_region
  )
  expect_s4_class(all_points, "SpatVector")
  expect_equal(terra::geomtype(all_points), "polygons")
  expect_setequal(unique(all_points$class), c("positive", "negative"))
  expect_equal(sum(all_points$class == "negative"), 20)
  expect_equal(sum(all_points$class == "positive"), 20)

  # Each buffer should be the same size
  areas <- round(terra::expanse(all_points))
  expect_true(all(areas == areas[1]))
})

test_that("sample_negative_points with proportion of 2", {
  positive_points <- sample_points(20,
    analysis_region,
    buffer = FALSE
  )
  all_points <- sample_negative_points(positive_points,
    analysis_region,
    negative_proportion = 2
  )
  expect_equal(sum(all_points$class == "negative"), 40)
})

test_that("sample_negative_points with buffer = FALSE", {
  positive_points <- sample_points(20,
    analysis_region,
    buffer = FALSE
  )
  all_points <- sample_negative_points(positive_points,
    analysis_region,
    buffer = FALSE
  )
  expect_s4_class(all_points, "SpatVector")
  expect_equal(terra::geomtype(all_points), "points")
  expect_equal(
    terra::geom(all_points)[all_points$class == "positive", "x"],
    terra::geom(positive_points)[, "x"]
  )
})

test_that("sample_negative_points distance", {
  positive_points <- sample_points(20,
    analysis_region_mask,
    buffer = FALSE
  )
  all_points <- sample_negative_points(positive_points,
    analysis_region_mask,
    buffer_radius = 1000,
    buffer = FALSE,
    negative_proportion = 0.5
  )
  negative_points <- all_points[all_points$class == "negative"]
  positive_points <- all_points[all_points$class == "positive"]
  expect_gt(min(terra::distance(negative_points, positive_points)), 1000)
})


test_that("extract_values from points", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  all_points <- sample_negative_points(positive_points,
    analysis_region,
    buffer = FALSE
  )

  values <- extract_values(vars_raster, all_points, xy = TRUE)

  expect_equal(nrow(values), length(all_points))
  expect_equal(terra::geom(all_points)[, "x"], values$x)
  expect_equal(terra::geom(all_points)[, "y"], values$y)
  expect_equal(all_points$class, values$class)
  expect_setequal(
    names(values),
    c(names(vars_raster), "x", "y", "class")
  )
})

test_that("extract_values no xy", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  all_points <- sample_negative_points(positive_points,
    analysis_region,
    buffer = FALSE
  )

  values <- extract_values(vars_raster, all_points, xy = FALSE)
  values_xy <- extract_values(vars_raster, all_points, xy = TRUE)

  expect_equal(nrow(values), length(all_points))
  expect_equal(all_points$class, values$class)
  expect_equal(values$elevation, values_xy$elevation)
  expect_setequal(
    names(values),
    c(names(vars_raster), "class")
  )
})

test_that("extract_values from polygons centroid", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  polygons <- sample_negative_points(positive_points,
    analysis_region,
    buffer = TRUE,
    buffer_radius = 500
  )
  values <- extract_values(vars_raster,
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
    c(names(vars_raster), "x", "y", "class")
  )
})

test_that("extract_values from polygons min/max", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  polygons <- sample_negative_points(positive_points,
    analysis_region,
    buffer = TRUE,
    buffer_radius = 500
  )

  expect_error(
    {
      values <- extract_values(vars_raster,
        polygons,
        extraction_method = "max",
        extraction_layer = "test"
      )
    },
    "could not find test"
  )

  # --- max ---#
  values_max <- extract_values(vars_raster,
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
    c(names(vars_raster), "x", "y", "class")
  )

  # --- min ---#
  values_min <- extract_values(vars_raster,
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
    c(names(vars_raster), "x", "y", "class")
  )

  expect_true(all(values_max$elevation >= values_min$elevation))


  # --- rand ---#
  values_min <- extract_values(vars_raster,
    polygons,
    extractionMethod = "min",
    extractionLayer = "random",
    na.rm = FALSE,
    xy = TRUE
  )
  values_max <- extract_values(vars_raster,
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

test_that("extract_values polygons", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )
  polygons <- sample_negative_points(positive_points,
    analysis_region,
    buffer = TRUE
  )
  values <- extract_values(vars_raster,
    polygons,
    extraction_method = "centroid",
    return_type = "points"
  )
  expect_s4_class(values, "SpatVector")
})


test_that("create_training_data_from_points default params", {
  positive_points <- sample_points(10,
    analysis_region_mask,
    buffer = FALSE
  )

  training_data <- create_training_data_from_points(
    positive_points = positive_points,
    predictors_raster = vars_raster,
    analysis_region_mask = analysis_region_mask
  )

  expect_setequal(names(training_data), c(names(vars_raster), "class"))
  expect_equal(nrow(training_data), 2 * nrow(positive_points))
})

test_that("create_training_data_from_polygons default params", {
  wetlands <- test_polygons[sample(length(test_polygons), 3, replace = FALSE)]
  data <- create_training_data_from_polygons(
    polygons = wetlands,
    analysis_region = analysis_region_polygon,
    predictors_raster = vars_raster
  )
  expect_setequal(unique(data$class), c("positive", "negative"))
})

test_that("create_training_data_from_polygons sample rate", {
  wetlands <- test_polygons[sample(length(test_polygons), 3, replace = FALSE)]
  data <- create_training_data_from_polygons(
    polygons = wetlands,
    analysis_region = analysis_region_polygon,
    predictors_raster = vars_raster,
    sample_rate = 0.5
  )

  # Get expected count. First have to determine allowable region
  region <- all(vars_raster)
  region <- terra::crop(region, analysis_region_polygon)
  expected_count <- terra::expanse(region) / (1000 * 1000) * 0.5
  # Leave room for some loss due to NA
  expect_gt(nrow(data), expected_count - expected_count * 0.1)
  expect_lte(nrow(data), expected_count + expected_count * 0.1)
})

test_that("create_training_data_with_buffer default parameters", {
  positive_points <- sample_points(5,
    analysis_region_mask,
    buffer = FALSE
  )

  data <- create_training_data_with_buffer(positive_points = positive_points,
    predictors_raster = vars_raster,
    pos_buffer = 300,
    neg_buffer = 1000,
    analysis_region_mask = analysis_region_mask)

  expect_setequal(unique(data$class), c("positive", "negative"))
})
