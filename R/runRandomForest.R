#' run random forest
#'
#' @param workingDir Working directory where model files can be found and output files will be saved in
#' @param modelFile Filename of the model
#' @param referenceRasterFile  Raster to use as a grid reference
#' @param inputRasterFiles  List of input raster filenames
#' @param inputPolygonFiles  List of input polygon filenames
#' @param testPointsFile  Filename of point feature classified by wetland type
#' @param classFieldName  Name of the class field in the test dataset
#' @param wetlandClass Class name for wetlands
#' @param nonwetlandClass  Class name for non-wetlands
#' @param calcStats Calculate ROC statistics for the built model?
#' @param probRasterName Filename of the generated wetland probability raster

runRandomForest <- function(workingDir,
                            modelFile,
                            referenceRasterFile,
                            inputRasterFiles,
                            inputPolygonFiles,
                            testPointsFile,
                            classFieldName,
                            wetlandClass,
                            nonwetlandClass,
                            calcStats,
                            probRasterName) {

  # Define helper functions ----------------------------------------------------

  baseFilename <- function(file) {
    if (is.null(file)) {
      return(NULL)
    }
    return(gsub(basename(file), pattern = "\\..*$", replacement = ""))
  }

  # Setup ----------------------------------------------------------------------

  setwd(workingDir)

  modelName <- baseFilename(modelFile)

  # Set up logging
  logFilename <- paste0(modelName, "_run.log")
  file.create(logFilename)

  log_msg("workingDir: ", workingDir)
  log_msg("modelFile: ", modelFile)
  log_msg("inputRasterFiles: ", paste0(inputRasterFiles, collapse = "; "))
  log_msg("inputPolygonFiles: ", paste0(inputPolygonFiles, collapes = "; "))
  log_msg("testPointsFile: ", testPointsFile)
  log_msg("classFieldName: ", classFieldName)
  log_msg("wetlandClass: ", wetlandClass)
  log_msg("nonwetlandClass: ", nonwetlandClass)
  log_msg("calcStats: ", calcStats)
  log_msg("probRasterName: ", probRasterName)

  # Validate parameters --------------------------------------------------------

  # Make sure reference raster file exists
  if (!file.exists(referenceRasterFile)) {
    log_err("Could not find reference raster: '", referenceRasterFile, "'")
  }

  # Make sure model file exists
  if (!file.exists(modelFile)) {
    log_err("Could not find model file: '", modelFile)
  }

  # Make sure at least one input raster or polygon was given
  if (length(inputRasterFiles) == 0 && length(inputPolygonFiles) == 0) {
    log_err("Must provide at least one input raster or polygon")
  }

  # Make sure all input raster files exist
  lapply(inputRasterFiles, function(inputRasterFile) {
    if (!file.exists(inputRasterFile)) {
      log_err("Could not find input raster: '", inputRasterFile)
    }
  })

  # Make sure test points file exists
  if (!file.exists(testPointsFile)) {
    log_err("Could not find test points dataset: '", testPointsFile)
  }

  # Load model -----------------------------------------------------------------

  # Load the "modelInfo" object
  load(modelFile)
  rfModel <- modelInfo$model

  # Load reference raster ------------------------------------------------------

  referenceRaster <- terra::rast(referenceRasterFile)

  # Load input variables -------------------------------------------------------

  log_msg("Preparing inputs...")
  # NOTE: Polygon rasterization must occur BEFORE aligning rasters. Otherwise
  # a bug sometimes appears which assigns the reference raster name and
  # variable type to the rasterized polygon values.
  # Ex:
  # working directory:      C:/Work/netmapdata/Mashel
  # reference raster:       elev_mashel.flt
  # input rasters:          grad_15.tif, plan_15.tif
  # input polygon:          lithology.shp
  # output polygon raster:  non-factor raster with name "elev_mashel"

  # Load input polygons
  polygonList <- lapply(inputPolygonFiles, function(file) terra::vect(file))

  # Rasterize each polygon
  polygonRasterList <- list()
  for (polygon in polygonList) {
    polygon <- terra::project(polygon, referenceRaster)
    for (i in seq_along(names(polygon))) {
      varName <- names(polygon)[i]
      raster <- terra::rasterize(polygon, referenceRaster, field = varName)
      polygonRasterList[[i]] <- raster
    }
  }

  # Load rasters
  rasterList <- lapply(inputRasterFiles, function(file) terra::rast(file))


  # Make sure the model has been given all its expected input variables
  expectedInputVars <- names(modelInfo$inputVars)
  givenInputVars <- sapply(c(polygonRasterList, rasterList), names)

  if (!setequal(expectedInputVars, givenInputVars)) {
    log_err(paste0(
      "Input variables (",
      paste0(givenInputVars, collapse = ", "),
      ") do not match those expected by the model (",
      paste0(expectedInputVars, collapse = ", "), ")\n"
    ))
  }

  log_msg("Aligning rasters...")
  # Align rasters with the reference raster
  rasterList <- TerrainWorksUtils::alignRasters(referenceRaster, rasterList)
  log_msg("Done aligning rasters")

  # Add rasterized polygons to raster list
  rasterList <- c(polygonRasterList, rasterList)

  # Make sure factor rasters are factored correctly
  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    if (terra::is.factor(raster)) {
      if (ncol(terra::cats(raster)[[1]]) > 2) {
        raster <- TerrainWorksUtils::fixFactorRaster(raster)
      }
      expectedCats <- modelInfo$inputVars[[names(raster)]]$cats
      rasterList[[i]] <- TerrainWorksUtils::applyCats(raster, expectedCats)
    }
  }

  # Build test dataset ---------------------------------------------------------

  log_msg("Preparing test data...")
  # Load the test points
  testPoints <- terra::vect(testPointsFile)

  # Define the test dataset. This will store all the predictor variables as
  # well as the response variable (wetland/non-wetland class)
  testDf <- data.frame(
    class = terra::values(testPoints)[[classFieldName]],
    stringsAsFactors = TRUE
  )

  # Add predictor variables from input rasters
  for (raster in rasterList) {
    rasterValues <- TerrainWorksUtils::extractRasterValues(raster, testPoints)
    testDf <- cbind(testDf, rasterValues)
  }

  # Apply expected levels to factor variables
  for (varName in names(testDf)) {
    if (varName != "class" && is.factor(testDf[[varName]])) {
      testDf[[varName]] <- factor(testDf[[varName]], levels = modelInfo$inputVars[[varName]]$levels)
    }
  }

  # Remove points with NA values
  testDf <- na.omit(testDf)

  # Make sure there are at least some points classified with the given
  # wetland/non-wetland class names
  correctlyLabeledPointIndices <- testDf$class == wetlandClass | testDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0) {
    log_err(paste0(
      "No points in the dataset with the given
                      wetland/non-wetland classes: '", wetlandClass, "'/'",
      nonwetlandClass, "'."
    ))
  }

  # Remove points that aren't labeled wetland/non-wetland
  testDf <- testDf[correctlyLabeledPointIndices, ]

  # Remove unused class levels
  testDf$class <- droplevels(testDf$class)

  log_msg(
    "Ground-truth classifications:\n",
    paste0(capture.output(summary(testDf$class)), collapse = "\n")
  )

  # Predict classes with model -------------------------------------------------

  log_msg("Predicting classes using model...")
  # Run model on test dataset
  testDataPredictions <- predict(
    rfModel,
    type = "response",
    newdata = testDf
  )

  # Log confusion matrix for test data class predictions
  capture.output(table(testDataPredictions, testDf$class), file = logFilename, append = TRUE)

  # Calculate ROC statistics ---------------------------------------------------

  if (calcStats) {

    # Calculate wetland probability for each test point
    wetProb <- predict(
      rfModel,
      type = "prob",
      newdata = testDf
    )[, wetlandClass]

    # Calculate ROC stats
    rocStats <- TerrainWorksUtils::calcRocStats(
      classes = testDf$class,
      probs = wetProb,
      wetlandClass,
      nonwetlandClass
    )

    # Log ROC statistics
    cat(paste0("AUROC: ", rocStats$auc@y.values, "\n"), file = logFilename, append = TRUE)
    capture.output(c(PRBE = rocStats$prbe, cutoff = rocStats$maxPrecisionCutoff), file = logFilename, append = TRUE)
    capture.output(c(accuracy = rocStats$maxAccuracy, cutoff = rocStats$maxAccuracyCutoff), file = logFilename, append = TRUE)

    # Display ROC plot
    dev.new()
    ROCR::plot(rocStats$roc, colorize = TRUE, main = paste0(modelName, "_roc"))
    abline(a = 0, b = 1, lty = 2)
    dev.copy(win.metafile, paste0(modelName, "_roc.wmf"))
    dev.off()

    # Display precision-recall plot
    dev.new()
    ROCR::plot(rocStats$precision, colorize = TRUE, main = paste0(modelName, "_prc"))
    dev.copy(win.metafile, paste0(modelName, "_prc.wmf"))
    dev.off()

    # Display accuracy plot
    dev.new()
    ROCR::plot(rocStats$accuracy, main = paste0(modelName, "_acc"))
    dev.copy(win.metafile, paste0(modelName, "_acc.wmf"))
    dev.off()
  }

  # Generate probability raster ------------------------------------------------

  if (!is.null(probRasterName) && !is.na(probRasterName)) {

    # Combine individual input rasters into a single multi-layered raster
    inputRaster <- rasterList[[1]]
    if (length(rasterList) > 1) {
      for (i in 2:length(rasterList)) {
        inputRaster <- c(inputRaster, rasterList[[i]])
      }
    }

    # Generate probability rasters for wetland and non-wetland
    probRaster <- terra::predict(
      inputRaster,
      rfModel,
      na.rm = TRUE,
      type = "prob"
    )

    # Save the wetland probability raster
    wetProbRaster <- probRaster[[wetlandClass]]
    terra::writeRaster(
      wetProbRaster,
      filename = paste0(probRasterName, ".tif"),
      overwrite = TRUE
    )

    log_msg("Created probability raster: ", probRasterName, ".tif")
  }
}
