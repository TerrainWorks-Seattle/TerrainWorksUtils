

#' Build Random Forest
#'
#' @param referenceRaster Raster to use as a grid reference
#' @param inputRasterList list of input rasters
#' @param inputPolygonList list of input polygons
#' @param trainingPoints Point feature classified by wetland type
#' @param classFieldName Name of the class field in the training dataset
#' @param wetlandClass Class name for wetlands
#' @param nonwetlandClass class name for non-wetlands
#' @param modelName Name for the random forest model
#' @param calcStats Calculate ROC statistics for the built model?
#' @param probRasterName Filename of the generated wetland probability raster
buildRandomForest <- function(referenceRaster,
                              inputRasterList,
                              inputPolygonList = list(),
                              trainingPoints,
                              classFieldName,
                              wetlandClass,
                              nonwetlandClass,
                              modelName,
                              calcStats = FALSE,
                              plotDir = NULL) {

  # Validate parameters --------------------------------------------------------

  if (!class(referenceRaster) == "SpatRaster") stop("referenceRaster must be SpatRaster")

  # Make sure at least one input raster or polygon was given
  if (length(inputRasterList) == 0 && length(inputPolygonList) == 0) {
    log_err("Must provide at least one input raster or polygon")
  }

  # Load input variables -------------------------------------------------------

  # NOTE: Polygon rasterization must occur BEFORE aligning rasters. Otherwise
  # a bug sometimes appears which assigns the reference raster name and
  # variable type to the rasterized polygon values.
  # Ex:
  # working directory:      C:/Work/netmapdata/Mashel
  # reference raster:       elev_mashel.flt
  # input rasters:          grad_15.tif, plan_15.tif
  # input polygon:          lithology.shp
  # output polygon raster:  non-factor raster with name "elev_mashel"

  # Rasterize each input polygon
  polygonRasterList <- list()
  for (polygon in inputPolygonList) {
    polygon <- terra::project(polygon, referenceRaster)
    for (varName in names(polygon)) {
      raster <- terra::rasterize(polygon, referenceRaster, field = varName)
      polygonRasterList <- c(polygonRasterList, raster)
    }
  }

  # Load input rasters

  log_msg("aligning rasters...")
  # Align rasters with the reference raster
  rasterList <- TerrainWorksUtils::alignRasters(
    referenceRaster,
    inputRasterList
  )

  # Make sure factor rasters are factored correctly (use character level names
  # rather than numeric level names)
  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    if (terra::is.factor(raster)) {
      rasterList[[i]] <- TerrainWorksUtils::fixFactorRaster(raster)
    }
  }

  rasterList <- c(rasterList, polygonRasterList)

  # Record input variable metadata ---------------------------------------------

  # A place to store metadata
  inputVars <- list()

  # Record raster input variables
  for (raster in rasterList) {
    for (varName in names(raster)) {
      layer <- raster[[varName]]
      if (terra::is.factor(layer)) {
        inputVars[[varName]] <- list(
          levels = terra::levels(layer)[[1]],
          cats = terra::cats(layer)[[1]]
        )
      } else {
        inputVars[[varName]] <- NA
      }
    }
  }

  # Build training dataset -----------------------------------------------------

  # Define the training dataset. This will store all the predictor variables as
  # well as the response class variable
  trainingDf <- data.frame(
    class = terra::values(trainingPoints)[[classFieldName]],
    stringsAsFactors = TRUE
  )

  # Add input raster variables
  for (raster in rasterList) {
    rasterValues <- TerrainWorksUtils::extractRasterValues(raster, trainingPoints)
    trainingDf <- cbind(trainingDf, rasterValues)
  }

  # Apply original levels to factor variables
  for (varName in names(trainingDf)) {
    if (varName != "class" && is.factor(trainingDf[[varName]])) {
      trainingDf[[varName]] <- factor(trainingDf[[varName]], levels = inputVars[[varName]]$levels)
    }
  }

  # Remove training data with NA values
  trainingDf <- na.omit(trainingDf)

  # Check that at least some training data are classified with the given
  # wetland/non-wetland class names
  correctlyLabeledPointIndices <- trainingDf$class == wetlandClass | trainingDf$class == nonwetlandClass
  if (sum(correctlyLabeledPointIndices) == 0) {
    logAndStop(paste0(
      "No entries in the training dataset have the given
                     wetland/non-wetland classes: '", wetlandClass, "'/'",
      nonwetlandClass, "'."
    ))
  }

  # Remove training data that aren't classified with the given
  # wetland/non-wetland class names
  trainingDf <- trainingDf[correctlyLabeledPointIndices, ]

  # Remove unused class levels
  trainingDf$class <- droplevels(trainingDf$class)

  log_msg("Ground-truth classifications:")
  log_msg("\n", paste0(capture.output(summary(trainingDf$class)), collapse = "\n"))

  # Build Random Forest model --------------------------------------------------

  # Build a model that predicts wetland/non-wetland class from all input variables
  rfModel <- randomForest::randomForest(
    formula = class ~ .,
    data = trainingDf,
    ntree = 200,
    importance = TRUE
  )

  rfModelInfo <- list(
    rfModel = rfModel,
    inputVars = inputVars
  )

  if (calcStats) {
    writeModelStats(rfModelInfo,
      trainingDf,
      modelName,
      plotDir = plotDir
    )
  }

  probRaster <- generateWetlandProbabilityRaster(
    rasterList,
    rfModel
  )
}



#' display model stats
#'
#' @param rfModelInfo list with random forest model and metadata about inputs,
#' as returned by buildRandomForest()
writeModelStats <- function(rfModelInfo,
                            trainingDf,
                            modelName = "",
                            plotDir = NULL) {
  log_msg(" ----- Model statistics ----- ")
  # Log model information
  log_obj(rfModelInfo)
  rfModel <- rfModelInfo$rfModel
  log_obj(randomForest::importance(rfModel))

  # Plot model statistics ----------------------------------------------------

  # Display model error rates plot
  errorRateNames <- colnames(rfModel$err.rate)
  if (!interactive()) dev.new()
  plot(rfModel, main = paste0(modelName, "_rfclass"))
  legend("topright", errorRateNames, col = seq_along(errorRateNames), cex = 0.8, fill = seq_along(errorRateNames))
  if (!is.null(plotDir)) {
    dev.copy(jpeg, file.path(plotDir, paste0(modelName, "_rfclass.jpg")))
  }

  # Display model variable importance plot (if multiple variables were given)
  varCount <- length(rfModelInfo$inputVars)
  if (varCount > 1) {
    if (!interactive()) dev.new()
    randomForest::varImpPlot(rfModel, sort = TRUE, main = paste0(modelName, "_importance"))
    if (!is.null(plotDir)) {
      dev.copy(jpeg, file.path(plotDir, paste0(modelName, "_importance.jpg")))
    }
  }

  # Calculate ROC statistics ---------------------------------------------------


  testDf <- trainingDf

  # Calculate wetland probability for each test dataset entry
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
  log_msg("AUROC: ", rocStats$auc@y.values)
  log_obj(c(PRBE = rocStats$prbe, cutoff = rocStats$maxPrecisionCutoff))
  log_obj(c(accuracy = rocStats$maxAccuracy, cutoff = rocStats$maxAccuracyCutoff))

  # Display ROC plot
  if (!interactive()) dev.new()
  ROCR::plot(rocStats$roc, colorize = TRUE, main = paste0(modelName, "_roc"))
  abline(a = 0, b = 1, lty = 2)
  if (!is.null(plotDir)) {
    dev.copy(jpeg, paste0(modelName, "_roc.jpg"))
  }

  # Display precision-recall plot
  if (!interactive()) dev.new()
  ROCR::plot(rocStats$precision, colorize = TRUE, main = paste0(modelName, "_prc"))
  if (!is.null(plotDir)) {
    dev.copy(jpeg, paste0(modelName, "_prc.jpg"))
  }

  # Display accuracy plot
  if (!interactive()) dev.new()
  ROCR::plot(rocStats$accuracy, main = paste0(modelName, "_acc"))
  if (!is.null(plotDir)) {
    dev.copy(jpeg, paste0(modelName, "_acc.jpg"))
  }
}

generateWetlandProbabilityRaster <- function(rasterList,
                                             rfModel,
                                             wetlandClass) {
  # Generate wetland probability raster ----------------------------------------

  # Combine individual input rasters into a single multi-layered raster
  inputRaster <- Reduce(c, rasterList)

  # Generate probability rasters for wetland and non-wetland
  probRaster <- terra::predict(
    inputRaster,
    rfModel,
    na.rm = TRUE,
    type = "prob"
  )

  return(probRaster[[wetlandClass]])
}
