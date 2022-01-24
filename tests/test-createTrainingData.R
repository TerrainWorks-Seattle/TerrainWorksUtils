test_that("sampleNegativePoints", {
  analysisRegion <- terra::rast(system.file("ex/elev.tif", package="terra"))
  positivePoints <- samplePoints(20, analysisRegion)

})
