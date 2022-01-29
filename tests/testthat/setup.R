library(TerrainWorksUtils)
library(testthat)



analysisRegion <- terra::rast(system.file("ex/elev.tif", package = "terra"))
analysisRegionMask <- terra::deepcopy(analysisRegion)
terra::values(analysisRegionMask) <- ifelse(terra::values(analysisRegionMask) < 500,
  NA,
  terra::values(analysisRegionMask)
)



elevRaster <- analysisRegion
randRaster <- terra::deepcopy(analysisRegion)
terra::values(randRaster) <- rnorm(length(terra::values(randRaster)))

inverseElevRaster <- terra::deepcopy(analysisRegion)
terra::values(inverseElevRaster) <- max(terra::values(elevRaster), na.rm = TRUE) - terra::values(elevRaster)

varsRaster <- c(
  elevRaster,
  randRaster,
  inverseElevRaster
)
names(varsRaster) <- c("elevation", "random", "inverse")
