library(testthat)
library(TerrainWorksUtils)

elev <- terra::rast("data-cache/elev_scottsburg.tif")
grad <- terra::rast("data-cache/grad_15.tif")
plan <- terra::rast("data-cache/plan_15.tif")
prof <- terra::rast("data-cache/prof_15.tif")
dev <- terra::rast("data-cache/dev_15.tif")
pca <- terra::rast("data-cache/pca_15m_48hr.tif")

varsRaster <- c(
  grad,
  plan,
  prof,
  dev,
  pca
)

points <- terra::vect("data-cache/initiation_points.shp")
initRange <- extractRange(varsRaster, points)

analysisRegionMask <- terra::deepcopy(elev)
terra::values(analysisRegionMask) <-
  ifelse((
    terra::values(grad) < initRange["grad_15", "min"] |
      terra::values(grad) > initRange["grad_15", "max"]
  ) | (
    terra::values(plan) < initRange["plan_15", "min"] |
      terra::values(plan) > initRange["plan_15", "max"]
  ) | (
    terra::values(prof) < initRange["prof_15", "min"] |
      terra::values(prof) > initRange["prof_15", "max"]
  ),
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
