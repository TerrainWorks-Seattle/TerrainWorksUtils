library(TerrainWorksUtils)
library(testthat)



analysis_region <- terra::rast(system.file("ex/elev.tif", package = "terra"))
analysis_region_mask <- terra::deepcopy(analysis_region)
terra::values(analysis_region_mask) <- ifelse(terra::values(analysis_region_mask) < 300,
  NA,
  terra::values(analysis_region_mask)
)

test_polygons <- terra::vect(system.file("ex/lux.shp", package = "terra"))
analysis_region_polygon <- terra::convHull(test_polygons)


elev_raster <- analysis_region
rand_raster <- terra::deepcopy(analysis_region)
terra::values(rand_raster) <- rnorm(length(terra::values(rand_raster)))

inverseelev_raster <- terra::deepcopy(analysis_region)
terra::values(inverseelev_raster) <- max(terra::values(elev_raster), na.rm = TRUE) - terra::values(elev_raster)

vars_raster <- c(
  elev_raster,
  rand_raster,
  inverseelev_raster
)
names(vars_raster) <- c("elevation", "random", "inverse")
