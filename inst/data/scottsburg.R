library(terra)

elev <- rast("E:/NetmapData/Scottsburg/elev_scottsburg.flt")
elev_agg <- aggregate(elev, fact = 10)
writeRaster(elev_agg, filename = "data-cache/elev_scottsburg.tif")
rm(elev)
rm(elev_agg)

grad <- rast("E:/NetmapData/Scottsburg/grad_15.tif")
grad_agg <- aggregate(grad, fact = 10)
writeRaster(grad_agg, "data-cache/grad_15.tif")
rm(grad)

dev <- rast("E:/Netmapdata/Scottsburg/dev_15.tif")
dev_agg <- aggregate(dev, fact = 10)
writeRaster(dev_agg, "data-cache/dev_15.tif")
rm(dev)

prof <- rast("E:/NetmapData/Scottsburg/prof_15.tif")
prof_agg <- aggregate(prof, fact = 10)
writeRaster(prof_agg, "data-cache/prof_15.tif")
rm(prof)

plan <- rast("E:/NetmapData/Scottsburg/plan_15.tif")
plan_agg <- aggregate(plan, fact = 10)
writeRaster(plan_agg, "data-cache/plan_15.tif")
rm(plan)

pca_15m_48hr <- rast("E:/NetmapData/Scottsburg/pca_15m_48hr.flt")
pca_agg <- aggregate(pca_15m_48hr, fact = 10)
writeRaster(pca_agg, "data-cache/pca_15m_48hr.tif")

points <- vect("E:/NetmapData/Scottsburg/Scottsburg_Upslope.shp")
writeVector(points, "data-cache/initiation_points.shp")
