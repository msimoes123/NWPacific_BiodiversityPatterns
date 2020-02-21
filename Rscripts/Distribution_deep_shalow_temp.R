library(raster)

setwd("D:\\Black_HD\\nw_shallow\\Variables")
mt <- raster("set1/2100_rcp85/temp.asc")
plot(mt)

setwd("D:\\Black_HD\\MODELS_Ready\\Excluded_files_folders\\Excluded_nw_arctic_deep\\Variables")
dmt <- raster("set1/2100_rcp85/temp.asc")
plot(dmt)

smt <- rasterToPoints(mt)

smt <- smt[sample(nrow(smt), 10000), ]
ddmt <- extract(dmt, smt[, 1:2])


plot(smt[, 2], smt[, 3], col = "transparent")
lines(loess(smt[, 3] ~ smt[, 2]), col = "purple")


points(smt[, 2], ddmt, col = "blue")
loess(smt[, 3] ~ smt[, 2], smt, span=0.25)

data2 <- rbind(data, data1)

ggplot(data2, aes(x=Latitude, y = Temperature, color=ID, fill=ID)) + 
  geom_smooth(method = "loess", se = FALSE)
