#GBIF
install.packages(c('dismo','maptools', 'rgbif', 'ropensci', 'robis'))
install.packages("devtools")
install.packages("rworldmap")
install.packages("ggplot2")
install.packages("ggmap")
devtools::install_github("iobis/robis")
library(dismo)
library(robis)
library(maptools)
library(rgbif)
library(plyr)
library(XML)
library(httr)
library(rworldmap)
library(ggplot2)
library(ggmap)



#GBIF------
x2<-read.csv(file = "C:\\Users\\admin\\Desktop\\Beneficial_prj\\dataset\\Isop_Benficial_sp.csv", header = TRUE, sep = ",") #head(x2)
splist <- as.character((x2[,c(2)]))
#splist <- c('Cyanocitta stelleri', 'Junco hyemalis', 'Aix sponsa')
keys <- sapply(splist, function(x) name_suggest(x)$key[1], USE.NAMES=FALSE)
Iso_occ <- occ_search(taxonKey=keys) 
write.csv(Iso_occ, file = "Iso_occ.csv")

#OBIS-------

data <- occurrence("Bopyroides hippolytes", fields = c( "decimalLatitude","decimalLongitude"))
write.csv(data, file = "Mirabilicoxa kussakini.csv")

#Plot data-------

library(rworldmap)
library(ggplot2)
library(ggmap)

newmap <- getMap(resolution = "low")
plot(newmap)
points(df$Long, df$Lat, col = "red", cex = .6) #occ pts

data(wrld_simpl)
plot(wrld_simpl, axes=TRUE, col='light green', las=1) #plot points on a world map
zoom(wrld_simpl, axes=TRUE, las=1, col='light green') #zoom to a specific region
points(df, col='orange', pch=20, cex=0.75)




# creating a sample data.frame with your lat/lon points
lon <- c(-38.31,-35.5)
lat <- c(40.96, 37.5)
df <- as.data.frame(cbind(lon,lat))

df <- as.data.frame(read.csv(file = "C:\\Users\\admin\\Desktop\\Beneficial_prj\\dataset\\CSV\\B_hippolytes.csv", header = TRUE, sep = ",")) #head(x2)

# getting the map
mapgilbert <- get_map(zoom = 10, maptype = "hybrid", scale = 4)

# plotting the map with some points on it
  ggmap(mapgilbert) +
  geom_point(data = df, aes(x = Long, y = Lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
