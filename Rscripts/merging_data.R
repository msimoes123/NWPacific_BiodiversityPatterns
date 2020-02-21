#Merging Species data--------------------
#Subsetting name, longitude and Latitude from each file .csv
setwd("C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records")
path <- "C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records\\"
outpath <-"C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records\\subsets\\" 
filenames <- list.files(path= path, pattern = ".csv", full.names=TRUE)
spvector <- filenames
spvec <- dir()
exclude <- c("trial", "trial2", 'subsets')
spvector <- spvec[!spvec %in% exclude]

i=1
for (i in 1: length(spvector)) {
  all <- read.csv(paste(spvector[i], sep = ","))
  myvars <- c("name", "decimalLongitude", "decimalLatitude") #gotta change the names_molacostraca has differnt names
  table <- all[myvars]
  write.csv(table, paste0(outpath, paste(spvector[i], ".csv", sep = "")),row.names = FALSE)
}  

#Merging all -------------------

library(dplyr)
library(readr)
library(rgdal)
path <- "C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records\\subsets\\"
filenames <- list.files(path= path, pattern = ".csv", full.names=TRUE) #list csv files
tables1 <- lapply(filenames, read.csv, header = TRUE) #combining tables 
combined.df <- do.call(rbind , tables1) #
write.csv(combined.df, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records\\subsets\\Total_Occ.csv', row.names=F)
x<- read.csv('C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_records\\subsets\\Total_Occ.csv', header = T)
head(x)
deu_pts<- x[with(x, ((decimalLongitude >=7  & decimalLongitude <= 15) | (decimalLatitude >= 47 & decimalLatitude <= 55))), ]
deu1 <- deu_pts[1:500000,]
write.csv(deu1, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu1.csv', row.names = F)
deu2 <- deu_pts[500001:1000000,]
write.csv(deu2, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu2.csv', row.names = F)
deu3 <- deu_pts[1000001:1500000,]
write.csv(deu3, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu3.csv', row.names = F)
deu4<- deu_pts[1500001:2000000,]
write.csv(deu4, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu4.csv', row.names = F)
deu5 <- deu_pts[2000001:2500000,]
write.csv(deu5, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu5.csv', row.names = F)
deu6 <- deu_pts[2500001:3000000,]
write.csv(deu6, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu6.csv', row.names = F)
deu7 <- deu_pts[3500001:4000000,]
write.csv(deu7, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu7.csv', row.names = F)
deu8 <- deu_pts[4000001:4500000,]
write.csv(deu8, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu8.csv', row.names = F)
deu9 <- deu_pts[4500001:5000000,]
write.csv(deu9, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu9.csv', row.names = F)
deu10 <- deu_pts[4500001:5000000,]
write.csv(deu10, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu10.csv', row.names = F)
deu11 <- deu_pts[5000001:5500000,]
write.csv(deu11, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu11.csv', row.names = F)
deu12 <- deu_pts[5500001:6500000,]
write.csv(deu12, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_Germany\\deu12.csv', row.names = F)
#Ignore this-- the date was too large to proceed on it--------------
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(7, 15), ylim = c(47, 55), asp = 1)
points(deu_pts$decimalLongitude, deu_pts$decimalLatitude, col = "red", cex = .6)
library(rgdal)
setwd("C:\\Users\\admin\\Desktop\\Community_insects\\Shapes")
Germany<-readOGR(".","DEU_adm0") #reading germany shapefile
plot(Germany)
pts<-readOGR(dsn="overpass-turbo/shp/",layer="amenity")
UTMcoor<-read.csv(file="https://dl.dropboxusercontent.com/u/549234/s1.csv")
coordinates(UTMcoor)=~X+Y
proj4string(UTMcoor)=CRS("++proj=utm +zone=48") # set it to UTM
LLcoor<-spTransform(UTMcoor,CRS("+proj=longlat")) #set it to Lat Long
plot(LLcoor)
points(LLcoor$X,LLcoor$Y,pch=19,col="blue",cex=0.8) #to test if coordinate can be plot as point map
writeOGR(UTMcoor, dsn="c:/todel" ,layer="tsb",driver="ESRI Shapefile")
writeSpatialShape("LLcoor","test")
pts_in<-pts[!is.na(over(pts,poly)),]


#merging data germany only----
setwd("C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only")
path <- "C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\"
outpath <-"C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\final\\" 
filenames <- list.files(path= path, pattern = ".csv", full.names=TRUE)
spvector <- filenames
spvec <- dir()
exclude <- c("trial", "trial2", 'subsets', 'final')
spvector <- spvec[!spvec %in% exclude]

i=1
for (i in 1: length(spvector)) {
  all <- read.csv(paste(spvector[i], sep = ","))
  myvars <- c('NAME_0', 'NAME_1', 'NAME_2','NAME_3','ENGTYPE_3', "name", "decimalLon", "decimalLat") #gotta change the names_molacostraca has differnt names
  table <- all[myvars]
  write.csv(table, paste0(outpath, paste(spvector[i], ".csv", sep = "")),row.names = FALSE)
}  

path <- "C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\final\\"
filenames <- list.files(path= path, pattern = ".csv", full.names=TRUE) #list csv files
tables1 <- lapply(filenames, read.csv, header = TRUE) #combining tables 
combined.df <- do.call(rbind , tables1) #
write.csv(combined.df, 'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\Total_Germany.csv', row.names=F)

#replacing 1s in matrix
x<- read.csv('C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\germany_beta.csv', header = T)
x[x > 1] <- 1
write.csv(x,'C:\\Users\\admin\\Desktop\\Community_insects\\Results\\Species_germany_only\\germany_beta_binary.csv', row.names=F)
