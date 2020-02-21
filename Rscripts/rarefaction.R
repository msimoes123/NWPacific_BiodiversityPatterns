install.packages('vegan', force=T)
library(vegan)
setwd('C:\\Users\\adminz\Desktop\\Hani_databasePaper\\rarefaction')
x <- read.csv("Deep_Eco-Spp-Samples.csv",header=T,sep=",")
x2 <- read.csv("shallow_Eco-Spp-Samples.csv",header=T,sep=",")
MyData1 <- rbind(x,x2)
write.csv(x3, 'all_eco_spp_samples.csv', row.names = F)


# Convert the values in a column into row names
MyData2 <- read.csv("C:\\Users\\admin\\Desktop\\Database_paper\\Rarefaction\\all_eco_spp_samples_pivt2.csv",header=T,sep=",") 
data <- MyData2+100 #add 100 to the cells taht are numbers already
data[is.na(data)] <- 100 #replace Nas
write.csv(data, 'C:\\Users\\admin\\Desktop\\Database_paper\\Rarefaction\\test_data.csv', row.names = F)
# Number of INDIVIDULS per site (?)
raremax <-(rowSums(data))  

subsample = 50

rare_richness <- rarefy(data, subsample,se=F, MARGIN=1) # Margin for which the index is computed.
#Diversity indexes 
diversity <- diversity(data)
simp_div <- diversity(data, "simpson") #simpson
invsimp_div <- diversity(data, "inv") #

#total number of species at each site (row of data)
S <- specnumber(data)
View(S)

# rarefy, w/ raremax as input (?)
Srare <- rarefy(MyData2, 50)


#Plot rarefaction results
par(mfrow = c(1,2))
plot(S, Srare, xlab = "Observed No. of Species", 
     ylab = "Rarefied No. of Species",
     main = " plot(rarefy(MyData, raremax))")
abline(0, 1)
rarecurve(MyData, step = 20, 
          sample = raremax, 
          col = "blue", 
          cex = 0.6,
          main = "rarecurve()")