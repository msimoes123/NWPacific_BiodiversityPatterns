setwd('C:\\Users\\admin\\Desktop\\')
x <- read.csv('Complete_Data_Isopods.csv')
sp <- unique(x$scientific) #str(sp)

n = 5 #number of records 
cols = 2:4 # columns extracted from dataset - species, long, lat
for (i in 1:length(sp)) {
  sptable <- x[x$scientific == sp[i], cols]
    if (dim(sptable)[1] >= n) {
    write.csv(sptable, paste(sp[i], ".csv", sep = ""), row.names = FALSE)
  }
}