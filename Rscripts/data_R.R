#Make folders with csv files of species with  >30 records ----------------------------------------------------
#Note> I created another folder to extract the species and occ that beneficial dataset had _
#The merged datasets of Isopods n>20 occ, is in Ku_ENM2
setwd('C:\\Users\\admin\\Desktop\\Community_insects\\data')
x <- read.csv('data1.csv') #str(x)

sp <- unique(x$order) #str(sp)

n = 1 #number of records 
cols = 1:56 # columns extracted from dataset - species, long, lat
for (i in 1:length(sp)) {
  sptable <- x[x$order == sp[i], cols]
  if (dim(sptable)[1] >= n) {
    dir.create(paste(sp[i], collapse = "_"))
    write.csv(sptable, paste(paste(sp[i], collapse = '_'), '\\',
                             paste(sp[i], collapse = '_'), ".csv", sep = ""), row.names = FALSE)
  }
  
}

#Gbif data ------------------
##
# loading needed package
if(!require(rgbif)){
  install.packages("rgbif")
  library(rgbif)
}


# defining working directory
## project forlder
setwd("C:\\Users\\admin\\Desktop\\Community_insects\\data\\Hymenoptera\\Species") # Your folder

#x<-read.csv('NW_spp.csv')
dir()
#x1<-unique(x$scientific)
#write.csv(x1, 'NW_uniq_spp.csv')
#x2 <- read.csv('NW_uniq_spp.csv')

spvector <- as.character(read.csv("hymenoptera.csv")[, 1])# binomial names

## Getting info species by species 
occ_count <- list() # object to save info on number of georeferenced records per species 

for (i in 1:length(spvector)) {
  sps <- try(name_lookup(query = spvector[i], rank = "species", 
                         return = "data", limit = 100), silent = TRUE) # information about the species
  
  sps_class <- class(sps)
  
  # avoiding errors from GBIF (e.g., species name not in GBIF)
  if(sps_class[1] == "try-error") {
    occ_count[[i]] <- c(Species = spvector[i], keys = 0, counts = 0) # species not in GBIF
    cat("species", spvector[i], "is not in the GBIF database\n")
    
  }else {
    keys <- sps$key # all keys returned
    counts <- vector() # object to save info on number of records per key
    
    for (j in 1:length(keys)) { # testing if keys return records
      counts[j] <- occ_count(taxonKey = keys[j], georeferenced = TRUE) 
    }
    
    if (sum(counts) == 0) { # if no info, tell the species
      occ_count[[i]] <- c(Species = spvector[i], keys = "all", counts = 0) # species not in GBIF
      cat("species", spvector[i], "has no goereferenced data\n")
      
    }else { # if it has info, use the key with more records, which is the most useful
      if (length(keys) == 1) { # if it is only one key
        key <- keys # detecting species key 
        occ_count[[i]] <- cbind(spvector[i], counts) # count how many records
        
      }else { # if its more than one key
        keysco <- cbind(keys, counts)
        keysco <- keysco[order(keysco[, 2]), ]
        key <- keysco[dim(keysco)[1], 1] # detecting species key that return information
        occ_count[[i]] <- c(Species = spvector[i], keysco[dim(keysco)[1], ])# count how many records
      }
      
      occ <- try(occ_search(taxonKey = key, return = "data", limit = 10000), silent = TRUE) # getting the data from GBIF
      occ_class <- class(occ)
      
      # avoiding errors from GBIF
      while (occ_class[1] == "try-error") {
        occ <- try(occ_search(taxonKey = key, return = "data", limit = 10000), silent = TRUE) # getting the data from GBIF
        occ_class <- class(occ)
        
        if(occ_class[1] != "try-error") {
          break()
        }
      }
      
      # following steps
      occ_g <- occ
      occ_g <- occ_g[, c(1, 2, 4, 3, 5:dim(occ_g)[2])] # reordering longitude and latitude
      
      # keeping only unique georeferenced records. IF NO FILTERING IS NEEDED, PUT A # IN FRONT OF THE NEXT 3 LINES
      occ_g <- occ_g[!is.na(occ_g$decimalLatitude) & !is.na(occ_g$decimalLongitude), ] # excluding no georeferences
      occ_g <- occ_g[!duplicated(paste(occ_g$name, occ_g$decimalLatitude, # excluding duplicates
                                       occ_g$decimalLongitude, sep = "_")), ]
      
      # writting file
      file_name <- paste(gsub(" ", "_", spvector[i]), "csv", sep = ".") # csv file name per each species
      write.csv(occ_g, file_name, row.names = FALSE) # writing inside each genus folder
      
      cat(i, "of", length(spvector), "species\n") # counting species per genus 
    }
  }
}

#genus_data <- do.call(rbind, occ_count) # making the list of countings a table


#genus_data <- data.frame(genus_data[, 1], genus_data[, 2], as.numeric(genus_data[, 3])) # making countings numeric

#names(genus_data) <- c("Species", "Key", "N_records") # naming columns

# writing the table
#file_nam <- "Species_record_count.csv" # csv file name for all species
#write.csv(genus_data, file_nam, row.names = FALSE) # writing inside each genus folder

#Dont know why the othe rcommands didnt work - I ended up playing this hack and coping and transposing gluing on excel
x <- as.data.frame(occ_count)
head(x)
write.csv(x, 'test.csv', row.names = FALSE)
