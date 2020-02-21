#Diversity graphs database paper
library('dplyr')
x <- read.csv("C:\\Users\\admin\\Desktop\\Corrections\\Processing\\Banda Sea\\beta.csv",row.names=1,check.names=FALSE)
as.data.frame(x)
length(x$phylum)
Chordata<- x %>% filter(phylum == 'Chordata')
dim(Chordata)
Arthropoda<- x %>% filter(phylum == 'Arthropoda')
dim(Arthropoda)
Mollusca<- x %>% filter(phylum == 'Mollusca')
dim(Mollusca)
Echinodermata<- x %>% filter(phylum == 'Echinodermata')
dim(Echinodermata)
Cnidaria<- x %>% filter(phylum == 'Cnidaria')
dim(Cnidaria)
Annelida<- x %>% filter(phylum == 'Annelida')
dim(Annelida)

#Benthic and pelagic
#
#
#
d <- read.csv("C:\\Users\\admin\\Desktop\\Corrections\\Processing\\Yellow Sea\\beta.csv",row.names=1,check.names=FALSE)

ps <- read.csv("C:\\Users\\admin\\Desktop\\Database_paper\\Diversity_phylum\\benthic_pelagic\\pelagic_shallow.csv",check.names=FALSE)
names <-ps$Scientific_matched
final_ps <- d %>% filter(d$scientific %in% names) #occ_s= shallow unique species accourences - 
dim(final_ps)

pd <- read.csv("C:\\Users\\admin\\Desktop\\Database_paper\\Diversity_phylum\\benthic_pelagic\\pelagic_deep.csv",check.names=FALSE)
names <-pd$Scientific_matched
final_pd <- d %>% filter(d$scientific %in% names) #occ_s= shallow unique species accourences - 
dim(final_pd)

bs <- read.csv("C:\\Users\\admin\\Desktop\\Database_paper\\Diversity_phylum\\benthic_pelagic\\benthic_shallow.csv",check.names=FALSE)
names <-bs$Scientific_matched
final_bs <- d %>% filter(d$scientific %in% names) #occ_s= shallow unique species accourences - 
dim(final_bs)

bd <- read.csv("C:\\Users\\admin\\Desktop\\Database_paper\\Diversity_phylum\\benthic_pelagic\\benthic_deep.csv",check.names=FALSE)
names <-bd$Scientific_matched
final_bd <- d %>% filter(d$scientific %in% names) #occ_s= shallow unique species accourences - 
dim(final_bd)


