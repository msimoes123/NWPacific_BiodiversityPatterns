s1 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow.csv')
s2 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2050_26.csv')
s3 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2050_85.csv')
s4 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2100_26.csv')
s5 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2100_85.csv')
library(reshape)
sh <- cbind(s2[,1], s2[,3],s3[,3],s4[,3],s5[,3])
colnames(sh) <- c('x', 'shallow1', 'shallow2', 'shallow3', 'shallow4' )
head(sh)
write.csv(sh, 'sh.csv', row.names = F)
test <- read.csv('sh.csv')
head(test)
melt_t <- melt(test, id.vars = "x", 
               measure.vars = c( 'shallow1', 'shallow2', 'shallow3', 'shallow4'))
head(melt_t)
df<-NULL
breaks <- seq(-180,180, by=5)
spvector<-unique(breaks)
time <- c('shallow1', 'shallow2', 'shallow3', 'shallow4')
i=1
#present 
df_ <- NULL
head(s1)
for (j in 1:length(spvector)) {
    sub1 <-s2[s2$x  >= spvector[j],]
    sub2 <-s2[s2$x <= spvector[j]+5,]
    m <-  sum(sub2$Shallow)
    df_ = rbind(df_, data.frame(spvector[j], 'Present', m))
  }
  
write.csv(df_,'shallow_suitability_pres.csv', row.names = F)

#4 times
i=1
j=1
head(df_)
df<-NULL
for (i in 1:length(time)) {
  sub <- subset(melt_t, variable == paste0('',time[i]))
  for (j in 1:length(spvector)) {
    sub1 <-sub[sub$x  >= spvector[j],]
    sub2 <-sub1[sub1$x <= spvector[j]+5,]
    m <-  sum(sub2$value)
   df <- rbind(df, data.frame(spvector[j], time[i], m))
  }
}  
str(df)
write.csv(df,'shallow_suitability_times.csv', row.names = F)



#Graph loss
loss <- read.csv('loss.csv')
head(loss)
melt_l <- melt(loss, id.vars = "intervals", 
               measure.vars = c( 'Loss_2050_26', 'Loss_2050_85', 'Loss_2100_26', 'Loss_2100_85'))

head(melt_l)
library(ggplot2)
ggplot(data = melt_l, aes(x = intervals, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + coord_flip() +
  theme_classic()

#LATITUDE
sh_L <- cbind(s2[,1], s2[,3],s3[,3],s4[,3],s5[,3])
colnames(sh_L) <- c('x', 'shallow1', 'shallow2', 'shallow3', 'shallow4' )
head(sh_L)
write.csv(sh_L, 'sh_latitude.csv', row.names = F)
test_ <- read.csv('sh_latitude.csv')
head(test_)
melt_t_ <- melt(test_, id.vars = "x", 
               measure.vars = c( 'shallow1', 'shallow2', 'shallow3', 'shallow4'))
head(melt_t_)

df<-NULL
breaks <- seq(-90,90, by=5)
spvector<-unique(breaks)
time <- c('shallow1', 'shallow2', 'shallow3', 'shallow4')
i=1
#present 
df_ <- NULL
min(s1$y)
j=1
for (j in 1:length(spvector)) {
  sub1 <-s1[s1$y  >= spvector[j],]
  sub2 <-s1[s1$y <= spvector[j]+5,]
  m <-  sum(sub2$Shallow)
  df_ = rbind(df_, data.frame(spvector[j], 'Present', m))
}

write.csv(df_,'shallow_suitability_pres_lat.csv', row.names = F)

#4 times
i=1
j=1
head(melt_t_)
df<-NULL
for (i in 1:length(time)) {
  sub <- subset(melt_t_, variable == paste0('',time[i]))
  for (j in 1:length(spvector)) {
    sub1 <-sub[sub$x  >= spvector[j],]
    sub2 <-sub1[sub1$x <= spvector[j]+5,]
    m <-  sum(sub2$value)
    df <- rbind(df, data.frame(spvector[j], time[i], m))
  }
}  
head(df)
write.csv(df,'shallow_suitability_times_lat.csv', row.names = F)



#Graph loss
loss <- read.csv('loss_lat.csv')
head(loss)
melt_l <- melt(loss, id.vars = "intevals", 
               measure.vars = c( 'X2050_26', 'X2050_85', 'X2100_26', 'X2100_85'))

head(melt_l)
library(ggplot2)
ggplot(data = melt_l, aes(x = intevals, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + coord_flip() +
  theme_classic()

#-------------------
s1 <- read.csv('D:\\MODELS_Ready\\Histograms\\deep.csv')
s2 <- read.csv('D:\\MODELS_Ready\\Histograms\\deep_2050_26.csv')
s3 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2050_85.csv')
s4 <- read.csv('D:\\MODELS_Ready\\Histograms\\deep_2100_26.csv')
s5 <- read.csv('D:\\MODELS_Ready\\Histograms\\shallow_2100_85.csv')
s <- read.csv('D:\\MODELS_Ready\\Supplementary_Material\\Shallow_occ_thin.csv')
df<-NULL
breaks <- seq(-180,180, by=5)
spvector<-unique(breaks)
#present 
df<- NULL
head(s)
head(sub1)
j=1
for (j in 1:length(spvector)) {
  sub1 <-s[s$decimalLongitude >= spvector[j],]
  sub2 <-sub1[sub1$decimalLongitude <= spvector[j]+5,]
  m <-  nrow(sub2) #change the name of the column
  df = rbind(df, data.frame(spvector[j], m))
}
write.csv(df, 'D:\\MODELS_Ready\\Supplementary_Material\\Longitude_occ_shallow.csv', row.names=F)

#longitude

ggplot(df, aes(x=sp))+ geom_histogram(color="black", fill="white") 

#+ 
  geom_histogram(color="black", fill="white") + 
  theme_classic()
#+
  # xlim(-90, 90+
 # geom_density(aes(y=..count../10))+
  #scale_x_continuous(expand = c(0,0),limits =
  #                     c(-90, 90))+
  scale_y_continuous(expand = c(0,0),limits =
                       c(0, 500000),breaks = seq(0, 500000, by = 100000))+
  labs(title="Lat_shallow_present")






