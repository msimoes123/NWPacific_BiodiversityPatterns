# Ggplot2 library
library(ggplot2)

# Use the mtcars dataset.
setwd('C:\\Users\\admin\\Desktop\\Database_paper\\Densities_year\\Species_ecor')
dir()
x<- read.csv('Diversity_zone_range_graph.csv')
head(x)

ggplot(data=x, aes(x=Lat, y=Count, fill=X)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  scale_fill_manual(values=c( "#033E6B","#3F92D2"))+
  theme_classic() 

#---------------------------------------------------------------
#packcircles
setwd('C:\\Users\\admin\\Desktop\\Database_paper\\circle packing')
install.packages('packcircles') 
install.packages('ggplot2') 
install.packages('scales')
install.packages('RColorBrewer')
install.packages('grDevices')
install.packages('viridis')
library(viridis)
library(grDevices)
library(RColorBrewer)
library(scales)
library(packcircles)
library(ggplot2)

# Create data
data<- read.csv('Arctic\\Arctic_circle_total.csv')
str(data)
#example line:data <-data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$value, sizetype='area')

# We can add these packing information to the initial data frame
data = cbind(data, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=200)


# Make the plot
p <-ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 1) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=value, label = data$Phylum)) +
  scale_size_continuous(range = c(1,4)) +
  scale_fill_manual(values = viridis(nrow(data))) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="right") +
  coord_equal()  +
  ggtitle("Arctic_diversity")

p

#Culmulative curve-------------------------------------------------------
# collect the values together, and assign them to a variable called y
c(6,10,10,17,7,12,7,11,6,16,3,8,13,8,7,12,6,5,10,9) -> y

hist( y ) -> h # do a histogram of y and assign its info to h
h$counts <- cumsum(h$counts) # replace the cell freq.s by cumulative freq.s
plot( h ) # plot a cumulative histogram of y



