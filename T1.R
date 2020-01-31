# Change your working directory using setwd()
temp <- read.csv("Data/Temp_China.csv")
colnames(temp)[4:ncol(temp)] <- as.character(c(1951:2002))

station<-paste("sta",1:nrow(temp),sep = "")
View(station)

# column bind
temp<-cbind(station,temp)

temp_matrix<-data.matrix(temp[,5:ncol(temp)])



# 1.2 Examining non spatio-temporal data characteristics
mu = mean(temp_matrix)
mu

sdev = sd(temp_matrix)
sdev

# draw histogram of temp_matrix
hist(temp_matrix)
abline(v=mu, col="red")

# QQ plot
qqnorm(temp_matrix)
qqline(temp_matrix, col="red")

# draW simple scatterplot matrix
pairs(~LOG+LAT+ALT+rowMeans(temp_matrix),data=temp,
      main="Simple Scatterplot Matrix")

# install if necessary
library(scatterplot3d)
library(plot3D)
library(rgl)

#1. Using Scatterplot3d
scatterplot3d(x=temp$LAT, y=temp$ALT, z=rowMeans(temp_matrix))

#2. Using plot3D (x,y and z are first three variables so no need to # explicitly define them).
scatter3D(temp$LAT, temp$ALT, rowMeans(temp_matrix))

# Need to see if this can be interactive in web-version
plot3d(temp$LAT, temp$ALT, rowMeans(temp_matrix))

# Looking at the 3D plot, how would you interpret this pattern?



# 1.3 Examining temporal characteristics
plot(colMeans(temp_matrix), xlab = "Year", ylab = "Temperature", type="l", xaxt="n")
axis(1, at = seq(9, 49, 10), labels=seq(1960, 2000, 10))

library(reshape)
newtemp <- melt(temp, id.vars=1:4, measure.vars=5:ncol(temp))

colnames(newtemp)[5:6] <- c("year", "temperature") 

# create a multiple line plot of the data 
# using the lattice library
library(lattice)
station.chosen=c("sta1","sta2","sta3","sta4","sta5","sta6","sta7","sta8","sta9","sta10")
View(station.chosen)
#Create a variable containing just the selected stations
a <- newtemp[station %in% station.chosen,]

# plot the time series of a number of stations on the same map
xyplot(temperature ~ year | station, xlab = "year", type = "l",
       layout = c(5, 2),
       data=a,
       main = "Temperature in China")

# select temperature data from temp data frame and create a matrix of temperature data.
#Create the heatmap:
heatmap(temp_matrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column", margins=c(5,3),xlab="Year",ylab="Station ID", cexCol=1.1,y.scale.components.subticks(n=10))

# ordering the stations by altitude to see the effect:
temp_order<-temp[order(temp$ALT, decreasing=TRUE),]
temp_ordermatrix<-data.matrix(temp_order[,5:ncol(temp)])
heatmap(temp_ordermatrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column",margins=c(3,3))
# levelplot
levelplot(t(temp_ordermatrix), aspect="fill")
# levelplot requires the input to be transposed (rows and columns exchanged) to produce the same plot.



# 1.4 Examining spatial characteristics
library(ggplot2)
library(OpenStreetMap)
library(raster)

# Note, you need to have the correct Java version (32 or 64 bit) installed for the following to work
#Get the data from the final year (2002):
data_last<-cbind(temp[1:4],temp$"2002")
# Change the column name:
colnames(data_last)[5]<-"tempvalue"
# Make a proportional symbol of the latest data. 
# Convert latitude and longitude to Mercator projection:
data_last[,2:3] <-projectMercator(data_last$LAT, data_last$LOG)
# Download a map tile:
map <- openmap(c(53.5,73.6),c(15.7,134.7),type= 'esri-topo')

autoplot(map)+ geom_point (data= data_last, aes(x=LOG,y=LAT, color=tempvalue, size=tempvalue))+ ggtitle("Annual Average Temperature in China, 2002")


hist(data_last$ALT, 20)
hist(log(data_last$ALT), 20)

# using log to make its distribution more normal
autoplot(map) + 
  geom_point(data = data_last, aes(x = LOG, y = LAT, color = tempvalue, size = log(ALT)))+ ggtitle("Annual Average Temperature in China, 2002") +
  scale_colour_gradient2(low="blue", high="red")

hist(rowMeans(temp_matrix), 20)

# plot those with a mean temperature over >15°C
high_temp <- which(rowMeans(temp_matrix)>15)
data_last <- cbind(data_last, "Low", stringsAsFactors = FALSE)
colnames(data_last)[ncol(data_last)] <- "TempGroup"
data_last[high_temp,"TempGroup"] <- "High"

autoplot(map) + geom_point(data = data_last, aes(x = LOG, y = LAT, color = TempGroup, size = log(ALT)))+ ggtitle("Stations with High/Low means")

# As these two groups appear to have distinct distributions, it may be a good idea to model them separately.

# use map series to visualise the change in phenomena
newtemp[,2:3] <- projectMercator(newtemp$LAT, newtemp$LOG)
# Select years from 1991 onwards
years <- which(as.numeric(as.character(newtemp$year))>1990)
autoplot(map) + geom_point(data=newtemp[years,], aes(x=LOG,y=LAT, color=temperature, size=log(ALT))) + facet_wrap(facets=~year) +
  scale_colour_gradient2(low="blue", high="red")


# Calculate difference in temperatures from year to year (first year is zero as there is no preceding year)
tempdiff <- cbind(0, temp[,6:ncol(temp)]-temp[,5:(ncol(temp)-1)])
newtemp <- cbind(newtemp, unlist(tempdiff))
colnames(newtemp)[ncol(newtemp)] <- "tempdiff"

autoplot(map) + geom_point(data=newtemp[years,], aes(x=LOG,y=LAT, color=tempdiff, size=log(ALT))) + scale_colour_gradient2(low="blue", mid='white', high="red") + facet_wrap(facets=~year)





