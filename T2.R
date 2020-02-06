# 2 Spatio-temporal Dependence and Autocorrelation

# 重点介绍两种类型的空间数据：面数据和点数据
# 首先，您将使用自相关函数和部分自相关函数检查时间自相关
# 然后，您将了解点和面数据的空间自相关索引
# 最后，您将学习如何在这两种数据类型中量化时空自相关


# Load the required libraries

library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)

#set working directory accordingly with setwd()
uk_districts <- readOGR(dsn="Data/uk_districts.shp", layer="uk_districts", 
                        p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)


china_temp <- read.csv("Data/Temp_China.csv")
ch_temp_matrix<-data.matrix(china_temp[,4:ncol(china_temp)])
uk_temp_matrix<-data.matrix(uk_districts@data[,-c(1:3)])
rownames(uk_temp_matrix) <- uk_districts@data[,"NAME"]



# Mapping the data
tm_shape(uk_districts)+ 
  tm_fill("Jan_1910", style="jenks", palette="Purples")+
  tm_borders("white")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar()

#  colour palette is defined based on the entire space-time series.
brks=quantile(as.numeric(unlist(uk_districts@data[,-c(1:2)])), seq(0,1,1/10)) 

tm_shape(uk_districts)+ 
  tm_fill("Jan_1910", style="fixed", palette="-Spectral", breaks=brks)+
  tm_borders("white")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar()

tm_shape(uk_districts)+ 
  tm_fill("Jul_1910", style="fixed", palette="-Spectral", breaks=brks)+
  tm_borders("white")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar()
