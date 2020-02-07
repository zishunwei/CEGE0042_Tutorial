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

View(uk_districts@data)

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



# The concept of lagged variables
# 时间自相关量化了一个过程的近距离观察比时间上远距离观察更相似的程度。

ChMeanTemp <- colMeans(china_temp[,4:(ncol(china_temp))])
ChLagged <- data.frame(year = 1951:2001, t=ChMeanTemp[2:(length(ChMeanTemp))], t_minus_1=ChMeanTemp[1:(length(ChMeanTemp)-1)])
View(ChLagged)

# p1 draws a time series of the temperatures; 
# p2 draws a scatter plot with the temperature in each year on the x-axis, and the temperature in the previous year on the y-axis.
p1 <- ggplot(ChLagged, aes(x=year, y=t)) + geom_line()
p2 <- ggplot(ChLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  annotate("text", 8.5, 10, label=paste("r =", round(cor(ChLagged$t, ChLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)

# 'cor' used to calculate the autocorrelation in the temperatures
cor(ChLagged$t, ChLagged$t_minus_1)



# Calculating Temporal autocorrelation

# 通常针对一定数量的滞后计算自相关函数（ACF），并将其显示在ACF图上
acf(ChMeanTemp)

acf(uk_temp_matrix["Midlands",], lag.max=50, main="ACF, Midlands")

acf(ch_temp_matrix[1,], lag.max=50, main="ACF, station 1")

acf(colMeans(matrix(uk_temp_matrix["Midlands",],12)), main="ACF, Midlands Annual Average")

# 偏自相关函数（PACF）是在考虑了所有较低时滞的自相关之后，在特定时滞下的自相关的度量
pacf(uk_temp_matrix["Midlands",], lag.max=50, main="ACF, Midlands")

pacf(ch_temp_matrix[1,], lag.max=50, main="ACF, station 1")

# the confidence intervals are different (acf and pacf)



# Sptial Autocorrelation


# Spatial adjacency and spatial weight matrices
W <- nb2listw(poly2nb(uk_districts))
W

library(knitr)
kable(listw2mat(W))


# Global Spatial Autocorrelation Measures
uk_temp_avg <- rowMeans(uk_temp_matrix)
moran.test(x=uk_temp_avg, listw=W)

# Monte Carlo approach to estimating significance
moran.mc(x=uk_temp_avg, listw=W, nsim=9999)
# 检验原假设，即我们观察到的自相关水平可能是由于偶然（随机）造成的。
# 重要的统计数据是p值；假设如果p <0.05，则有95％的置信区间，那么我们可以在这个水平上拒绝原假设。


# Local Autocorrelation Measures
# 全局自相关度量总结了整个研究区域中自相关的总体水平。但是，自相关的水平不一定在整个空间都是恒定的
# 这称为空间异质性，无法使用全局自相关度量来捕获。
lm <- localmoran(x=rowMeans(uk_temp_matrix), listw=W)
lm
# 第一列是局部Moran's I的值，第二列是期望值，第三列是方差，第四列是标准偏差。第五列提供p值。



# Measuring autocorrelation in point data
coords = list(projectMercator(china_temp[,2], china_temp[,1]))
plot(variogram(list(rowMeans(ch_temp_matrix)), locations=coords))

#检查不同方向上的半方差是否存在差异
plot(variogram(list(rowMeans(ch_temp_matrix)), locations=coords, alpha=c(0,45,90,135)))
# We only need to look at angles up to 135° because the remaining angles are opposites and identical.



# Spatio-temporal Autocorrelation


# 时空自相关函数
# 使用空间权重矩阵合并了一系列空间邻居,它计算在时间和空间上被滞后分开的观测值之间的相关性

source("Data/starima_package.R")
Wmat <- listw2mat(W)
stacf(t(uk_temp_matrix), Wmat, 48)

stpacf(t(uk_temp_matrix), Wmat, 4)


# 时空半变异函数
以空间和时间间隔计算半变异

# Project the points to get spatial lags in metres
pts <- SpatialPoints(china_temp[,1:2], 
                     proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# Years are converted to date format
time <- seq(as.Date("1951-01-01"), length = 52, by = "year")

stfdf <- STFDF(pts, time, data.frame(as.vector(t(ch_temp_matrix))))
names(stfdf@data) <- "Temperature"

# Calculate ST-semivariogram with bin width 100km, max lag 1000km.
ChSTVar <- variogram(Temperature~1, stfdf, width=100, cutoff=1000,tlags=0:10)

# 2D plot
plot(ChSTVar)

# 3D plot
plot(ChSTVar, wireframe=T)

# 可以看出，半方差随着时间滞后的增加而迅速增加。空间滞后的增加不那么引人注目。
# 时间和空间的距离具有不同的含义，不能直接比较
# 不容易将100公里的空间距离与一年的时间间隔进行比较
# 许多时空模型都假设时间和空间的影响可以分开。