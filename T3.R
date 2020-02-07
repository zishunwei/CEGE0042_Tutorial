# 3 Statistical modelling of time series and Spatio-temporal series

# Stationarity and trends 平稳性和趋势
library(forecast)
library(ggplot2)
library(fpp2)
library(gridExtra)

p1 <- autoplot(goog)
p2 <- autoplot(sunspotarea)
p3 <- autoplot(marathon)
p4 <- autoplot(oil)

grid.arrange(p1, p2, p3, p4)

