#### libraries ####
library(sf)
library(tmap)
library(spdep)
library(tidyverse)
library(ggplot2)
library(spatialreg)
library(mvtnorm)





#### Global variables ####

#read the italian provinces map 
map <- read_sf("map-folder/map.shp")

s
# map of the italian provinces 
tm_shape(map) +
  tm_borders()

#read data on seperate waste collection 
data <- read_csv2("data-folder/separateWC.csv")


#join the map object with the data object 
#left join 
swc <- left_join(map, data, by = "COD_PRO")

# map of swc in 2004
tm_shape(swc)+
  tm_fill("Y04", style = "quantile")