library(sf)
library(tmap)
library(spdep)
library(tidyverse)


#read the italian provinces map 

map <- read_sf("map-folder/map.shp")
class(map)
names(map)

#check for the type of coerdinates (Projected CRS) 
#before we draw 

# map of the italian provinces 

tm_shape(map) +
  tm_borders()

#read data on seperate waste collection 
data <- read_csv2("data-folder/separateWC.csv")

#join the map object with the data object 
#left join 

swc <- left_join(map, data, by = "COD_PRO")

# to get documentation 
vignette("tmap-getstarted")

# map of swc in 2004

tm_shape(swc)+
  tm_fill("Y04", style = "quantile")

tm_shape(swc)+
  tm_borders()+
  tm_fill("Y04", style = "quantile")

tm_shape(swc)+
  tm_borders()+
  tm_fill("Y04", palette = "Blues", style = "quantile")+
  tm_layout(legend.position = c("left", "bottom"))


tm_shape(swc)+
  tm_borders()+
  tm_fill("Y04", palette = "Blues", style = "quantile")+
  tm_layout(legend.position = c("RIGHT", "TOP"), main.title = "SWC in 2004", legend.title.color = "white", frame = F)


# multipanel map

tm_shape(swc)+
  tm_borders()+
  tm_fill(c("Y04", "Y09"), palette = "Blues", style = "quantile")

#create my breaks 
my_breaks <- quantile(swc$Y04)


tm_shape(swc)+
  tm_borders()+
  tm_fill("Y04", palette = "Blues", breaks = my_breaks)+
  tm_layout(legend.position = c("RIGHT", "TOP"), main.title = "SWC in 2004", frame = F)

# breaks for 2004 and 2009 
my_breaks <- quantile(c(swc$Y04, swc$Y09))

tmap_mode("view")
tm_shape(swc)+
  tm_borders()+
  tm_fill(c("Y04", "Y09"), palette = "Blues", breaks = my_breaks)

tmap_mode("plot")



