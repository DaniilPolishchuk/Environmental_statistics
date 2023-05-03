#Daniil Polishchuk

#Global variables####
#Solution in the end 

map <- read_sf("map-folder/map.shp")
map_nblist <- poly2nb(map)
data <- read_csv2("data-folder/separateWC.csv")
swc <- left_join(map, data, by = "COD_PRO")
wlist_til <- nb2listw(map_nblist, style = "W")
n <- nrow(map)

y <- swc$Y04
lisa_moran <- localmoran(y, wlist_til, alternative = "two.sided")

#identify the quadrant of the moran 
#scatter plot by building a vector
#with the "HH", "LL", "HL", "LH"

#obtain a map with "significant areas" 
#distinguished by quadrant of moran scatter plot 

####SOLUTION####

# use output of localmoran() to get quadrats 
# we store result in lisa_moran
quadr <- attr(lisa_moran,"quadr")$mean

# draw the map 
swc$quadr <- quadr
tm_shape(swc)+
  tm_borders()+
  tm_fill("quadr", title = "", position = c("RIGHT", "TOP"))