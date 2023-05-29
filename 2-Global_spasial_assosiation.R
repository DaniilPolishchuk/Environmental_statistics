#read the italian provinces map 

map <- read_sf("map-folder/map.shp")
class(map)
names(map)


# map  of the italian provinces 

tm_shape(map) +
  tm_borders()

#read data on separate waste collection 

data <- read_csv2("data-folder/separateWC.csv")

#join the map object with the data object 
#left join 

swc <- left_join(map, data, by = "COD_PRO")


#building  the adjacency matrix 
map_nblist <- poly2nb(map)


# get the centroids of the italian provinces 

centriods <- st_centroid(
  st_geometry(map), of_largest_polygon = T
)


tm_shape(map)+
  tm_borders()+
  tm_shape(centriods)+
  tm_dots()

# graph introdused by adjacency matrix 

plot(map_nblist, centriods, col = "blue", lwd = 1)

#Adjacency matrix  in list form 
wlist <- nb2listw(map_nblist, style = "B")


#row - standardised 
#Adjacency matrix  in list form 
wlist_til <- nb2listw(map_nblist, style = "W")

#handmade adj. matrix 

n <- nrow(map)
w <- matrix(0, nrow = n, ncol = n)
for(k in 1:n){
  w[k, wlist$neighbours[[k]] ]<- 1 
}

#row standardisation 
w_til <- w/rowSums(w)
rowSums(w_til)


#compute of Moran's 

y <- swc$Y04

# mean stand. 
y <- y - mean(y)

# computing the spatial lag 
y_til <- w_til %*% y

sd(y_til)/sd(y)
# should be less that 1 



#Moran's I with the spdep function 
moran(y,wlist_til, n = n, S0 = n)
# S0 is the sum of elements of row 

# k is kuriosis of the study variable 

# Moran's I is slide 50/67 
t(y) %*% w_til %*% y / (t(y) %*% y)

# Moran's I - regration coefficion 
lm(formula = y_til ~ y)

#Ord Index  - slide 50/67
t(y) %*% w_til %*% y / (t(y_til) %*% y_til)

