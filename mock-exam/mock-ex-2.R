### Mock exam 04.06.2020


#Import libraries 
library(sf)
library(tmap)
library(spdep)
library(tidyverse)
library(ggplot2)
library(spatialreg)
library(mvtnorm)

#1 ---
#import map 
molise <- read_sf('map-exam-areal/molise.shp')

#Check map
tm_shape(molise)+
  tm_borders()


#2 ---
#import data 
db <- read.csv2('data-areal/data-molise.csv', header = T)
db

#3 ---

head(db)
# add study vsriable to map object 
molise$Y <- db$dep_var

#obtain the map 
tm_shape(molise)+
  tm_borders()+
  tm_fill("Y")+
  tm_layout(legend.position = c("left","top"))# legend is top left as asked in the task



#4 ---


# Adjacency matrix W 
# Building the adjacency matrix 
map_nblist <- poly2nb(molise)

# using spdep list 
wlist <- nb2listw(map_nblist, style = "B")

# Hand-made w to obtain matrix
n <- nrow(db)
#matrix object
w <- matrix(0, n, n)

for(k in 1:n){
  w[k, wlist$neighbours[[k]] ]<-1
}

#5 --- 


# Hand-made row-standardized w
w_til <- w / rowSums(w)


# Row-standardized adjacency matrix using spdep
wlist_til<-nb2listw(map_nblist,style="W")


#6 --- 

# compute moran's I
y <- molise$Y

#row standardiesed data 
y <- y - mean(y)
y_til <- w_til %*% y


#I <- cov(y, y_til)/var(y)
I <- t(y) %*% w_til %*% y / t(y) %*% y



#7 ---

###
 
#Firstly, it provides a standardized measure of spatial autocorrelation,
#which allows for easier interpretation and comparison across 
#different datasets. 
#Additionally, it helps to eliminate the effects of varying population 
#sizes among spatial units, which can lead to biased results 
#if not properly accounted for. 
#Finally, row-standardization helps to ensure 
#that the spatial weights matrix is row-stochastic, 
#which is a necessary condition for the proper calculation of Moran's I.


#8 ---

var(y_til)/var(y)
# the variance of row-standardised Y (Y lagged) 
#is less than of the regular Y

#When the variance of the spatially lagged variable 
# is greater than the variance of the study variable, 
# it indicates that there is positive spatial autocorrelation 
# in the dataset, meaning that neighboring values 
# tend to be similar to each other. On the other hand, 
# if the variance of the study variable is greater 
# than the variance of the spatially lagged variable, 
# it suggests that there is negative spatial autocorrelation, 
# indicating that neighboring values tend to be dissimilar to each other.

#9 ---

# Monte-Carlo test
moran.mc(molise$Y, wlist_til, nsim = 9999)
#We can say that there is a high global spatial autocorrelation

#conclude that there is spatial dependence


#10 --- 

head(db)
model <- lm(dep_var ~ indep_var, data = db)

summary(model)

#11 --- 

lm.morantest(model, wlist_til)

#12 ---

convergence_sperr <- errorsarlm(dep_var ~ indep_var, listw = wlist_til, data = db)

summary(convergence_sperr)
