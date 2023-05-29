##### MOCK EXAM #####
#Import libraries 
library(sf)
library(tmap)
library(spdep)
library(tidyverse)
library(ggplot2)
library(spatialreg)
library(mvtnorm)

#import map#
map_emr <- read_sf("mock-exam/map-emr.shp")

#build the map to check if the data is ok 
tm_shape(map_emr) +
  tm_borders()

#import data from file 
data <- read.csv2('mock-exam/data.csv', header = T)

#obtain the map of the Y 
#firstly get our data 
map_emr$Y <- data$Y

#then obtain the map
#here we do not centre the Y yet
tm_shape(map_emr)+
  tm_fill("Y", palette = 'Reds')# fill with red color 


# Adjacency matrix W 
# Building the adjacency matrix 
map_nblist <- poly2nb(map_emr)

# using spdep list 
wlist <- nb2listw(map_nblist, style = "B")

# Hand-made w to obtain matrix
n <- nrow(data)
#matrix object
w <- matrix(0, n, n)
for(k in 1:n){
  w[k, wlist$neighbours[[k]] ]<-1
}
# w is a sparse matrix
image(w != 0)

# Hand-made row-standardized w
w_til <- w / rowSums(w)


# Row-standardized adjacency matrix
wlist_til<-nb2listw(map_nblist,style="W")


# compute moran's I
y <- map_emr$Y

#row standardiesed data 
y <- y - mean(y)
y_til <- w_til %*% y


sd(y_til)/sd(y)# should be less then 1


#Moran's I using the spdep function 
moran(y,wlist_til, n = n, S0 = n)
# S0 is the sum of elements of row 


#Moran's I in matrix form
I <- cov(y, y_til)/var(y)
I_2 <- t(y) %*% w_til %*% y / t(y) %*% y


#ord's index in matrix form 
#t(Y) %*% W_til %*% Y / t(Y_til) %*% Y_til
o_2 <-t(y)  %*% w_til %*% y / t(y_til) %*% y_til

#using spatial lag 
o <- t(y) %*% y_til / (t(y_til) %*% y_til)


# Moran's I - regration coefficion 
lm(formula = y_til ~ y)$coeff[2]

#ALPE using spdep fun.
aple(y, w_til)

#compare the variance of y and lagged variable 
var(y_til)/var(y)
# the variance of row-standardised Y (Y lagged) 
#is less than of the regular Y


#Monte-carlo test for SAT
# Monte-Carlo test
moran.mc(map_emr$Y, wlist_til, nsim = 9999)
#Therefore we can say that there is a high global spatial autocorrelation


#Moran scatter plot 

# y = y - mean(y)
windows()
par(mfrow=c(1,2))
moran.plot(y, wlist_til)

plot(map_emr$Y - mean(map_emr$Y), y_til - mean(y_til))
abline(h = 0, v = 0)

# Local Indicators of spatial association.
moran_loc <- localmoran(y, wlist_til, alternative = "two.sided")
lisa <- moran_loc[,1]

#obtain the map with significant p-values 
significant_pvalues <- ifelse(moran_loc[,5] < 0.05, "signif", "not_signif")

map_emr$significant_pvalues <- significant_pvalues

tm_shape(map_emr)+
  tm_borders(col = 'grey')+
  tm_fill('significant_pvalues', palette = c('white', 'red'))

#14 ----

model <- lm(Y ~ X, data = data)

summary(model)

#15 -----

residuals_lm <- model$residuals

map_emr$residuals_lm <- residuals_lm

# map of the residuals of the CLASSICAL lm
tm_shape(map_emr) +
  tm_borders() +
  tm_fill("residuals_lm")

#16---

lm.morantest(model, wlist_til)

## When testing for a residual spatial autocorrelation, we can assume that
## there is spatial residual autocorrelation since the Moran's I is high.


#17 ----

convergence_sperr <- errorsarlm(Y~X, listw = wlist_til, data = data)

summary(convergence_sperr)

#18 ----

lm.LMtests(model, Wlist_til, test = "LMerr")

