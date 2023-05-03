# simulate rho 
# simulate all
#draw 

#test lines with  ggplot 
#combine different plots together 


# Exercise: obtain (and store) k = 1000
# simulation from moran's I and aple 
#sampling  distribution for "some" values of 
# rho ranging from 0 to 0.95 and obtain 
# the plot of MSE vs rho 

library(sf)
library(tmap)
library(spdep)
library(tidyverse)




#Global variables####

map <- read_sf("map-folder/map.shp")

map_nblist <- poly2nb(map)

wlist <- nb2listw(map_nblist, style = "B")

wlist_til <- nb2listw(map_nblist, style = "W")

n <- nrow(map)
w <- matrix(0, nrow = n, ncol = n)
for(k in 1:n){
  w[k, wlist$neighbours[[k]] ]<- 1 
}

w_til <- w/rowSums(w)




#Simulation rho = 0 ####

rho = 0 

In <- diag(n)

sigma_sar <- solve(
  t(In - rho * w_til) %*% (In - rho * w_til)
)

k <- 1000
moran_sim <- numeric(k)
aple_sim <- numeric(k)
for(i in 1:k){
  
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y<- Y - mean(Y)
  moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple_sim[i] <- aple(Y, wlist_til)
}


#mse
mse_moran <- mean((moran_sim - rho)^2)
mse_aple <- mean((aple_sim - rho)^2)

##############################
n_draw = 20
rho <- seq(from = 0, to =  0.95, length.out = n_draw)

mse_moran_sim <- numeric(n_draw)
mse_aple_sim <- numeric(n_draw)

k <- 1000

#### Start our code 

for(j in 1:n_draw){
  
  print(j)
  
  In <- diag(n)
  
  sigma_sar <- solve(
    t(In - rho[j] * w_til) %*% (In - rho[j] * w_til)
  )
  
  # simulate the moran's I and aple 
  
  moran_sim <- numeric(k)
  aple_sim <- numeric(k)
  
  for(i in 1:k){
    Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
    Y <- Y - mean(Y)
    moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
    aple_sim[i] <- aple(Y, wlist_til)
  }
  
  
  mse_moran_sim[j] <- mean((moran_sim - rho[j])^2)
  mse_aple_sim[j] <- mean((aple_sim - rho[j])^2)
  
  
}



#ploting 

data <- data.frame(moran = mse_moran_sim, aple = mse_aple_sim)

windows()
ggplot(data = data, aes(x = rho))+
  geom_line(aes(y = moran, col = "Moran"))+
  geom_line(aes(y = aple, col = "ALPE"))+
  geom_point(aes(y= moran))+
  geom_point(aes(y= aple))
  xlab("rho")+ 
  ylab("MSE")+
  theme(legend.title=element_blank())+
  ggtitle("The mean squared error as a funtion of rho")
  # scale_fill_discrete(labels=c("Moran's I", "APLE"))



# rho<- 0
# 
# In <- diag(n)
# 
# sigma_sar <- solve(
#   t(In - rho * w_til) %*% (In - rho * w_til)
# )

## SIMULATION 

# k <- 1000
# moran_sim <- numeric(k)
# aple_sim <- numeric(k)
# 
# for(i in 1:k){
#   Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
#   Y<- Y - mean(Y)
#   moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
#   aple_sim[i] <- aple(Y, wlist_til)
# }


# mse_moran <- mean((moran_sim - rho)^2)
# mse_aple <- mean((aple_sim - rho)^2)


# simulation <- tibble(
#   moran  = moran_sim,
#   aple  = aple_sim
# )


# ggplot(data = simulation)+
#   geom_histogram(aes(x = moran), fill = 4, alpha = 0.75)+
#   geom_histogram(aes(x = aple), fill = 2, alpha = 0.75)+
#   geom_vline(xintercept = rho)


