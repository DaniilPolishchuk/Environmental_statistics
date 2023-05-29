# run global_spatial_assosiation before 

library(spatialreg)
library(mvtnorm)



#build the covariaance matrix of SAR process

#sigma^2 is just scale is not really valiable 



#### RHO = 0 ####
rho <- 0
In <- diag(n)

sigma_sar <- solve(
  t(In - rho * w_til) %*% (In - rho * w_til)
)

#draw a realisation from the SAR process
Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
# make vector from the matrix 


#mean center the matrix 
Y <- Y - mean(Y)

map$Y <- Y

# draw map
tm_shape(map)+
  tm_borders()+ 
  tm_fill("Y")


#compute moran's I and APLE

moran(Y, wlist_til, n = n, S0 = n)$I

aple(Y, wlist_til)


# EXERCISE: obtain (and store) K = 1000 
# realisation from the moran's and aple 
# sampling distribution

moran_1000 <- c()
aple_1000 <- c()
Y_1000 <- c()
for( i in 1:1000){
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y <- Y - mean(Y)
  moran <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple <- aple(Y, wlist_til)
  Y_1000 <- c(Y_1000, Y)
  moran_1000 <- c(moran_1000, moran)
  aple_1000 <- c(aple_1000, aple)
  
}

#MAX version 
moran_1000 <- rep(0, 1000)
aple_1000 <- rep(0, 1000)
Y_1000 <- rep(0, 1000)
for( i in 1:1000){
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y<- Y - mean(Y)
  Y_1000[i] <- Y
  moran_1000[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple_1000[i] <- aple(Y, wlist_til)
}


#Prof sol.
k <- 1000
moran_sim <- numeric(k)
aple_sim <- numeric(k)
for(i in 1:k){
  
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y<- Y - mean(Y)
  moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple_sim[i] <- aple(Y, wlist_til)
}



#bies 
bies_moran <- mean(moran_sim - rho)
bies_aple <- mean(aple_sim - rho)

# bies of Moran's I if rho = 0
# -1/ (n - 1)
# -0.009174312

# get clother bies we should increase K(as we use montecarlo method)

#mse 
rho = 0
mse_moran <- mean((moran_sim - rho)^2)
mse_aple <- mean((aple_sim - rho)^2)
mse_moran/mse_aple



#obtain the plot of the sampling distribution 
library(ggplot2)

simulation <- tibble(
  moran  = moran_sim,
  aple  = aple_sim
)


ggplot(data = simulation)+
  geom_histogram(aes(x = moran), fill = 4, alpha = 0.75)+
  geom_histogram(aes(x = aple), fill = 2, alpha = 0.75)+
  geom_vline(xintercept = rho)


#### RHO = 0.7 ####


rho<- 0.7

sigma_sar <- solve(
  t(In - rho * w_til) %*% (In - rho * w_til)
)


Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
# make vector from the matrix 


#mean center the matrix 
Y <- Y - mean(Y)

map$Y <- Y

# draw map
tm_shape(map)+
  tm_borders()+ 
  tm_fill("Y")


## SIMULATION 

k <- 1000
moran_sim <- numeric(k)
aple_sim <- numeric(k)
for(i in 1:k){
  
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y<- Y - mean(Y)
  moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple_sim[i] <- aple(Y, wlist_til)
}

bies_moran <- mean(moran_sim - rho)
bies_aple <- mean(aple_sim - rho)

mse_moran <- mean((moran_sim - rho)^2)
mse_aple <- mean((aple_sim - rho)^2)

mse_aple/mse_moran


simulation <- tibble(
  moran  = moran_sim,
  aple  = aple_sim
)


ggplot(data = simulation)+
  geom_histogram(aes(x = moran), fill = 4, alpha = 0.75)+
  geom_histogram(aes(x = aple), fill = 2, alpha = 0.75)+
  geom_vline(xintercept = rho)




#### RHO = 0.95 #### 
# problem is that aple is built on the first taylor's polim.



rho<- 0.95

sigma_sar <- solve(
  t(In - rho * w_til) %*% (In - rho * w_til)
)

## SIMULATION 

k <- 1000
moran_sim <- numeric(k)
aple_sim <- numeric(k)
for(i in 1:k){
  
  Y <- c(mvtnorm::rmvnorm(1, sigma = sigma_sar) )
  Y<- Y - mean(Y)
  moran_sim[i] <- moran(Y, wlist_til, n = n, S0 = n)$I
  aple_sim[i] <- aple(Y, wlist_til)
}

bies_moran <- mean(moran_sim - rho)
bies_aple <- mean(aple_sim - rho)

mse_moran <- mean((moran_sim - rho)^2)
mse_aple <- mean((aple_sim - rho)^2)

mse_aple/mse_moran


simulation <- tibble(
  moran  = moran_sim,
  aple  = aple_sim
)


ggplot(data = simulation)+
  geom_histogram(aes(x = moran), fill = 4, alpha = 0.75)+
  geom_histogram(aes(x = aple), fill = 2, alpha = 0.75)+
  geom_vline(xintercept = rho)


# Exercise: obtain (and store) k = 1000
# simulation from moran's I and aple 
#sampling  distribution for "some" values of 
# rho ranging from 0 to 0.95 and obtain 
# the plot of MSE vs rho 




