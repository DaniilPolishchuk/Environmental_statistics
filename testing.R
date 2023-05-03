#Global variables####

map <- read_sf("map-folder/map.shp")

map_nblist <- poly2nb(map)

data <- read_csv2("data-folder/separateWC.csv")

swc <- left_join(map, data, by = "COD_PRO")

wlist <- nb2listw(map_nblist, style = "B")

wlist_til <- nb2listw(map_nblist, style = "W")

n <- nrow(map)
w <- matrix(0, nrow = n, ncol = n)
for(k in 1:n){
  w[k, wlist$neighbours[[k]] ]<- 1 
}

w_til <- w/rowSums(w)



#####Lectures code#####

#Testing for global  spatial autocorrelation ####
#Normal assumption on Y and on the test statistics 

#H_0: hro = 0
#H_1: hro > 0 

y <- swc$Y04


moran.test(y, wlist_til, randomisation = F, 
           alternative = "greater")
# we use V_n(I|h_o) if we use randomisation=T


##Normal assumption only on the test statistics
moran.test(y, wlist_til, randomisation = T, 
           alternative = "greater")


#drop all the assumption - Monte Carlo permutation test 
moran.mc(y, wlist_til, nsim = 10000, alternative = "greater")


#Hand mademontecarlo permutation test 

x <- c(10, 5, 23, 75, 1)
sample(x, size = length(x), replace = F)


# build the distribution of Moran's I under permutations 

#number of permutation 
k = 50000

#object containing realisation of moran's I under the H_0
I_perm <- numeric(k)

for(i in 1:k){
  y_perm <- sample(y, size = n, replace = F)
  I_perm[i] <- moran(y_perm, wlist_til, n= n, S0 = n)$I
}


#compute a monte-carlo p-value 
#observe value of the test statistics 

I_obs <- moran(y, wlist_til, n= n, S0 = n)

p_value_mc <- sum(I_perm >= I_obs)/k # we can use mean instad


#plort the distribution of moran's I

hist(I_perm, breaks = 40)





#LISA -  local indicators of spatial assosciation  ####

lisa_moran <- localmoran(y, wlist_til, alternative = "two.sided")


sum(lisa_moran[,1])/n# moran'I

#mapping local moran's I and p_values 
swc$I_i <- lisa_moran[,1]
tm_shape(swc)+
  tm_borders()+
  tm_fill("I_i", palette = "RdBu", style = "quantile", midpoint = NA)

#maping areas with "significant" p-values 

significant_pvalues <- ifelse(lisa_moran[,5] < 0.05, "signif", "non_split")
swc$significant_pvalues <- significant_pvalues
tm_shape(swc)+
  tm_borders()+
  tm_fill("significant_pvalues")

#identify the quadrant of the moran 
#scatter plot by building a vector
#with the "HH", "LL", "HL", "LH"

#obtain a map with "significant areas" 
#distinguished by quadrant of moran scatter plot 

m <- moran.plot(y, wlist_til, return_df = T)
mean(y)
wlist_til

quadr <- attr(lisa_moran,"quadr")$mean
swc$quadr <- quadr
tm_shape(swc)+
  tm_borders()+
  tm_fill("quadr", title = "", position = c("RIGHT", "TOP"))
