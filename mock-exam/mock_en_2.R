library(spatstat)

windows()
plot(X)

# number of points 
n_points <- X$n

#area
area <- area.owin(X$window)

#TAsk 2 plot cov and add points 
windows()
plot(cov, main = '')
plot(X, add = T, cex=0.8)
#Yes, the more cov the more amount of poisonous mushrooms

#Task 3
#lambda is intensety function 
lambda<-npoints(X)/area.owin(X$window)
# 3.5 is observedvnumber of points divided by the window area

#TAsk 4
n_mush <- lambda * 5 * 6
#Yes I can do it because it's homogeneous
# only depends on the areasize,not on its spatial location


#Task 5
alpha = 0.05
#level = 1 - alpha/2
level = alpha
z = qnorm(level)

(z/2-sqrt(X$n))^2
(z/2+sqrt(X$n+1))^2

((z/2-sqrt(X$n))^2)/area.owin(X$window)
((z/2+sqrt(X$n+1))^2)/area.owin(X$window)

fit=ppm(X)
exp(confint(fit))*30

#TAsk 6

chisquare<-quadrat.test(X, nx = 3, ny = 3)
quadrat.test(X, nx=3, ny=3, alternative="clustered")
windows()
plot(chisquare)

#H0 is rejected 
#clustered behaviour, non homogeneous



#Task 7 


l=ppm(X~cov, covariates = list(cov = cov))




