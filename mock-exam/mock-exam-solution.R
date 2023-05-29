library(spatstat)
load("examPP.Rdata")

#1
plot(X)
npoints(X) #350
W=X$window
area.owin(W) #100 metres

#2
plot(cov)
plot(X, add=T)
#yes, positive relationship

#3
lambda=npoints(X)/area.owin(W)
lambda
#I expect 3.5 mushrooms per square metre

#4
lambda*30
#I expect 105 mushrooms over an area of 6x5 metres
#Yes I can do it because it's homogeneous, so I only care about the area size

#5
fit=ppm(X)
exp(confint(fit))*30
#94.56; 116.60

#6
quadrat.test(X, nx=3, ny=3)
quadrat.test(X, nx=3, ny=3, alternative="clustered")
#clustered behaviour, non homogeneous

#7
fit2=ppm(X~cov, covariates = list(cov = cov))
coef.vec<-as.numeric(coef(fit2))
location<-c(1,10)#intercept, humidity=10
lambda.fit<-exp(sum(coef.vec*location))
#intensity at humidity=10% is 2.03

#8
anova(fit, fit2, test="Chisq")
#prefer the inhomogeneous model
AIC(fit);AIC(fit2)
#same