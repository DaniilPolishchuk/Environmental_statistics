library(spatstat)
load("examPP.Rdata")

#1 ---
windows()
plot(cov)
plot(X, add =T, pch = 19)


#2 ---
W=X$window
plot(W)
area <- area.owin(W)

#3---

lambda<-npoints(X)/area.owin(W)


#4 ---

lambda * (5 * 5) 


#5 ---
exp(-lambda)
1-lambda

#6 ---
GG=Gest(X)
plot(GG$theo, col="blue", ylim=c(0,1), type="l", lty=2, lwd=2)
points(GG$rs, col=2, type="l", lwd=2)
legend("bottomright", col=c("blue","red"), lty=c(2,1), legend=c("Poisson", "Empirical"))


#7 ----


#8 ----
fit=ppm(X)

fit2=ppm(X~cov, covariates = list(cov = cov))

anova(fit, fit2, test="Chisq")
#prefer the inhomogeneous model
AIC(fit);AIC(fit2)
#same