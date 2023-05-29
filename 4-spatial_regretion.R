library(spatialreg)

#Trend of coefficiaent of variation 
cvs <- numeric(6)

cvs[1] <- sd(swc$Y04) / mean(swc$Y04)
cvs[2] <- sd(swc$Y05) / mean(swc$Y05)
cvs[3] <- sd(swc$Y06) / mean(swc$Y06)
cvs[4] <- sd(swc$Y07) / mean(swc$Y07)
cvs[5] <- sd(swc$Y08) / mean(swc$Y08)
cvs[6] <- sd(swc$Y09) / mean(swc$Y09)

plot(cvs)

#beta-convergence model 

y_0 <- swc$Y04
y_t <- swc$Y09

plot(log(y_0), log(y_t/ y_0), pch= 19)

response <- log(y_t/ y_0)
covariate <-  log(y_0)

#log(yi1/yi0) = alpha + beta * log(yo) + epsilon_i


covergence_lm <- lm(response ~ covariate)

summary(covergence_lm)

plot(covergence_lm)

swc$residual <- covergence_lm$residuals

tm_shape(swc)+
  tm_borders()+
  tm_fill("residual")

#test for the spatial autocorrelation of the residuals 

lm.morantest(covergence_lm, wlist_til, alternative = "two.sided")
