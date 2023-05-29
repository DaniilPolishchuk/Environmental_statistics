W<-square(r=5)
plot(W)
area.owin(W)

#create a circular window
W <- disc(radius = 3, centre = c(0, 0))
plot(W)
area.owin(W)

#create a rectangular observation window
W <- owin(xrange = c(0, 500), yrange = c(0, 250), 
          unitname="metres")


X=rpoispp(lambda=100, win=square(2)) 
plot(X, main = "") 


####
g = 40
h = 0.7

W<-square(r=g)
plot(W)
W <- disc(radius = h, centre = c(0, 0))
plot(W)
