data("cars")
str(cars)

plot(speed ~ dist, data = cars)

help("lowess")

lowess(cars$speed ~ cars$dist)
lines(lowess(cars$speed ~ cars$dist, f = 2/3), col = 'blue')
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red") # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green") # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5) # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6) # f = 0.01 
