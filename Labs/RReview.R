install.packages("MASS")
library(MASS)
attach(Boston)
?Boston
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
#statistics
summary(Boston$crim)
# DataFrame
days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')

RPI_weather_week <- data.frame(days, temp, snowed)
head(RPI_weather_week)
summary(RPI_weather_week)

