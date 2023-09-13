# MASS Library
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
summary(Boston$crim)

# ISLR Library
install.packages("ISLR")
library(ISLR)
?Auto
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))

# Read 2010EPI
names(data_2020EPI) <- as.matrix(data_2010EPI[1,])
