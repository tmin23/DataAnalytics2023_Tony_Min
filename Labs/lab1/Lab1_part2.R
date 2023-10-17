multivariate <- read.csv("multivariate.csv")
attach(multivariate)
head(multivariate)
help(lm)
mm <- lm(multivariate$Homeowners~multivariate$Immigrant)
mm
summary(mm)$coef
plot(multivariate$Homeowners~multivariate$Immigrant)
help(abline)
abline(mm)
abline(mm, col = 2,, lwd = 3)
# Predictions can be easily made using predict()
# passing through values 0 to 20 values for Immigrants
library(dplyr)
newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm, col = 3, lwd = 3)
attributes(mm)
mm$coefficients

# ggplot examples
library(ggplot2)
head(mtcars)
plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars) # Same Plot
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom = "line") # Same plot

ggplot(pressure, aes(x=temperature, y = pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y = pressure)) + geom_line() + geom_point()
 
# Create Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate bar plot of counts
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
# Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# Creating Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) # approximate number of bins with breaks
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)
                                            
# Creating Box plot using ggplot
plot(ToothGrowth$supp, ToothGrowth$len)

boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")

qplot(supp, len, data = ToothGrowth, geom = "boxplot")

ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")

ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()





