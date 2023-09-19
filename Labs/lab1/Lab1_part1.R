library(xls)

EPI_data <- read.csv("2010EPI_data.csv", header = TRUE)
# Change first row to be the header(
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
EPI_data
View(EPI_data)

attach(EPI_data)
fix(EPI_data)
EPI_data
EPI <- EPI_data$EPI
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
E
# Exercise 1: Exploring the Distribution
summary(EPI)
fivenum(EPI, na.rm = TRUE)
stem(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI, na.rm = TRUE, bw  = 1.))
#lines(density(EPI, na.rm = TRUE, bw  = "SJ"))
rug(EPI)
# Exercise 1: Fitting a distribution beyond histograms
# Cumulative density function
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
# Quantile-Quantile
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x <- seq(30, 96, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
# Same Exploration with another 2 variables 
AIR_H <- EPI_data$AIR_H

# Gotta save the plots onto computer too