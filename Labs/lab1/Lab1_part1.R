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
WATER_H <- EPI_data$WATER_H

boxplot(EPI,AIR_H)
qqplot(EPI, AIR_H)

boxplot(EPI, WATER_H)
qqplot(EPI, WATER_H)

# ====================================
# Exercise 2: Filtering
# Reset Data
EPI_data <- read.csv("2010EPI_data.csv")
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
attach(EPI_data)

EPI <- EPI_data$EPI
# Landlock
EPILand <- EPI[!EPI_data$Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob = TRUE)

# no surface water
surfaceWater <- EPI[!EPI_data$No_surface_water]
surfWater <- surfaceWater[!is.na(surfaceWater)]
hist(surfWater)
hist(surfWater, seq(30., 95., 1.0), prob = TRUE)

# Desert 
desert <- EPI[!EPI_data$Desert]
des <- desert[!is.na(desert)]
hist(des)
hist(des, seq(30., 95., 1.0), prob = TRUE)

# High Population Density
HighPopDen <- EPI[!EPI_data$High_Population_Density]
hpd <- HighPopDen[!is.na(HighPopDen)]
hist(hpd)
hist(hpd, seq(30., 95., 1.0), prob = TRUE)

# Filter out Subregion
EPI_South_Asia <- EPI[EPI_data$GEO_subregion == "South Asia"]
sa <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(sa)
#hist(sa, seq(30., 95., 1.0), prob = TRUE)
