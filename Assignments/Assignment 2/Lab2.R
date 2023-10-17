# ----------------------------
# Lab 2 Part 1 (a)
# ----------------------------

# Loading Data
EPI_data <- read.csv("2010EPI_data.csv")
attach(EPI_data)

# Changing first row to be header in R
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
View(EPI_data)

getMode <- function(d, na.rm = FALSE) {
  if (na.rm) {
    d <- na.omit(d)
  }
  uniqv <- unique(d)
  uniqv[which.max(tabulate(match(d, uniqv)))]
}
# Central Tendency Values are Mean, Medium and Mode
# Generate Central Tendency values for AIR_E Variable
AIR_E <- EPI_data$AIR_E
mean(AIR_E, na.rm = TRUE)
median(AIR_E, na.rm = TRUE)
getMode(AIR_E, na.rm = TRUE)

# Generate Central Tendency values for WATER_E Variable
WATER_E <- EPI_data$WATER_E
mean(WATER_E, na.rm = TRUE)
median(WATER_E, na.rm = TRUE)
getMode(WATER_E, na.rm = TRUE)

# Generate Boxplots for AIR_E and WATER_E variables
# Note: Side by side comparison of boxplots
boxplot(AIR_E, WATER_E, names = c("AIR_E", "WATER_E"))

# Generate Central Tendency values for NOX_pt Variable
NOX_PT <- EPI_data$NOX_pt
mean(NOX_PT, na.rm = TRUE)
median(NOX_PT, na.rm = TRUE)
getMode(NOX_PT, na.rm = TRUE)

# Generate Central Tendency values for S02_pt Variable
SO2_PT <- EPI_data$SO2_pt
mean(SO2_PT, na.rm = TRUE)
median(SO2_PT, na.rm = TRUE)
getMode(SO2_PT, na.rm = TRUE)

# Generate boxplots for OZONE_pt and WQI_pt variables
# Note: Side by side comparsion of boxplots
OZONE_PT <- EPI_data$OZONE_pt
WQI_PT <- as.numeric(EPI_data$WQI_pt)
boxplot(OZONE_PT, WQI_PT, names = c("OZONE_PT", "WQI_PT"))

# Generate Central Tendency Values for Climate variables
CLIMATE <- EPI_data$CLIMATE
mean(CLIMATE, na.rm = TRUE)
median(CLIMATE, na.rm = TRUE)
getMode(CLIMATE, na.rm = TRUE)

# Generate Central Tendency Values for Agriculture variables
AGRICULTURE <- EPI_data$AGRICULTURE
mean(AGRICULTURE, na.rm = TRUE)
median(AGRICULTURE, na.rm = TRUE)
getMode(AGRICULTURE, na.rm = TRUE)

# Generate boxplots for FISHERIES_pt and NMVOC_pt variables 
# Note: Side by side comparsion of boxplots
FISH <- as.numeric(EPI_data$FISHERIES)
NMVOC <- EPI_data$NMVOC_pt
boxplot(FISH, NMVOC, names = c("FISHERIES_pt", "NMVOC_pt"))

# ----------------------------
# Lab 2 Part 1 (b)
# ----------------------------

# Loading Data
EPI_data <- read.csv("2010EPI_data.csv")
attach(EPI_data)

# Changing first row to be header in R
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
View(EPI_data)

boxplot(EPI_data$ENVHEALTH, 
        EPI_data$DALY, 
        EPI_data$AIR_H,
        EPI_data$WATER_H)
lmENV <- 
  lm(EPI_data$ENVHEALTH~ EPI_data$DALY + EPI_data$AIR_H + EPI_data$WATER_H)

lmENV
summary(lmENV)
cENVH <- coef(lmENV)

DALYNEW <- c(seq(5, 95, 5))
AIR_HNEW <- c(seq(5, 95, 5))
WATER_HNEW <- c(seq(5, 95, 5))
NEW <- 
  data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
pENV <- 
  predict(lmENV, NEW, interval = "prediction")
cENV <- 
  predict(lmENV, NEW, interval = "confidence")


Model1 <- lm(EPI_data$AIR_E~EPI_data$DALY +
               EPI_data$AIR_H + 
               EPI_data$WATER_H)
model1pred <- predict(Model1, NEW, interval = "prediction")
model1conf <- predict(Model1, NEW, interval = "confidence")

Model2 <- lm(EPI_data$CLIMATE ~ EPI_data$DALY + 
               EPI_data$AIR_H + 
               EPI_data$WATER_H)
model2pred <- predict(Model2, NEW, interval = "prediction")
model2conf <- predict(Model2, NEW, interval = "confidence")

# ----------------------------
# Lab 2 Part 1 (c)
# ----------------------------

# Loading Data
EPI_data <- read.csv("2010EPI_data.csv")
attach(EPI_data)

# Changing first row to be header in R
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))
View(EPI_data) 

shapiro.test(EPI_data$ENVHEALTH)
shapiro.test(EPI_data$ECOSYSTEM)

# ENVHEALTH 
ENV <- na.omit(data.frame(EPI_data$ENVHEALTH))
dim(ENV)
shapiro.test(EPI_data$ENVHEALTH)
# dimension of 163 which is between 3 and 5000 
# p-value of 8.178e-10 therefore we reject null hypothesis 
# this data is likely not to be normally distributed 

# DALY
DALY <- na.omit(data.frame(EPI_data$DALY))
dim(DALY)
shapiro.test(EPI_data$DALY)
# Dimensions are 163 which is between 3 and 5000 
# p-value of 1.523e-6 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# AIR_H
AIR <- na.omit(data.frame(EPI_data$AIR_H))
dim(AIR)
shapiro.test(EPI_data$AIR_H)
# Dimensions are 163 which is between 3 and 5000 
# p-value of 3.206e-7 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# WATER_H
WATER <- na.omit(data.frame(EPI_data$WATER_H))
dim(WATER)
shapiro.test(EPI_data$WATER_H)
# Dimensions are 163 which is between 3 and 5000 
# p-value of 1.348e-10 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# Now use Data from EPI.csv 
# Load new data
EPI_data <- read.csv("EPI_Data.csv")
attach(epi_data)

# ENVHEALTH 
ENV <- na.omit(data.frame(EPI_data$ENVHEALTH))
dim(ENV)
shapiro.test(EPI_data$ENVHEALTH)
# dimension of 182 which is between 3 and 5000 
# p-value of 1.083e-8 therefore we reject null hypothesis 
# this data is likely not to be normally distributed 

# DALY
DALY <- na.omit(data.frame(EPI_data$DALY))
dim(DALY)
shapiro.test(EPI_data$DALY)
# Dimensions are 192 which is between 3 and 5000 
# p-value of 1.891e-7 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# AIR_H
AIR <- na.omit(data.frame(EPI_data$AIR_H))
dim(AIR)
shapiro.test(EPI_data$AIR_H)
# Dimensions are 197 which is between 3 and 5000 
# p-value of 8.994e-9 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# WATER_H
WATER <- na.omit(data.frame(EPI_data$WATER_H))
dim(WATER)
shapiro.test(EPI_data$WATER_H)
# Dimensions are 197 which is between 3 and 5000 
# p-value of 1.679e-12 therefore we reject null hypothesis
# this data is likely not to be normally distributed

# ----------------------------
# Lab 2 Part 2 (a) Regression
# ----------------------------
multivariate <- read.csv("dataset_multipleRegression.csv")
attach(multivariate)
# lm(Dependent Variable ~ independent variable)
mm <- lm(ROLL ~ UNEM + HGRAD, data = multivariate)

new <- data.frame(UNEM = c(7), HGRAD = c(90000))
predict(mm, newdata = new)

# Adding Per Capita Income
mm <- lm(ROLL ~ UNEM + HGRAD + INC, data = multivariate)

new <- data.frame(UNEM = c(7), HGRAD = c(90000), INC = c(25000))
predict(mm, newdata = new)

# ----------------------------
# Lab 2 Part 2 (b) Classification
# ----------------------------

library(class)
abalone <- read.csv("abalone.csv")
attach(abalone)
aba <- abalone

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# remove sex column since knn requires all numeric numbers
aba$Sex <- NULL

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))

#Split data into training and testing sets
ind <- sample(2, nrow(aba), replace = TRUE, prob = c(0.7, 0.3))
knntrain <- aba[ind== 1, ]
knntest <- aba[ind == 2, ]

k <- 55
knnpred <- knn(train = knntrain[1:7], test = knntest[1:7], cl = knntrain$Rings, k)
knnpred
table(knnpred)

# ----------------------------
# Lab 2 Part 2 (c) Clustering
# ----------------------------

library(ggplot2)
head(iris)

sapply(iris[,-5], var)
summary(iris)
# Plot Sepal Length Vs. Sepal Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
# Plot Petal Length Vs. Sepal Width
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()

set.seed(300)
k.max <- 12
wss <- sapply(1:k.max, function(k) {
  kmeans(iris[,3:4], k, nstart = 20, iter.max = 20)$tot.withinss
})

wss

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

icluster <- kmeans(iris[,3:4], 3, nstart = 20)
table(iris[,5], icluster$cluster)
