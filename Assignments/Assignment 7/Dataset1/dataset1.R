library(rpart)
library(rpart.plot)
library(randomForest)

abs <- read.csv(file = 'Absenteeism_at_work.csv', sep = ';', header = TRUE)
View(abs)

# EDA
summary(abs)
summary(abs$Absenteeism.time.in.hours)
par(mfrow = c(1, 2))
boxplot(abs$Absenteeism.time.in.hours, )
hist(abs$Absenteeism.time.in.hours)
hist(abs$Absenteeism.time.in.hours, xlim = c(0, 50), breaks = 100, main = 'Absenteeism Time in Hours ')

boxplot(abs$Reason.for.absence, main = "Reason for Absence Boxplot")
hist(abs$Reason.for.absence, main = "Reason for Absence Histgram")
par(mfrow = c(1, 1))
# Linear Regression 
age <- lm(abs$Absenteeism.time.in.hours ~ abs$Age)
age
summary(age)
plot(abs$Absenteeism.time.in.hours ~ abs$Age, main = "Age Vs. Absence Time")
abline(age, col = 'blue')

dist <- lm(abs$Absenteeism.time.in.hours ~ abs$Distance.from.Residence.to.Work)
dist
summary(dist)
plot(abs$Absenteeism.time.in.hours ~ abs$Distance.from.Residence.to.Work, main = "Distance Vs. Absence Time")
abline(dist, col="red")


# Decision Tree
dim(abs)
sample_abs <- sample(1:nrow(abs), 500)
train_abs <- abs[sample_abs, ]
test_abs <- abs[-sample_abs, ]
dt <- rpart(Absenteeism.time.in.hours ~ ., data = train_abs)
dt
rpart.plot(dt)

# Random Forest
set.seed(100)
train <- sample(nrow(abs), 0.7 * nrow(abs), replace = FALSE)
TrainSet <- abs[train,]
ValidSet <- abs[-train,]
rf <- randomForest(Absenteeism.time.in.hours ~ ., data = TrainSet, importance = TRUE)
rf
importance(rf)
