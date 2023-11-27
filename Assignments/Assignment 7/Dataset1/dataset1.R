library(rpart)
library(rpart.plot)
library(randomForest)

abs <- read.csv(file = 'Absenteeism_at_work.csv', sep = ';', header = TRUE)
View(abs)

# EDA
summary(abs)
summary(abs$Absenteeism.time.in.hours)

boxplot(abs$Absenteeism.time.in.hours)
hist(abs$Absenteeism.time.in.hours)
hist(abs$Absenteeism.time.in.hours, xlim = c(0, 50), breaks = 100)

# Linear Regression 
age <- lm(abs$Absenteeism.time.in.hours ~ abs$Age)
age
summary(age)
plot(abs$Absenteeism.time.in.hours ~ abs$Age)
abline(age, col = 'blue')

dist <- lm(abs$Absenteeism.time.in.hours ~ abs$Distance.from.Residence.to.Work)
dist
summary(dist)
plot(abs$Absenteeism.time.in.hours ~ abs$Distance.from.Residence.to.Work)
abline(dist, col="red")


# Decision Tree
tree_abs <- rpart(Absenteeism.time.in.hours ~ ., data = abs)
tree_abs
rpart.plot(tree_abs)

# Random Forest 
set.seed(100)
train <- sample(nrow(abs), 0.7 * nrow(abs), replace = FALSE)
TrainSet <- abs[train,]
ValidSet <- abs[-train,]
rf <- randomForest(Absenteeism.time.in.hours ~ ., data = TrainSet, importance = TRUE)
rf
importance(rf)
