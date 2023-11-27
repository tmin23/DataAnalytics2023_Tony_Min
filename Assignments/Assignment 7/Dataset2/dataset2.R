library(rpart)
library(rpart.plot)
library(randomForest)

cancer <- read.csv("sobar-72.csv", header = TRUE)
View(cancer)

# EDA
summary(cancer)


summary(cancer$behavior_eating)
boxplot(cancer$behavior_eating, main = "Behavior Eating Boxplot")
hist(cancer$behavior_eating, main = "Behavior Eating Boxplot")

summary(cancer$motivation_strength)
boxplot(cancer$motivation_strength, main = "Motivation_Strength Boxplot")
hist(cancer$motivation_strength, main = "Motivation_Strength Histogram")


summary(cancer$socialSupport_instrumental)
