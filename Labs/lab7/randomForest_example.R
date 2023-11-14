install.packages("randomForest")
library(randomForest)

data1 <- read.csv(file.choose(), header = TRUE)
View(data1)

colnames(data1)<- c("BuyingPrice", "Maintenance", "NumDoors", "NumPersons", 
                    "BootSpace", "Safety", "Condition")
head(data1)
str(data1)

levels(data1$Condition)
summary(data1)

set.seed(100)
train <- sample(nrow(data1), 0.7 * nrow(data1), replace = FALSE)
TrainSet <- data1[train, ]
TestSet <- data1[-train, ]

summary(TrainSet)
summary(TestSet)

help(randomForest)
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)


