library(MASS)
names(iris)
dim(iris)
head(isis)
str(iris)

set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/ 2)

iris_Train <- iris[Train,]
iris_Test <- iris[-Train, ]

fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)

predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class

table <- table(predict1_class, iris_Train$Species)
table

sum(diag(table))/sum(table)
