set.seed(123)
x <- seq(0, 2 * pi, length.out = 100)
y <- sin(x) + rnorm(100, sd = 0.2)

train_index <- sample(1:100, 70)
train_x <- x[train_index]
train_y <- y[train_index]
test_x <- x[-train_index]
test_y <- y[-train_index]

# SVM 
library(e1071)
svr_model <- svm(train_y ~ train_x, kernel = 'radial', epsilon = 0.1, cost = 1)

predictions <- predict(svr_model, data.frame(train_x = test_x))
# plot the actual and predicted values:
plot(test_x, test_y, main = "SVR Example")
lines(test_x, predictions, col = "red")

