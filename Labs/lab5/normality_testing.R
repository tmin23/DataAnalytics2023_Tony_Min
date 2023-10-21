# Normality Distribution Tests

help("rnorm")
set.seed(10)
data1 <- rnorm(50)
data1
set.seed(30)
data2 <- rnorm(50)

# Shapiro-Wilk Normality Test
shapiro.test(data1)
hist(data1, col = 'green')

shapiro.test(data2)
hist(data2, col = 'steelblue')

# Poisson Distribution
set.seed(0)
data <- rpois(n = 100, lambda = 3)
shapiro.test(data)
hist(data, col = 'yellow')


# Aderson-Darling test
library(nortest)
set.seed(1)
x <- rnorm(100, 0, 1)

ad.test(x)
