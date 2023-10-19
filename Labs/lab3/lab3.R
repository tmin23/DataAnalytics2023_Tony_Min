# set seed for random numbers in matrix
set.seed(12345)
help(par)

par(mar = rep(0.2, 4))
data_matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

# Heatmap
par(mar = rep(0.2, 4))
heatmap(data_matrix)

# Doing random coin flip to add pattern to data 
help('rbinom')
set.seed(678910)
for(i in 1:40) {
  # Flipping coin and getting data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  if(coin_Flip) {
    # if coin is true then add common pattern to row
    data_matrix[i, ] <- data_matrix[i, ] + rep(c(0, 3), each = 5)
  }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

par(mar = rep(0.2, 4))
heatmap(data_matrix)

# Closer took at patterns in rows and columns
hh <- hclust(dist(data_matrix))
data_matrix_ordered <- data_matrix[hh$order,]
par(mfrow = c(1, 3))
image(t(data_matrix_ordered)[,nrow(data_matrix_ordered):1])
plot(rowMeans(data_matrix_ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_matrix_ordered), xlab = "Column", ylab = "Column Mean", pch = 19)


