data("iris")
head(iris)

irisdata1 <- iris[,1:4]
irisdata1

principle_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principle_components)

plot(principle_components)

plot(principle_components, type = 'l')

biplot(principle_components)
