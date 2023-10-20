library(rpart)
install.packages("party")
require(party)
library(stats)
install.packages("rattle")
library(rattle)

data("Titanic")
Titanic

titanic.df <- as.data.frame(Titanic)

# Tree usign rpart 
rpart_tree <- rpart(Survived ~., data = titanic.df)

plot(rpart_tree, uniform = TRUE, main = "Classification Tree for Survival")
text(rpart_tree, use.n = TRUE, all = TRUE, cex = .755)

# Ctree 
ctree <- ctree(Survived ~., data = titanic.df)

plot(ctree, main="Conditional Inference Tree for Survived")

# hclust
h_tree <- hclust(dist(titanic.df))

plot(h_tree)
