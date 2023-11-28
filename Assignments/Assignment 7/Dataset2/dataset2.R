library(rpart)
library(rpart.plot)
library(randomForest)
library(class)

cancer <- read.csv("sobar-72.csv", header = TRUE)
View(cancer)

# EDA
summary(cancer)

summary(cancer$behavior_eating)
boxplot(cancer$behavior_eating, main = "Behavior Eating Boxplot")
hist(cancer$behavior_eating, main = "Behavior Eating Boxplot")

summary(cancer$intention_commitment)
boxplot(cancer$intention_commitment, main = "Intention Commitment Boxplot")
hist(cancer$intention_commitment, main = "Intention Commitment Histogram")

summary(cancer$attitude_consistency)
boxplot(cancer$attitude_consistency, main = "Attitude Consistency Boxplot")
hist(cancer$attitude_consistency, main = "Attitude Consistency Histogram")

summary(cancer$motivation_strength)
boxplot(cancer$motivation_strength, main = "Motivation_Strength Boxplot")
hist(cancer$motivation_strength, main = "Motivation_Strength Histogram")

summary(cancer$socialSupport_appreciation)
boxplot(cancer$socialSupport_appreciation, main = 'Social Support Appreciation Boxplot')
hist(cancer$socialSupport_appreciation, main = 'Social Support Appreciation Histogram')

summary(cancer$empowerment_desires)
boxplot(cancer$empowerment_desires, main = "Empowerment Desires Boxplot")
hist(cancer$empowerment_abilities, main = "Empowerment Desires Histogram")

# Decision Tree
dim(cancer)
samp_cancer <- sample(1:nrow(cancer), 50)
train_cancer <- cancer[samp_cancer, ]
test_cancer <- cancer[-samp_cancer, ]
dt <- rpart(ca_cervix ~ ., data = train_cancer)
dt
rpart.plot(dt)


# Random Forest
rf <- randomForest(ca_cervix ~ ., data = cancer_train, importance = TRUE)
rf
importance(rf)

# KNN
cancer$ca_cervix[cancer$ca_cervix == 0] <- 'No'
cancer$ca_cervix[cancer$ca_cervix == 1] <- 'Yes'
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
cancer[1:19] <- as.data.frame(lapply(cancer[1:19], normalize))
ind <- sample(2, nrow(cancer), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain <- cancer[ind == 1,]
KNNtest <- cancer[ind == 2,]
sqrt(nrow(cancer))
k <- 9
KNNpred <- knn(train = KNNtrain[1:19], test = KNNtest[1:19], cl = KNNtrain$ca_cervix, k = 9)
KNNpred
table(KNNpred)
