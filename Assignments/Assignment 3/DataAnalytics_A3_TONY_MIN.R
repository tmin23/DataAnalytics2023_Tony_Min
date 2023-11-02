# Boxplots
# Load Data
nyt7 = read.csv("Data/nyt7.csv")
nyt13 = read.csv("Data/nyt13.csv")
nyt14 = read.csv("Data/nyt14.csv")
nyt15 = read.csv("Data/nyt15.csv")
nyt17 = read.csv("Data/nyt17.csv")
nyt23 = read.csv("Data/nyt23.csv")
nyt29 = read.csv("Data/nyt29.csv")


boxplot(nyt7$Age, nyt13$Age, nyt14$Age,
        nyt15$Age, nyt17$Age, nyt23$Age, 
        nyt29$Age, 
        names = c("7", "13", "14", "15", "17", "23", "29"),
        main = "Age", xlab = "Dataset", ylab = "Age")

boxplot(nyt7$Impressions, nyt13$Impressions, 
        nyt14$Impressions, nyt15$Impressions, 
        nyt17$Impressions, nyt23$Impressions, 
        nyt29$Impressions, 
        names = c("7", "13", "14", "15", "17", "23", "29"),
        main = "Impressions", xlab = "Dataset", ylab = "Impressions")

# Anderson Darling Normality Test
install.packages("nortest")
library(nortest)
# > 0.05 accept null hypothesis follows normal distribution
print(ad.test(nyt7$Age)$p.value > 0.05)
print((ad.test(nyt7[nyt7$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt13$Age)$p.value > 0.05)
print((ad.test(nyt13[nyt13$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt14$Age)$p.value > 0.05)
print((ad.test(nyt14[nyt14$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt15$Age)$p.value > 0.05)
print((ad.test(nyt15[nyt15$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt17$Age)$p.value > 0.05)
print((ad.test(nyt17[nyt17$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt23$Age)$p.value > 0.05)
print((ad.test(nyt23[nyt23$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt29$Age)$p.value > 0.05)
print((ad.test(nyt29[nyt29$Age > 0,]$Age))$p.value > 0.05)


print(ad.test(nyt7$Impressions)$p.value > 0.05)
print(ad.test(nyt13$Impressions)$p.value > 0.05)
print(ad.test(nyt14$Impressions)$p.value > 0.05)
print(ad.test(nyt15$Impressions)$p.value > 0.05)
print(ad.test(nyt17$Impressions)$p.value > 0.05)
print(ad.test(nyt23$Impressions)$p.value > 0.05)
print(ad.test(nyt29$Impressions)$p.value > 0.05)
# ALL are not normally distributed
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

hist7 <- ggplot(data=nyt7, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 7") 
hist13 <- ggplot(data=nyt13, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 13") 
hist14 <- ggplot(data=nyt14, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 14") 
hist15 <- ggplot(data=nyt15, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 15") 
hist17 <- ggplot(data=nyt17, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 17")
hist23 <- ggplot(data=nyt23, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 23") 
hist29 <- ggplot(data=nyt29, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 29")

ages <- ggarrange(hist7, hist13, hist14,
                  hist15, hist17, hist23, hist29)
annotate_figure(ages, top = text_grob("Age Histograms",
                                      color = "blue", face = "bold", size = 10))
# Ages without zeros
hist7 <- ggplot(data=nyt7[nyt7$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 7") 
hist13 <- ggplot(data=nyt13[nyt13$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 13")
hist14 <- ggplot(data=nyt14[nyt14$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 14") 
hist15 <- ggplot(data=nyt15[nyt15$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 15") 
hist17 <- ggplot(data=nyt17[nyt17$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 17") 
hist23 <- ggplot(data=nyt23[nyt23$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 23") 
hist29 <- ggplot(data=nyt29[nyt29$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 29")

age_no_zero <- ggarrange(hist7, hist13, hist14,
                  hist15, hist17, hist23, hist29)
annotate_figure(age_no_zero, top = text_grob("Age Histograms (No Zero)",
                                      color = "blue", face = "bold", size = 10))
# Impression Histograms
hist7 <- ggplot(data=nyt7, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 7") 
hist13 <- ggplot(data=nyt13, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 13") 
hist14 <- ggplot(data=nyt14, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 14") 
hist15 <- ggplot(data=nyt15, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 15") 
hist17 <- ggplot(data=nyt17, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 17")
hist23 <- ggplot(data=nyt23, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 23") 
hist29 <- ggplot(data=nyt29, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 29")

impres <- ggarrange(hist7, hist13, hist14,
                         hist15, hist17, hist23, hist29)
annotate_figure(impres, top = text_grob("Impressions Histograms",
                                             color = "green", face = "bold", size = 10))

# Empirical Cumulative Distribution Function for Age
ecdf7 <- ggplot(data=nyt7[nyt7$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 7")
ecdf13 <- ggplot(data=nyt13[nyt13$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 13")
ecdf14 <- ggplot(data=nyt14[nyt14$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 14")
ecdf15 <- ggplot(data=nyt15[nyt15$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 15")
ecdf17 <- ggplot(data=nyt17[nyt17$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 17")
ecdf23 <- ggplot(data=nyt23[nyt23$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 23")
ecdf29 <- ggplot(data=nyt29[nyt29$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 29")

Age_ecdf <- ggarrange(ecdf7, ecdf13, ecdf14,
                      ecdf15, ecdf17, ecdf23, ecdf29)
annotate_figure(Age_ecdf, top = text_grob("Age ECDF",
                                        color = "blue", face = "bold", size = 10))
# ECDF for Impressions
ecdf7 <- ggplot(data=nyt7, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 7")
ecdf13 <- ggplot(data=nyt13, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 13")
ecdf14 <- ggplot(data=nyt14, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 14")
ecdf15 <- ggplot(data=nyt15, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 15")
ecdf17 <- ggplot(data=nyt17, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 17")
ecdf23 <- ggplot(data=nyt23, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 23")
ecdf29 <- ggplot(data=nyt29, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 29")

impres_ecdf <- ggarrange(ecdf7, ecdf13, ecdf14,
                      ecdf15, ecdf17, ecdf23, ecdf29)
annotate_figure(impres_ecdf, top = text_grob("Impressions ECDF",
                                          color = "blue", face = "bold", size = 10))

# QQ Plot for Age
qq7 <- ggplot(data=nyt7[nyt7$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 7")
qq13 <- ggplot(data=nyt13[nyt13$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 13")
qq14 <- ggplot(data=nyt14[nyt14$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 14")
qq15 <- ggplot(data=nyt15[nyt15$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 15")
qq17 <- ggplot(data=nyt17[nyt17$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 17")
qq23 <- ggplot(data=nyt23[nyt23$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 23")
qq29 <- ggplot(data=nyt29[nyt29$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 29")

age_qq <- ggarrange(qq7, qq13, qq14, qq15, qq17, qq23, qq29)
annotate_figure(age_qq, top = text_grob("Age QQ-Plots",
                                             color = "purple", face = "bold", size = 10))

# QQ Plot for Impressions
qq7 <- ggplot(data=nyt7, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 7")
qq13 <- ggplot(data=nyt13, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 13")
qq14 <- ggplot(data=nyt14, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 14")
qq15 <- ggplot(data=nyt15, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 15")
qq17 <- ggplot(data=nyt17, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 17")
qq23 <- ggplot(data=nyt23, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 23")
qq29 <- ggplot(data=nyt29, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 29")

impres_qq <- ggarrange(qq7, qq13, qq14, qq15, qq17, qq23, qq29)
annotate_figure(impres_qq, top = text_grob("Impressions QQ-Plots",
                                             color = "red", face = "bold", size = 10))

# Significance test

lin_model <- lm(nyt7$Age[nyt7$Age > 0] ~ nyt7$Impressions[nyt7$Age > 0])
lin_model
plot(nyt7$Age[nyt7$Age > 0]~nyt7$Impressions[nyt7$Age > 0], xlab="Impressions", ylab="Age", main="Age vs. Impressions")
abline(lin_model, col="blue", lwd=2)







# 6000 Level Question Code
# Choosing 4 and reloading data
nyt7 = read.csv("Data/nyt7.csv")
nyt13 = read.csv("Data/nyt13.csv")
nyt14 = read.csv("Data/nyt14.csv")
nyt15 = read.csv("Data/nyt15.csv")

# Filter data sets
nyt7 <- nyt7[nyt7$Age > 0 & nyt7$Impressions > 0, ]
nyt13 <- nyt13[nyt13$Age > 0 & nyt13$Impressions > 0, ]
nyt14 <- nyt14[nyt14$Age > 0 & nyt14$Impressions > 0, ]
nyt15 <- nyt15[nyt15$Age > 0 & nyt15$Impressions > 0, ]

# Normality Test 
ad.test(nyt7$Age)$p.value 
ad.test(nyt7$Impressions)$p.value
ad.test(nyt13$Age)$p.value
ad.test(nyt13$Impressions)$p.value
ad.test(nyt14$Age)$p.value
ad.test(nyt14$Impressions)$p.value
ad.test(nyt15$Age)$p.value
ad.test(nyt15$Impressions)$p.value

# Creating Histograms for Age
par(mfrow = c(2, 2))
hist(nyt7$Age, main = "Dataset 7", xlab = "Age", col = 'blue')
hist(nyt13$Age, main = "Dataset 13", xlab = "Age", col = 'red')
hist(nyt14$Age, main = "Dataset 14", xlab = "Age", col = 'green')
hist(nyt15$Age, main = 'Dataset 15', xlab = "Age", col = 'yellow')

# Creating Histograms for Impressions
hist(nyt7$Impressions, main = "Dataset 7", xlab = "Impressions", col = 'blue')
hist(nyt13$Impressions, main = "Dataset 13", xlab = "Impressions", col = 'red')
hist(nyt14$Impressions, main = "Dataset 14", xlab = "Impressions", col = 'green')
hist(nyt15$Impressions, main = 'Dataset 15', xlab = "Impressions", col = 'yellow')

# Plot ECDFs for Age
nyt7_ecdf <- ecdf(nyt7$Age)
plot(nyt7_ecdf, main = "ECDF for Age (Dataset 7)", xlab = "Age", ylab = "ECDF")
abline(h = 0.5, col = "red", lty = 2)
nyt13_ecdf <- ecdf(nyt13$Age)
plot(nyt13_ecdf, main = "ECDF for Age (Dataset 13)", xlab = "Age", ylab = "ECDF")
abline(h = 0.5, col = "red", lty = 2)
nyt14_ecdf <- ecdf(nyt14$Age)
plot(nyt14_ecdf, main = "ECDF for Age (Dataset 14)", xlab = "Age", ylab = "ECDF")
abline(h = 0.5, col = "red", lty = 2)
nyt15_ecdf <- ecdf(nyt15$Age)
plot(nyt15_ecdf, main = "ECDF for Age (Dataset 15)", xlab = "Age", ylab = "ECDF")
abline(h = 0.5, col = "red", lty = 2)

# Plot ECDFs for Impressions
nyt7_ecdf <- ecdf(nyt7$Impressions)
plot(nyt7_ecdf, main = "ECDF for Impressions (Dataset 7)", xlab = "Impressions", ylab = "ECDF")
abline(h = 0.5, col = "purple", lty = 2)
nyt13_ecdf <- ecdf(nyt13$Impressions)
plot(nyt13_ecdf, main = "ECDF for Impressions (Dataset 13)", xlab = "Impressions", ylab = "ECDF")
abline(h = 0.5, col = "purple", lty = 2)
nyt14_ecdf <- ecdf(nyt14$Impressions)
plot(nyt14_ecdf, main = "ECDF for Impressions (Dataset 14)", xlab = "Impressions", ylab = "ECDF")
abline(h = 0.5, col = "purple", lty = 2)
nyt15_ecdf <- ecdf(nyt15$Impressions)
plot(nyt15_ecdf, main = "ECDF for Impressions (Dataset 15)", xlab = "Impressions", ylab = "ECDF")
abline(h = 0.5, col = "purple", lty = 2)

# Plot QQ distribution for Age * Normal Distribution
nyt7_qq <- qnorm(ppoints(nyt7$Age))
qqplot(nyt7_qq, nyt7$Age, main = "QQ for Age (Dataset 7)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt13_qq <- qnorm(ppoints(nyt13$Age))
qqplot(nyt13_qq, nyt13$Age, main = "QQ for Age (Dataset 13)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt14_qq <- qnorm(ppoints(nyt14$Age))
qqplot(nyt14_qq, nyt14$Age, main = "QQ for Age (Dataset 14)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt15_qq <- qnorm(ppoints(nyt15$Age))
qqplot(nyt15_qq, nyt15$Age, main = "QQ for Age (Dataset 15)", xlab = "Theoretical Quantiles", ylab = "QQ")

# Plot QQ Distribution for Impressions * Normal Distribution
nyt7_qq <- qnorm(ppoints(nyt7$Impressions))
qqplot(nyt7_qq, nyt7$Impressions, main = "QQ for Impressions (Dataset 7)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt13_qq <- qnorm(ppoints(nyt13$Impressions))
qqplot(nyt13_qq, nyt13$Impressions, main = "QQ for Impressions (Dataset 13)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt14_qq <- qnorm(ppoints(nyt14$Impressions))
qqplot(nyt14_qq, nyt14$Impressions, main = "QQ for Impressions (Dataset 14)", xlab = "Theoretical Quantiles", ylab = "QQ")
nyt15_qq <- qnorm(ppoints(nyt15$Impressions))
qqplot(nyt15_qq, nyt15$Impressions, main = "QQ for Impressions (Dataset 15)", xlab = "Theoretical Quantiles", ylab = "QQ")

# Linear Regression
linear_model1 <- lm(nyt7$Age ~ nyt7$Impressions)
linear_model1 
linear_model2 <- lm(nyt13$Age ~ nyt13$Impressions)
linear_model2 
linear_model3 <- lm(nyt14$Age ~ nyt14$Impressions)
linear_model3 
linear_model4 <- lm(nyt15$Age ~ nyt15$Impressions)
linear_model4

plot(nyt7$Age ~ nyt7$Impressions, xlab="Impressions", ylab="Age", main="Age vs. Impressions (Dataset 7)")
abline(linear_model1, col="blue", lwd=2)
plot(nyt13$Age ~ nyt13$Impressions, xlab="Impressions", ylab="Age", main="Age vs. Impressions (Dataset 13)")
abline(linear_model2, col = 'red', lwd = 2)
plot(nyt14$Age ~ nyt14$Impressions, xlab="Impressions", ylab="Age", main="Age vs. Impressions (Dataset 14)")
abline(linear_model3, col = 'green', lwd = 2)
plot(nyt15$Age ~ nyt15$Impressions, xlab="Impressions", ylab="Age", main="Age vs. Impressions (Dataset 15)")
abline(linear_model4, col = 'purple', lwd = 2)

