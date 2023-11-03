# load packages 
library(ggplot2)
library(stats)
library(dplyr)

nyc <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20231102.csv")
View(nyc)

# Clean up nyc dataset for specific Borough
nyc <- nyc[nyc$BOROUGH == 'Brooklyn' | nyc$BOROUGH == '1',]
nyc$LAND.SQUARE.FEET <- gsub(",", '', as.character(nyc$LAND.SQUARE.FEET))
nyc$LAND.SQUARE.FEET <- as.numeric(nyc$LAND.SQUARE.FEET)
nyc$GROSS.SQUARE.FEET <- gsub(",", '', as.character((nyc$GROSS.SQUARE.FEET)))
nyc$GROSS.SQUARE.FEET <- as.numeric(nyc$GROSS.SQUARE.FEET)
nyc <- nyc[nyc$GROSS.SQUARE.FEET > 0 & !is.na(nyc$GROSS.SQUARE.FEET), ]
nyc <- nyc[nyc$LAND.SQUARE.FEET > 0 & !is.na(nyc$LAND.SQUARE.FEET), ]
nyc <- nyc[nyc$YEAR.BUILT > 0 & !is.na(nyc$YEAR.BUILT), ]
nyc <- nyc[nyc$SALE.PRICE > 0 & !is.na(nyc$SALE.PRICE), ]
View(nyc)

par(mfrow = c(2, 2))
summary(nyc$SALE.PRICE)
hist(nyc$SALE.PRICE, xlim = c(0, 2500000), breaks = 10000, main = "Sale Price Histogram", xlab = "Sale Price")

summary(nyc$GROSS.SQUARE.FEET)
hist(nyc$GROSS.SQUARE.FEET, xlim = c(0, 240000), breaks = 1000, main = "Gross Square Feet Histogram", xlab = "Gross Square Feet")

summary(nyc$YEAR.BUILT)
hist(nyc$YEAR.BUILT, xlim = c(1800, 2050), breaks = 10, main = "Year Built Histogram", xlab = "Year Built")

# Finding Outliers Using Cooks Distance
df <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$YEAR.BUILT)
colnames(df) <- c("SalePrice", "GrossSquareFeet", "YearBuilt")
df <- na.omit(df) # remove null values
model <- lm(SalePrice ~ GrossSquareFeet + YearBuilt, data = df)
summary(model)
cd <- cooks.distance(model)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
nameofinfluential <- names(influential)
nameofinfluential
outliers <- df[nameofinfluential,]
df_without_outliers <- df %>% anti_join(outliers)
model2 <- lm(SalePrice ~ GrossSquareFeet + YearBuilt, data = df_without_outliers)
summary(model2)

# Finding Outliers using IQR
Q1 <- quantile(df$SalePrice, 0.25)
Q3 <- quantile(df$SalePrice, 0.75)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- df[df$SalePrice < lower_bound | df$SalePrice > upper_bound, ]
outliers

# Multivariate Regression
multi <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$LAND.SQUARE.FEET)
colnames(multi) <- c("SalePrice", "GrossSquareFeet", "LandSquareFeet")
multi

model_sale_gross_land <- lm(SalePrice ~ GrossSquareFeet + LandSquareFeet, data = multi)
summary(model_sale_gross_land)


model_sale_gross <- lm(SalePrice ~ GrossSquareFeet, data = multi)
summary(model_sale_gross)
plot(SalePrice~GrossSquareFeet, data = multi)
abline(model_sale_gross, col = 'blue')

model_sale_land <- lm(SalePrice ~ LandSquareFeet, data = multi)
summary(model_sale_land)
plot(SalePrice~LandSquareFeet, data = multi)
abline(model_sale_land, col = 'red')

# Decision Tree
library(rpart)
library(rattle)

tree <- data.frame(nyc$SALE.PRICE, nyc$YEAR.BUILT)
colnames(tree) <- c("SalePrice", "YearBuilt")
tree <- na.omit(tree)
dec_tree <- rpart(SalePrice~., data = tree)
fancyRpartPlot(dec_tree)

rpart.plot(dec_tree, uniform = TRUE, box.palette = "Blues", main = "Decision Tree for Sale Price")

# Multivariate Regression Question 2
df <- data.frame(nyc$SALE.PRICE, nyc$GROSS.SQUARE.FEET, nyc$YEAR.BUILT)
colnames(df) <- c("SalePrice", "GrossSquareFeet", "YearBuilt")

#df$SalePrice <- (df$SalePrice - mean(df$SalePrice)) / sd(df$SalePrice)
#df$GrossSquareFeet <- (df$GrossSquareFeet - mean(df$GrossSquareFeet)) / sd(df$GrossSquareFeet)
#df$YearBuilt <- (df$YearBuilt - mean(df$YearBuilt)) / sd(df$YearBuilt)

model <- lm(SalePrice ~ GrossSquareFeet + YearBuilt, data = df)
summary(model)

model2 <- lm(SalePrice ~ YearBuilt, data = df)
summary(model2)
