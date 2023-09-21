multivariate <- read.csv("multivariate.csv")
attach(multivariate)
head(multivariate)
help(lm)
mm <- lm(multivariate$Homeowners~multivariate$Immigrant)
mm
summary(mm)$coef
plot(multivariate$Homeowners~multivariate$Immigrant)
help(abline)
abline(mm)
abline(mm, col = 2,, lwd = 3)
# Predictions can be easily made using predict()
# passing through values 0 to 20 values for Immigrants
library(dplyr)
newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm, col = 3, lwd = 3)
attributes(mm)
mm$coefficients

# ggplot examples


