# load packages 
library(ggplot2)


nyc <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20231031.csv")
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
