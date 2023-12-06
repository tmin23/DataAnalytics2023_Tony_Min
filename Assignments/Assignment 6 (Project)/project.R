library(dplyr)
library(rpart)
library(rpart.plot)
# Cleaning the Vaccine 
vaccine.df <- read.csv("COVID-19_Vaccination_by_Town_and_Race_Ethnicity_-_ARCHIVED.csv")
colnames(vaccine.df) <- c("TownName", "VaccinationStatus", "Race", "Type", "Value", "Date")
summary(vaccine.df)
# Understanding Variables
length(unique(vaccine.df$TownName))
unique(vaccine.df$VaccinationStatus)
unique(vaccine.df$Race)
unique(vaccine.df$Type)
length(unique(vaccine.df$Value))
unique(vaccine.df$Date)
length(unique(vaccine.df$Date))
# Clean the vaccine dataset into the data we choose to use
vaccine.df <- vaccine.df[vaccine.df$VaccinationStatus == 'Fully vaccinated' &
                           vaccine.df$Date == '07/20/2022' &
                           vaccine.df$Type == 'Percentage', ]
dim(vaccine.df)
# remove null values 
vaccine.df <- na.omit(vaccine.df)
dim(vaccine.df)

# Cleaning the Attendance Dataset 
attendance.df <- read.csv("School_Attendance_by_Student_Group_and_District__2021-2022.csv")
colnames(attendance.df) <- c("Code", "DistrictName", "Category", "StudentGroup", 
                          "21-22count", "21-22rate", "20-21count", "20-21rate", 
                          "19-20count", "19-20rate", "ReportPeriod", "Date")
summary(attendance.df)

# Understanding all variables
length(unique(attendance.df$Code))
unique(attendance.df$DistrictName)
unique(attendance.df$Category)
unique(attendance.df$StudentGroup)

# Cleaning the Dataset
attendance.df <- subset(attendance.df, grepl("School District$", DistrictName))
attendance.df <- attendance.df[attendance.df$Category == 'Race/Ethnicity', ]
dim(attendance.df)
attendance.df <- na.omit(attendance.df)
dim(attendance.df)
# Change School districts into just districts
attendance.df$DistrictName <- sub(" School District$", "", attendance.df$DistrictName)

# Combine the datasets
df <- merge(vaccine.df, attendance.df, by.x = "TownName", by.y = "DistrictName", all = TRUE)

# Now with merged datasets lets remove unnecessary features
df <- subset(df, select = -c(Date.x, Code, ReportPeriod, Date.y))

white_df <- subset(df, Race == "NH White" & StudentGroup == "White")
black_df <- subset(df, Race == "NH Black" & StudentGroup == 'Black or African American')
hispanic_df <- subset(df, Race == "Hispanic" & StudentGroup == 'Hispanic/Latino of any race')
all_df <- subset(df, Race == "Total" & StudentGroup == 'All other races')

# Double check for null values 
white_df <- na.omit(white_df)
black_df <- na.omit(black_df)
hispanic_df <- na.omit(hispanic_df)
all_df <- na.omit(all_df)

# Now remove unnecessary features from these separated data frames
white_df <- subset(white_df, select = -c(VaccinationStatus, Race, Type, Category, StudentGroup, 
                                         `21-22count`, `20-21count`, `19-20count`))
black_df <- subset(black_df, select = -c(VaccinationStatus, Race, Type, Category, StudentGroup, 
                                         `21-22count`, `20-21count`, `19-20count`))
hispanic_df <- subset(hispanic_df, select = -c(VaccinationStatus, Race, Type, Category, StudentGroup, 
                                               `21-22count`, `20-21count`, `19-20count`))
all_df <- subset(all_df, select = -c(VaccinationStatus, Race, Type, Category, StudentGroup, 
                                     `21-22count`, `20-21count`, `19-20count`))
# Now multiple rates to be out of 100 percent
white_df <- white_df %>%
  mutate(across(c(`21-22rate`, `20-21rate`, `19-20rate`), ~ round(. * 100, 1)))
black_df <- black_df %>%
  mutate(across(c(`21-22rate`, `20-21rate`, `19-20rate`), ~ round(. * 100, 1)))
hispanic_df <- hispanic_df %>%
  mutate(across(c(`21-22rate`, `20-21rate`, `19-20rate`), ~ round(. * 100, 1)))
all_df <- all_df %>% 
  mutate(across(c(`21-22rate`, `20-21rate`, `19-20rate`), ~ round(. * 100, 1)))

# Linear Regression Model
lr <- lm(`19-20rate` ~ Value, data = all_df)
summary(lr)

plot(all_df$Value, all_df$`19-20rate`,  main = "Scatter Plot with Linear Regression Line", 
     xlab = "Vaccination Percent", ylab = "Attendance Rate")
abline(lr, col = 'red')

# logistic Regression Model
all_df$HighAttendance <- ifelse(all_df$`19-20rate` > 95, 1, 0)
log_reg <- glm(HighAttendance ~ Value, data = all_df, family = 'binomial')
summary(log_reg)
plot(all_df$Value, all_df$HighAttendance, col = 'blue', pch = 16, 
     xlab = "Vaccination Percent", ylab = 'High Attendance (1) or Not (0)')

x_vals <- seq(min(all_df$Value), max(all_df$Value), length.out = 100)
y_probs <- predict(log_reg, newdata = data.frame(Value = x_vals), type = 'response')
lines(x_vals, y_probs, col = 'purple', lwd = 2)

# Decision Tree
tree_model <- rpart(HighAttendance ~ Value, data = all_df, method = 'class')

rpart.plot(tree_model)

predictions <- predict(tree_model, all_df, type = 'class')

table(predictions, all_df$HighAttendance)
# Decision tree classifier
# SVM classier
# Random forest regression 
