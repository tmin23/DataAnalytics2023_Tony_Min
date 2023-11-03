
vaccine.df <- read.csv("COVID-19_Vaccination_by_Town_and_Race_Ethnicity_-_ARCHIVED.csv")
colnames(vaccine.df) <- c("TownName", "VaccinationStatus", "Race", "Type", "Value", "Date")
summary(vaccine.df)

length(unique(vaccine.df$TownName))
unique(vaccine.df$VaccinationStatus)
unique(vaccine.df$Race)
unique(vaccine.df$Type)
length(unique(vaccine.df$Value))
unique(vaccine.df$Date)
length(unique(vaccine.df$Date))
# We will always use the most updated data which is "02/08/2023" and finding the percentages
# Race_Status
total_fulvac<- vaccine.df[vaccine.df$VaccinationStatus == "Fully vaccinated" & 
                         vaccine.df$Race == "Total" &
                         vaccine.df$Date == "02/08/2023" &
                         vaccine.df$Type == "Percentage",]
summary(total_fulvac)
total_fulvac <- subset(total_fulvac, select = -c(Race, VaccinationStatus, Date, Type))
total_fulvac

attendance <- read.csv("School_Attendance_by_Student_Group_and_District__2021-2022.csv")
colnames(attendance) <- c("Code", "DistrictName", "Category", "StudentGroup", 
                          "21-22count", "21-22rate", "20-21count", "20-21rate", 
                          "19-20count", "19-20rate", "ReportPeriod", "Date")
summary(attendance)

length(unique(attendance$Code))
unique(attendance$DistrictName)
unique(attendance$Category)
unique(attendance$StudentGroup)

# Linear Regerssion
# logistic Regresion
# Decision Tree
# Decision tree classifier
# SVM classier
# Random forest regression 
