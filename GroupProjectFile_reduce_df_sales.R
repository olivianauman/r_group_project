##############################################################################
##############################################################################
#                          R GROUP PROJECT BULK FILE
##############################################################################
##############################################################################

# CLEAR ENVIRONMENT
rm(list=ls())

##############################################################################
#                           DATA CLEANSING
##############################################################################

# REDUCE SIZE OF MAIN DATAFRAME
#Read in larger dataset
df <- read.csv("Iowa_Liquor_Sales.csv")

#Change Date format
df$Date <- as.Date(df$Date, "%m/%d/%Y")

#Remove duplicative address info & latitude/longitude data (beyond the scope of this project).
df$Store.Location <- NULL 

#Grab only Dates from 2017
df_test <- subset(df, Date >= "2017-01-01" & Date < "2018-01-01")

#Make this a new csv file to use in next step
write.csv(df_test, "Iowa_Liquor_Sales_2017.csv", row.names = FALSE)

#Examine count of NAs
NA_count_2017 <- sum(is.na(df_test))