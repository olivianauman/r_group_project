rm(list=ls())

# Read in Iowa Liquor Sales data and subset into separate 2016 & 2017 CSV files.

df <- read.csv("Iowa_Liquor_Sales.csv")
df$Date <- as.Date(df$Date, "%m/%d/%Y")

df_test <- subset(df, Date >= "2016-01-01" & Date < "2017-01-01")
write.csv(df_test, "Iowa_Liquor_Sales_2016.csv", row.names = FALSE)

NA_count_2016 <- sum(is.na(df_test))

df_test <- subset(df, Date >= "2017-01-01" & Date < "2018-01-01")
write.csv(df_test, "Iowa_Liquor_Sales_2017.csv", row.names = FALSE)

NA_count_2017 <- sum(is.na(df_test))

# Clear previous work, read in the data set(s) of interest, and clean them up.

rm(list=ls())
df_sales <- read.csv("Iowa_Liquor_Sales_2017.csv", na.strings = c("NA", ""))

df_sales$Invoice.Item.Number <- as.numeric(df_sales$Invoice.Item.Number)
df_sales$Date <- as.Date(df_sales$Date, "%m/%d/%Y")
df_sales$Address <- as.character(df_sales$Address)
df_sales$Store.Location <- as.character(df_sales$Store.Location)
df_sales$Item.Number <- as.character(df_sales$Item.Number)
df_sales$Item.Description <- as.character(df_sales$Item.Description)

