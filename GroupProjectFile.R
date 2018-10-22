rm(list=ls())

df <- read.csv("Iowa_Liquor_Sales.csv")

df$Invoice.Item.Number <- as.numeric(df$Invoice.Item.Number)
df$Date <- as.Date(df$Date, "%m/%d/%Y")
df$Address <- as.character(df$Address)
df$Store.Location <- as.character(df$Store.Location)
df$Item.Number <- as.character(df$Item.Number)
df$Item.Description <- as.character(df$Item.Description)
df$Year <- as.Date(df$Year, "%m/%d/%Y")
df_test <- subset(df, Date >= "2015-01-01" & Date < "2018-01-01")

write.csv(df_test, "Iowa_Liquor_Sales_2015-2017.csv", row.names = FALSE)
