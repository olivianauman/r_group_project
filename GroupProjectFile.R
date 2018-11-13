################
# Read in Iowa Liquor Sales data, subset into separate 2017 CSV file, & clean up.
################

# Note: initially read in 2015-2017 data. This is a very large data set, so we subset to 2017 only instead.

rm(list=ls())

df <- read.csv("Iowa_Liquor_Sales.csv")
df$Date <- as.Date(df$Date, "%m/%d/%Y")
df$Store.Location <- NULL # Remove duplicative address info & latitude/longitude data (beyond the scope of this project).

df_test <- subset(df, Date >= "2017-01-01" & Date < "2018-01-01")
write.csv(df_test, "Iowa_Liquor_Sales_2017.csv", row.names = FALSE)

NA_count_2017 <- sum(is.na(df_test))

### Clears previous work, read in the data subset, and clean it up.

rm(list=ls())
df_sales <- read.csv("Iowa_Liquor_Sales_2017.csv", na.strings = c("NA", ""))

df_sales$Invoice.Item.Number <- as.character(df_sales$Invoice.Item.Number)
df_sales$Date <- as.Date(df_sales$Date)
df_sales$Zip.Code <- factor(df_sales$Zip.Code)
df_sales$Store.Number <- factor(df_sales$Store.Number)
df_sales$County.Number <- factor(df_sales$County.Number)
df_sales$Category <- factor(df_sales$Category)
df_sales$Vendor.Number <- factor(df_sales$Vendor.Number)
df_sales$Item.Number <- factor(df_sales$Item.Number)

### Notes: 
# Kept Item.Number - sometimes the Item.Description is the same but the 
# Item.Number is different for two data points -- for example, if the ml is different.

# Also kept other ID#s in case any of the descriptions are duplicative.

print(str(df_sales))

################
# Read in & clean up Census data.
################

df_census <- read.csv("City_Population_in_Iowa_by_County_and_Year.csv")

df_census$Primary.County.Coordinates <- NULL # Delete column - not needed
df_census$FIPS <- factor(df_census$FIPS)
# Note: left Date as a factor on purpose.

### Keep only the most recent population estimates

## Does the "July 01 2017" Census have ALL Cities? 

# Examine number of unique City/County combinations
unique_city_county_combos <- unique(df_census[c("County", "City")])
unique_city_county_combos <- unique_city_county_combos[order(unique_city_county_combos$County, unique_city_county_combos$City), ]

# Examine Population Data for July 01, 2017
pop_est_2017 <- subset(df_census, Year == "July 01 2017")
pop_est_2017$FIPS <- NULL
pop_est_2017$Year <- NULL
pop_est_2017$Estimate <- NULL
pop_est_2017 <- pop_est_2017[order(pop_est_2017$County, pop_est_2017$City), ]

# Are the two identical?
identical(tolower(pop_est_2017$City), tolower(unique_city_county_combos$City))

# ANSWER: Yes. We can subset to keep only that data.

df_census <- subset(df_census, Year == "July 01 2017")

################
# Question: What types and brands of alcohol are purchased in the highest quantity (by volume)?
################

suppressPackageStartupMessages(library(dplyr))
library(reshape2)

df_sales <- group_by(df_sales, Category.Name)
summ_Category <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))                  
summ_Category <- arrange(summ_Category, desc(Sum_Volume_Sold_Liters))

df_sales <- ungroup(df_sales)

df_sales <- group_by(df_sales, Vendor.Name)
summ_Vendor <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))       
summ_Vendor <- arrange(summ_Vendor, desc(Sum_Volume_Sold_Liters))

df_sales <- ungroup(df_sales)

df_sales <- group_by(df_sales, Item.Description)
summ_Item <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))       
summ_Item <- arrange(summ_Item, desc(Sum_Volume_Sold_Liters))

################
# Question: Which counties buy highest quantity (by volume)?
################

df_sales <- ungroup(df_sales)

df_sales$County <- tolower(df_sales$County)

df_sales <- group_by(df_sales, County)
summ_County <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))       
summ_County <- arrange(summ_County, desc(Sum_Volume_Sold_Liters))

################
# Question: Which cities buy highest quantity (by volume)?
################
df_sales <- ungroup(df_sales)

df_sales <- group_by(df_sales, City)
summ_City <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))       
summ_City <- arrange(summ_City, desc(Sum_Volume_Sold_Liters))

df_sales <- ungroup(df_sales)

################
# Graph of sales by highest volume over time
################

library(ggplot2)
library(scales)

plot <- qplot(Date, Volume.Sold..Liters., data = df_sales, geom = "line")
plot <- plot + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")

ggsave(filename = "plot_dates.png", plot = plot, width = 24, height = 4, dpi = 600)


################
# Per capita liquor volume
################
df_census <- group_by(df_census, County)
county_pop <- summarize(df_census, Sum_County_Pop = sum(Estimate))
df_census <- ungroup(df_census)

county_pop$County <- tolower(county_pop$County)
summ_County$County <- tolower(summ_County$County)

test_merge <- merge(summ_County, county_pop, all.x = TRUE)
test_merge$Vol_Per_Capita <- round(test_merge$Sum_Volume_Sold_Liters / test_merge$Sum_County_Pop, 1)

test_merge <- arrange(test_merge, desc(Vol_Per_Capita))
