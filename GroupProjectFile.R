##############################################################################
##############################################################################
#                          R GROUP PROJECT BULK FILE
##############################################################################
##############################################################################

# CLEAR ENVIRONMENT
rm(list=ls())

# LOAD NECESSARY LIBRARIES
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)


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

  #Clear the environment and continue
rm(list=ls())

# LOAD ALL DATA FRAMES
  #Liquor sales df
df_sales <- read.csv("Iowa_Liquor_Sales_2017.csv", na.strings = c("NA", ""))
  #Census df
df_census <- read.csv("County_Population_in_Iowa_by_Year.csv")

# CLEANSE LIQUOR SALES DF
df_sales$County <- tolower(df_sales$County)
df_sales$Invoice.Item.Number <- as.character(df_sales$Invoice.Item.Number)
df_sales$Date <- as.Date(df_sales$Date)
df_sales$Zip.Code <- factor(df_sales$Zip.Code)
df_sales$Store.Number <- factor(df_sales$Store.Number)
df_sales$County.Number <- factor(df_sales$County.Number)
df_sales$County <- factor(df_sales$County)
df_sales$Category <- factor(df_sales$Category)
df_sales$Vendor.Number <- factor(df_sales$Vendor.Number)
  #Kept this...sometimes Item.Description is the same but Item.Number is different for two data points
df_sales$Item.Number <- factor(df_sales$Item.Number)
  #Add levels
levels(df_sales$County) <- c(levels(df_sales$County), "buena vista", "cerro gordo", "o'brien", "pottawattamie")
  #Overwrite levels with accurate names
df_sales$County[11] <- "buena vista"
df_sales$County[17] <- "cerro gordo"
df_sales$County[71] <- "o'brien"
df_sales$County[78] <- "pottawattamie"
  #Remove inaccurate level names
levels(df_sales$County)[levels(df_sales$County) == "buena vist"] <- "buena vista"
levels(df_sales$County)[levels(df_sales$County) == "cerro gord"] <- "cerro gordo"
levels(df_sales$County)[levels(df_sales$County) == "obrien"] <- "o'brien"
levels(df_sales$County)[levels(df_sales$County) == "pottawatta"] <- "pottawattamie"


# CLEANSE CENSUS DF
  #column not needed; delete
df_census$Primary.County.Coordinates <- NULL 
df_census$FIPS <- factor(df_census$FIPS)
  #Note: left Date as a factor on purpose.

  #Examine Population Data for July 01, 2017 to ensure the 
  # "July 01 2017" Census includes data for all counties.
pop_est_2017 <- subset(df_census, Year == "July 01 2017")
pop_est_2017$FIPS <- NULL
pop_est_2017$Year <- NULL
pop_est_2017$Estimate <- NULL
pop_est_2017 <- pop_est_2017[order(pop_est_2017$County), ]

  #Subset to just 2017
df_census <- subset(df_census, Year == "July 01 2017")

  #Add the Urban/Rural Column
df_census$rural_or_urban <- df_census$Population
option_urban <- which(df_census$rural_or_urban >= 50000)
option_rural <- which(df_census$rural_or_urban <= 49999)
df_census$rural_or_urban[option_urban] <- "Urban"
df_census$rural_or_urban[option_rural] <- "Rural"


##############################################################################
#                           ANALYSIS
##############################################################################


# THE analyze_by FUNCTION ACCEPTS A DATAFRAME FOR THE FIRST ARG, df
# THE analyze_by FUNCTION ACCEPTS A QUOTED COLUMN NAME FOR THE SECOND ARG, col_name
# THE analyze_by FUNCTION RETURNS A DATAFRAME THAT GROUPS BY col_name AND SUMS THE VOLUME OF LIQUOR SOLD
# Example: 
# g <- analyze_by(df, quo(Date))
analyze_by <- function(df, col_name) {
  summ <- df %>%
    select(!! col_name, Volume.Sold..Liters.) %>%
    group_by(!! col_name) %>%
    summarize(VolSold = sum(Volume.Sold..Liters.)) %>%
    arrange(desc(VolSold))
  summ
}

# WHAT CATEGORIES OF ALCOHOL ARE PURCHASED IN THE HIGHEST QUANTITY (BY VOLUME IN LITERS)?
categories <- analyze_by(df_sales, quo(Category.Name))

# WHAT BRANDS (VENDOR) OF ALCOHOL ARE PURCHASED IN THE HIGHEST QUANTITY (BY VOLUME IN LITERS)?
vendors <- analyze_by(df_sales, quo(Vendor.Name))

# WHAT DESCRIPTIONS OF ALCOHOL ARE PURCHASED IN THE HIGHEST QUANTITY (BY VOLUME IN LITERS)?
descriptions <- analyze_by(df_sales, quo(Item.Description))

# WHICH CITIES BUY THE MOST ALCOHOL (BY VOLUME IN LITERS)?
cities <- analyze_by(df_sales, quo(City))

# WHICH COUNTIES BUY THE MOST ALCOHOL (BY VOLUME LITERS/PER CAPITA)?
df_census <- group_by(df_census, County)
county_pop <- summarize(df_census, Sum_County_Pop = sum(Estimate))
df_census <- ungroup(df_census)
county_pop$County <- tolower(county_pop$County)
summ_County$County <- tolower(summ_County$County)
test_merge <- merge(summ_County, county_pop, all.x = TRUE)
test_merge$Vol_Per_Capita <- round(test_merge$Sum_Volume_Sold_Liters / test_merge$Sum_County_Pop, 1)
test_merge <- arrange(test_merge, desc(Vol_Per_Capita))
df_sales <- ungroup(df_sales)

# VOLUME SOLD BY WEEK

df_sales$date_binned <- cut(df_sales$Date, breaks = "weeks")
df_sales <- group_by(df_sales, date_binned)
summ_Date <- summarize(df_sales, Sum_Volume_Sold_Liters = sum(Volume.Sold..Liters.))       
summ_Date <- arrange(summ_Date, desc(Sum_Volume_Sold_Liters))
df_sales <- ungroup(df_sales)

##############################################################################
#                           VISUALIZATIONS
##############################################################################

# LINE GRAPH OF SALES BY HIGHEST VOLUME (LITERS)
plot <- qplot(Date, Volume.Sold..Liters., data = df_sales, geom = "line")
plot <- plot + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")

ggsave(filename = "plot_dates.png", plot = plot, width = 24, height = 4, dpi = 600)

# 

