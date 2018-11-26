##############################################################################
##############################################################################
#                          R GROUP PROJECT BULK FILE
##############################################################################
##############################################################################

# CLEAR ENVIRONMENT
rm(list=ls())

# LOAD NECESSARY LIBRARIES
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(choroplethr))
suppressPackageStartupMessages(library(choroplethrMaps))
suppressPackageStartupMessages(library(sqldf))

##############################################################################
#                           DATA CLEANSING
##############################################################################

# LOAD ALL DATA FRAMES
#Liquor sales df
df_sales <- read.csv("Iowa_Liquor_Sales_2017.csv", na.strings = c("NA", ""))

#Census df
df_census <- read.csv("County_Population_in_Iowa_by_Year.csv")

#Holidays df
df_holidays <- read.csv("holidays.csv")
df_holidays$Date <- as.Date(df_holidays$Date, "%m/%d/%Y")

# LOAD & CLEANSE COUNTY.REGIONS DF
# Load detailed county info
data(county.regions)
# Subset to just Iowa
county.regions <- subset(county.regions, state.abb == "IA")
county.regions <- county.regions[, c("region", "county.name")]

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
#Clean up county spellings
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
# Pull in FIPS code
# Rename county.regions df for merge-specific cleansing
fips <- county.regions
# Rename column: county.name to County
names(fips)[names(fips) == "county.name"] <- "County"
names(fips)[names(fips) == "region"] <- "FIPS"
# Merge FIPS column into df_sales
df_sales <- merge(df_sales, fips, all.x = TRUE)


# CLEANSE CENSUS DF
#column not needed; delete
df_census$Primary.County.Coordinates <- NULL 
df_census$FIPS <- factor(df_census$FIPS)
#Note: left Date as a factor on purpose.

#Examine Population Data for July 01, 2017 to ensure the 
# "July 01 2017" Census includes data for all 99 counties.
pop_est_2017 <- subset(df_census, Year == "July 01 2017")
print(nrow(pop_est_2017))

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
#Summarize volume by county
county <- analyze_by(df_sales, quo(FIPS))
#Merge summary with census df
county_pc <- merge(county, df_census, all.x = TRUE)
#Calculate per capita consumption
county_pc$Vol_Per_Cap <- round(county_pc$VolSold / county_pc$Population, 1)
#Remove irrelevant columns
county_pc$VolSold <- NULL
county_pc$Year <- NULL
county_pc$Population <- NULL
county_pc$rural_or_urban <- NULL
#Order in descending order (highest to lowest consumption)
county_pc <- arrange(county_pc, desc(Vol_Per_Cap))


# TOP VENDOR FOR EACH COUNTY
vendor_by_county <- df_sales %>% # select the columns we want
  select(County, Vendor.Name, Volume.Sold..Liters.)
vendor_by_county$County <- tolower(vendor_by_county$County) # make case-insensitive
vendor_by_county <- vendor_by_county %>%
  group_by(County, Vendor.Name) %>% # group by county and vendor name
  summarize(VolSold = sum(Volume.Sold..Liters.)) # sum based on county & vendor name combo

colnames(vendor_by_county)[colnames(vendor_by_county)=="Vendor.Name"] <- "VendorName" #Rename column to work in SQL command

vendor_by_county <- sqldf("select County, VendorName, max(VolSold) from vendor_by_county where County != '' group by County") # Find the biggest supplier for each county

# TOP LIQUOR FOR EACH COUNTY
liquor_by_county <- df_sales %>% # select the columns we want
  select(County, Item.Description, Volume.Sold..Liters.)
liquor_by_county$County <- tolower(liquor_by_county$County) # make case-insensitive
liquor_by_county <- liquor_by_county %>%
  group_by(County, Item.Description) %>% # group by county and vendor name
  summarize(VolSold = sum(Volume.Sold..Liters.)) # sum based on county & vendor name combo

colnames(liquor_by_county)[colnames(liquor_by_county)=="Item.Description"] <- "Liquor" #Rename column to work in SQL command

liquor_by_county <- sqldf("select County, Liquor, max(VolSold) from liquor_by_county where County != '' group by County") # Find the biggest supplier for each county

# TOP STORES IN IOWA
stores <- analyze_by(df_sales, quo(Store.Name))

# HYVEE ONLY
df_hyvee <- df_sales[grep("Hy-Vee", df_sales$Store.Name), ]

#volume of hyvee compared to whole state
total_hyvee_volume_sold <- sum(df_hyvee$Volume.Sold..Liters.)
total_volume_sold <- sum(df_sales$Volume.Sold..Liters.)
percent_hyvee_volume <- (total_hyvee_volume_sold/total_volume_sold) * 100

# VOLUME SOLD BY WEEK
df_sales$date_binned <- cut(df_sales$Date, breaks = "weeks")
dates <- analyze_by(df_sales, quo(date_binned))


##############################################################################
#                           VISUALIZATIONS
##############################################################################
# BAR CHART FOR TOP 5 CATEGORIES BY VOLUME
pCat <- qplot(x = reorder(Category.Name, VolSold), y = VolSold, data = head(categories, 5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Categories by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("Category")+
  scale_y_continuous(labels = comma)
pCat
ggsave(filename = "Top5CatBarChart.png", plot = pCat, width = 8, height = 4,
       dpi = 600)


# BAR CHART FOR TOP 5 VENDORS BY VOLUME
pVen <- qplot(x = reorder(Vendor.Name, VolSold), y = VolSold, data = head(vendors, 5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Vendors by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("Vendor")+
  scale_y_continuous(labels = comma)
pVen
ggsave(filename = "Top5VenBarChart.png", plot = pVen, width = 8, height = 4,
       dpi = 600)

# BAR CHART FOR TOP 5 DESCRIPTIONS BY VOLUME
pDes <- qplot(x = reorder(Item.Description, VolSold), y = VolSold, data = head(descriptions, 5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Descriptions by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("Description")+
  scale_y_continuous(labels = comma)
pDes
ggsave(filename = "Top5DesBarChart.png", plot = pDes, width = 8, height = 4,
       dpi = 600)

# BAR CHART FOR TOP 5 CITIES BY VOLUME
pCit <- qplot(x = reorder(City, VolSold), y = VolSold, data = head(cities, 5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Cities by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("City")+
  scale_y_continuous(labels = comma)
pCit
ggsave(filename = "Top5CitBarChart.png", plot = pCit, width = 8, height = 4,
       dpi = 600)





# LINE GRAPH OF SALES BY HIGHEST VOLUME (LITERS)
plot <- qplot(Date, Volume.Sold..Liters., data = df_sales, geom = "line")
plot <- plot + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")

ggsave(filename = "plot_dates.png", plot = plot, width = 24, height = 4, dpi = 600)

# CHOROPLETH OF VOLUME PER CAPITA
plot_data <- county_pc

names(plot_data)[names(plot_data) == "County"] <- "county.name"
names(plot_data)[names(plot_data) == "FIPS"] <- "region"
names(plot_data)[names(plot_data) == "Vol_Per_Cap"] <- "value"

p <- county_choropleth(plot_data, state_zoom = "iowa", title = "Volume Per Capita", legend = "Liters Per Person")
print(p)
ggsave(filename = "volume_per_capita_map.png", plot = p, height = 4, dpi = 600)

# BAR CHART OF VOLUME PER CAPITA
Above7 <- subset(plot_data, value >= 7)

pPlo <- qplot(x = reorder(county.name, value), y = value, data = Above7) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("All Counties with value > 7")+
  ylab("Liters per Person")+
  xlab("County")+
  scale_y_continuous(labels = comma)
pPlo
ggsave(filename = "CountiesAbove7.png", plot = pPlo, width = 8, height = 4,
       dpi = 600)

# BAR CHART OF VOLUME BY STORE
pSto <- qplot(x = reorder(Store.Name, VolSold), y = VolSold, data = head(stores,5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Stores by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("Store Name")+
  scale_y_continuous(labels = comma)
pSto
ggsave(filename = "Top5StoreBarChart.png", plot = pSto, width = 8, height = 4,
       dpi = 600)


# BAR CHART OF VOLUME BY WEEK
pDat <- qplot(x = reorder(date_binned, VolSold), y = VolSold, data = head(dates,5)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() +
  ggtitle("Top 5 Weeks During 2017 by Volume of Liquor Sold")+
  ylab("Volume Sold (Liters)")+
  xlab("Week of")+
  scale_y_continuous(labels = comma)
pDat
ggsave(filename = "Top5WeeksBarChart.png", plot = pDat, width = 8, height = 4,
       dpi = 600)


# SUBSET TO WEEKS AROUND HAWKEYE FOOTBALL GAMES AND ONLY HAWKEYE VODKA SALES
df_hv <- subset(df_sales, Date >= "2017-08-15" & Date < "2018-01-01")
df_hv <- subset(df_hv, Item.Number == "36308" | Item.Number == "36307" | 
                  Item.Number == "36306" | Item.Number == "36305" | 
                  Item.Number == "36301") 

# LINE GRAPH OF SALES OF VODKA AROUND HAWKEYE FOOTBALL GAMES
plot2 <- qplot(Date, Volume.Sold..Liters., data = df_hv, geom = "line")
plot2 <- plot2 + scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")
ggsave(filename = "hv_dates.png", plot = plot2, width = 11, height = 5, dpi = 600)
