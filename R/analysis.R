# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load previously saved data
load(file = "thai_income.RData")

# packages 
library(tidyverse)
library(readxl)

# read data from Excel
# note: issues reading Thai alphabet (had to use Excel as intermediary)
df4 <- read_excel("/Users/paulapivat/Desktop/data_go_th/thai_file_2.xlsx")

####### PRE TIDY Data #######

# rename multiple column names at once
colnames(df4)[3:13] <- c("1998", "2000", "2002", "2004", "2006", "2007", "2009", "2011", "2013", "2015", "Region_Province")

# remove first five rows of data frame
df4 <- df4[-(1:5),]

# remove last four rows of data frame
df4 <- df4[-(84:87),]

# move last column to second (just going to copy it in, easiest way)
df4$...2 <- df4$Region_Province

# copy to df5, everything but first column (Regoin_province in Thai, duplicate)
# first column contains Thai alphabet, went with English spelling
df5 <- df4[,-(1)]

# create new column Region
# selecting consecutive rows within a column to add a character label
df5[3:6,12] <- "Greater Bangkok"
df5[8:29,12] <- "Central Region"
df5[31:47,12] <- "Northern Region"
df5[49:68,12] <- "Northeastern"
df5[70:83,12] <- "Southern"

df5[1:1,12] <- "Country Level" 
df5[2:2,12] <- "Greater Bangkok Category"
df5[7:7,12] <- "Central Region Category"
df5[30:30,12] <- "Northern Region Category"
df5[48:48,12] <- "Northeastern Category"
df5[69:69,12] <- "Southern Category"

##### Tidy Data

# gather, key, value
df6 <- df5 %>% gather(`1998`, `2000`, `2002`, `2004`, `2006`, `2007`, `2009`, `2011`, `2013`, `2015`, key = "year", value = "Avg_Monthly_Household_Income")

### Basic Plots
# Bar

ggplot(data = df6, mapping = aes(x = Region, y = Avg_Monthly_Household_Income)) 
+ geom_bar(stat = "identity")

# NOTE: distinguishing “Greater Bangkok Category” from “Greater Bangkok” is NOT helpful 
df5a <- df5
df5a[2:2,12] <- "Greater Bangkok"
df5a[7:7,12] <- "Central Region"
df5a[30:30,12] <- "Northern Region"
df5a[48:48,12] <- "Northeastern"
df5a[69:69,12] <- "Southern"

# delete 'whole kingdom'
df5a <- df5a[-1,]
df6a <- df5a %>% gather(`1998`, `2000`, `2002`, `2004`, `2006`, `2007`, `2009`, `2011`, `2013`, `2015`, key = "year", value = "Avg_Income")

bar1 <- ggplot(data = df6a, mapping = aes(x = Region, y = Avg_Income)) 
+ geom_bar(stat = "identity") 
+ labs(x = "Region", y = "Average Household Monthly Income", title = "Average Household Monthly Income, 1998 - 2015")

# turn off scienfic notation
options(scipen = 999)

# group_by province, then do another bar
# re-order works after group_by
province <- df6a %>% group_by(Region_Province) %>% summarize(sum_avg_income = sum(Avg_Income), mean = mean(Avg_Income), sd = sd(Avg_Income))

# Re-order only works after applying group_by, then summarize and saved in a new data frame
province_bar_reorder <- ggplot(data = province, mapping = aes(x = reorder(Region_Province, sum_avg_income), y = sum_avg_income)) 
+ geom_bar(stat = "identity") 
+ labs(x = "Province", y = "Average Household Monthly Income", title = "Sum Average Household Monthly Income by Province, 1998 - 2015")
# adjust x-axis tick to 90 degree
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot Region (only)
# re-order by sum_avg_income (greater bangkok NOT the largest)
ggplot(data = region, mapping = aes(x = reorder(Region, sum_avg_income), y = sum_avg_income)) 
+ geom_bar(stat = "identity") 
+ labs(x = "Region", y = "Sum Average Household Monthly Income", title = "Sum Average Household Monthly Income by Region, 1998 - 2015")

# re-order by mean (greater bangkok IS the largest)
ggplot(data = region, mapping = aes(x = reorder(Region, mean), y = mean)) 
+ geom_bar(stat = "identity") 
+ labs(x = "Region", y = "Mean Household Monthly Income", title = "Mean Household Monthly Income by Region, 1998 - 2015")

###### ------- MAP ---------#######

## Option 1 (Playing around with map - this option does not ultimately work)
## NOTE: maps package doesn’t include Thailand’s sub-region

### Complication ####
# Note: Trying to visualize Thailand map in GGPLOT, using geom_polygon requires, long, lat, group,
# If i wanted to visualize the average income of each region, I would need “subregion” data

library(devtools)
library(maps)
library(mapdata)
library(ggmap)
library(broom)  	# get tidy() function

# get data frame from map_data() from “broom” package
map_data_thai <- map_data("worldHires", region = c("thailand"))
# completely filled in map (solid black, not outline)
map_thai2 <- ggplot() + geom_polygon(data = map_data_thai, aes(x=long, y=lat, group=group)) + coord_fixed(1.0)
fun_map <- ggplot() + geom_polygon(data = map_data_thai, aes(x=long, y =lat, group=group), fill = "light blue", color = "green") + coord_fixed(1.0)

fun_map + geom_point(data = dots, aes(x=long, y=lat), color = "black", size = 5) 
        + geom_point(data = dots, aes(x=long, y=lat), color = "yellow", size = 4)

#### Option 2: Getting Maps to Work





