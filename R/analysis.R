# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# main source(s):
# Tutorial 1. https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# Tutorial 2. (see this for labels) http://bl.ocks.org/prabhasp/raw/5030005/
# Maps Data 3: https://gadm.org/download_country_v3.html
# Income Data 4: https://data.go.th/DatasetDetail.aspx?id=7049410f-5bb8-4c75-9e94-112ca18b63e2

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

#### ----- Option 2: Getting Maps to Work ----- ######

## could NOT get this data from the maps / mapdata package; 
## rather, had to go to GADM.org and download Thailand R (sf) level1 (and level2); 
## https://gadm.org/download_country_v3.html
## then use the broom package function tidy() to temporarily convert it to a dataframe in View() 
## to see that it had the following columns (long, lat, order, hole, piece, group, id); 

## tutorial: source: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# invalid graphics state error
dev.off()

# Confirm: the “id” in tidy(thai1) is “province” (n = 77)
# this is important to map

# Views to figure this out:
# - thai1 (not helpful)
# - tidy(thai1) 	function from broom package, identify “id”
# - thai1_dif		convert to data frame (see GID_1, NAME_1)
# - df5a		Province = ID (n = 77)

# Next steps:
# - add “id” column to thai1_df
# - fill that “id” column 1-to-77
# - also add “id” column to df5a, (erase: Greater Bangkok, Central Region, Northern Region, Northeastern Region, and Southern Region)
# - use that column to join with “id” column in tidy(thai1)

### data frames to use
# - thai1
# - tidy(thai1)   #broom package
# - View(thai1_tidy %>% group_by(id) %>% tally(sort = TRUE))    #confirm 77 province
# - thai1_df      #match id to province (column 0 is the id)
# - df5a 	
# - df6a

# step 0: 
df5b <- df5a

# step 1: delete “Greater Bangkok”, “Central Region”, “Northern Region”, “Northeastern Region”, “Southern Region”
df5b <- df5b[-1,]		# formerly “Greater Bangkok”
df5b <- df5b[-5,]		# formerly “Central Region”
df5b <- df5b[-27,]		# formerly “Northern Region”
df5b <- df5b[-44,]		# formerly “Northeastern Region”
df5b <- df5b[-64,]		# formerly “Southern Region”

# step 2: create new column (first column) next to “Region_Province” in df5b to put 'id'
# NOTE: major assumption is that column 0 in thai1_df is in fact the “id” in thai1_tidy

# step2a: manually enter id 1-77 for df5b

library(tibble) # for add_column() function

df5b <- add_column(df5b, id = c("23", "53", "30", "31", "41", "12", "15", "60", "67", "57", 
"4", "49", "77", "69", "56", "44", "19", "51", "48", "8", "63", "20", "54", "55", "38", "46", 
"2", "13", "11", "73", "42", "26", "36", "3", "16", "24", "72", "7", "66", "62", "40", "39", 
"37", "22", "45", "65", "59", "70", "75", "76", "1", "34", "28", "9", "71", "14", "29", "17", 
"50", "6", "52", "21", "18", "25", "10", "33", "43", "64", "47", "5", "61", "58", "68", "35", 
"32", "74", "27"), .before = "Region_Province")

# step 3: thai1_tidy <- tidy(thai) # already created
# note: “id” is a character, not a numeric

# step 4: join df5b and thai1_tidy by id
thai1_tidy_join <- thai1_tidy %>%
    inner_join(df5b, by = "id")

# step 5: plot fill = 1998 numbers (lame! cannot see the difference, must transform numbers)
ggplot(data = thai1_tidy_join) + geom_polygon(aes(x = long, y = lat, fill = 1998, group = group), color = "white") + coord_fixed(1.0) + guides(fill = FALSE)
# nice looking all Region_Province has different color (note: took out guides(fill=FALSE) so menu displayed all 77 provinces
ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = Region_Province, group = group), color = "white") 
+ coord_fixed(1.0)

# Goal: display Avg Income PER YEAR basis only (not aggregate)
avg_income_1998 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`1998`, group = group), color = "white") 
+ coord_fixed(1.0) 
+ scale_fill_continuous(type = "viridis")

avg_income_2015 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`2015`, group = group), color = "white") 
+ coord_fixed(1.0) 
+ scale_fill_continuous(type = "viridis") 
+ labs(fill = "Average Monthly Income, 2015")

### 2007 income: Divergent 
#### scale_fill_gradient2, especially for divergent continuous fill

avg_income_2007 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`2007`, group = group), color = "white") 
+ coord_fixed(1.0) 
+ scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 20000, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") 
+ labs(fill = "Average Monthly Income, 2007")

### Find median point to use as “white” mid-point of color divergent
summary(df5b)   	# use median as midpoint (mid = “white”)

## district names on the map
## make a dataset that creates centroids per district, map a layer of text 
# source: http://bl.ocks.org/prabhasp/raw/5030005/
# NOTE: since “deploy” is not available for R version 3.6.2, you can achieve the same thing using pipe functions in tidyverse

# text_centroid
thai1_tidy_join %>%
    group_by(Region_Province) %>%
    summarize(clat = mean(lat), clong = mean(long)) -> text_centroid


###--------- Final Plots (geom_text, size outside of aes())----------###

# average monthly household income (amhi) 1998
amhi_1998 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`1998`, group = group), color = "black") 
+ coord_fixed(1.0) 
+ scale_fill_gradient2(low = "#ca0020", mid = "white", high = "#0571b0", midpoint = 10000, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") 
+ labs(fill = "Average Monthly Household Income, 1998") 
+ theme_classic() 
+ geom_text(data = text_centroid, aes(x = clong, y = clat, label = Region_Province), size = 2)

# average monthly household income (amhi) 2007
amhi_2007 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`2007`, group = group), color = "black") 
+ coord_fixed(1.0) 
+ scale_fill_gradient2(low = "#ca0020", mid = "white", high = "#0571b0", midpoint = 15000, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") 
+ labs(fill = "Average Monthly Household Income, 2007") 
+ theme_classic() 
+ geom_text(data = text_centroid, aes(x = clong, y = clat, label = Region_Province), size = 2)

# average monthly household income (amhi) 2015
amhi_2015 <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$`2015`, group = group), color = "black") 
+ coord_fixed(1.0) 
+ scale_fill_gradient2(low = "#ca0020", mid = "white", high = "#0571b0", midpoint = 22000, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") 
+ labs(fill = "Average Monthly Household Income, 2015") 
+ theme_classic() 
+ geom_text(data = text_centroid, aes(x = clong, y = clat, label = Region_Province), size = 2)

##### Income Change over the years

# calculate change in income from 1998 - 2015
thai1_tidy_join <- thai1_tidy_join %>% 
mutate(income_change = thai1_tidy_join$`2015` - thai1_tidy_join$`1998`)

# ploting change in household income
amhi_change <- ggplot(data = thai1_tidy_join) 
+ geom_polygon(aes(x = long, y = lat, fill = thai1_tidy_join$income_change, group = group), color = "white") 
+ coord_fixed(1.0) 
+ scale_fill_continuous(type = "viridis") 
+ labs(fill = "Change in Avg Monthly Income, 1998 - 2015") 
+ theme_classic() 
+ geom_text(data = text_centroid, aes(x = clong, y = clat, label = Region_Province), size = 2)

