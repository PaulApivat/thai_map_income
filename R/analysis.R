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


