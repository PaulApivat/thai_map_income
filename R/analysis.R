# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load previously saved data
load(file = "thai_income.RData")

# packages 
library(tidyverse)
library(readxl)

# read data from Excel
# note: issues reading Thai alphabet (had to use Excel as intermediary)
df3 <- read_excel("/Users/paulapivat/Desktop/data_go_th/thai_file_2.xlsx")



