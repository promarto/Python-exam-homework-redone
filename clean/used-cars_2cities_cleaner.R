####################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com
#
# License: Free to share, modify and use for educational purposes.
# 	Not to be used for commercial purposes.
#
####################################################################

####################################################################
# used-car-la dataset
#
# input:
#       used_cars_2cities.csv

# output:
#       used-cars_2cities_prep.csv

# version 1.0   2021-06-01
####################################################################
rm(list=ls())


library(tidyverse)
library(dplyr)
library(haven)


# set working directory for da_data_repo 
setwd("/Users/vigadam/Dropbox/work/data_book/da_data_repo")

data_in = "used-cars/raw/"
data_out = "used-cars/clean/"

#load dataset
df <- read_csv(paste(data_in,"used_cars_2cities.csv",sep=""))

#check for duplicates
df <- df %>% dplyr::select(-v1)
df <- df %>% distinct()

#gen intiger and logarithmic price variables

df <- df %>% rename(pricestr = price) %>% drop_na(pricestr)

df <- df %>% mutate(price = as.numeric(str_replace_all(pattern = "\t|\n|\\$", replacement = "",pricestr)),
                    lnprice = log(price))

#gen year and age variables from name string
df <- df %>% mutate(year = as.numeric(lapply(str_split(name, " ") ,head,n=1L) %>% unlist()),
                    age = 2017 - year + 1)

#filter by odometer

df <- df %>% mutate(odometer = odometer / 10000)


df <- df[replace_na(!(df$odometer < 1 & df$age >= 3), T),]


# fill missing gaps by mean of age groups
df <- df %>% 
  group_by(age) %>% 
  mutate(odometer = mean(odometer, na.rm = T))


df <- df %>% mutate(lnodometer = log(odometer))

#general stats of major variables

df %>% select(price, lnprice, age, odometer) %>% describe()


#generate feature dummy variables

df <- df %>% mutate(LE = as.numeric(grepl(" le", tolower(name))),
                    XLE = as.numeric(grepl(" xle", tolower(name))),
                    SE = as.numeric(grepl(" se", tolower(name))),
                    Hybrid = as.numeric(grepl(" hybrid", tolower(name))))

#general stats of dummy variables

df %>% select(LE, XLE, SE, Hybrid) %>% describe()


write_csv(df, paste(data_out,"used-cars_2cities_prep.csv", sep=""))







