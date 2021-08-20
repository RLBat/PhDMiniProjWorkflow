require(dplyr)
require(tidyverse)

Bird_RLI <- read.csv("../Data/Bird_RLI_original.csv", stringsAsFactors = T)
Bird_RLI <- Bird_RLI[,2:10]

names(Bird_RLI) <- c("binomial", "taxonid", "1988", "1994", "2000","2004","2008","2012","2016")
Bird_RLI <- pivot_longer(Bird_RLI, cols = c("1988", "1994", "2000","2004","2008","2012","2016")
, names_to = "year", values_to = "category")

cols = c("X1988", "X1994", "X2000", "X2004", "X2008", "X2012", "X2016")

Bird_RLI$year <- as.numeric(Bird_RLI$year)

