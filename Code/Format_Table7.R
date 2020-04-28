## Author: Rachel Bates (rlb18@ic.ac.uk)
## Date: 13/02/2020

require(dplyr)

## Script to allow the Table 7 data to be usable
## First I downloaded the data from the IUCN website 
##### https://www.iucnredlist.org/resources/summary-statistics only available as a pdf
## Used a free online tool to convert them to csv

col_names <- c("scientific_name", "common_name", "previous_category", "new_category", "reason_for_change", "year")

## Read in the table 7 docs

### Could do via lapply or similar? ###
changes2007 <- read.csv("../Data/2007Table7.csv", header=FALSE)
changes2008 <- read.csv("../Data/2008Table7.csv", header=FALSE)
changes2009 <- read.csv("../Data/2009Table7.csv", header=FALSE)
changes2010 <- read.csv("../Data/2010Table7.csv", header=FALSE)
changes2011 <- read.csv("../Data/2011Table7.csv", header=FALSE)
changes2012 <- read.csv("../Data/2012Table7.csv", header=FALSE)
changes2013 <- read.csv("../Data/2013Table7.csv", header=FALSE)
changes2014 <- read.csv("../Data/2014Table7.csv", header=FALSE)
changes2015 <- read.csv("../Data/2015Table7.csv", header=FALSE)
changes2016 <- read.csv("../Data/2016Table7.csv", header=FALSE)
changes2017 <- read.csv("../Data/2017Table7.csv", header=FALSE)
changes2018 <- read.csv("../Data/2018Table7.csv", header=FALSE)
changes2019 <- read.csv("../Data/2019Table7.csv", header=FALSE)


## Individual changes to get the formatting in line with each other
changes2008["Reason_For_Change"] <- "G"
changes2008["Year"] <- "2008"

changes2009 <- changes2009[,1:5]
changes2009["Year"] <- "2009"

changes2011 <- changes2011[,2:7]
changes2012 <- changes2012[,2:7]
changes2013 <- changes2013[,2:7]
changes2014 <- changes2014[,2:7]

## Put all of the dfs in a list
all_tables = mget(ls(pattern = "changes[2007:2019]"))

## Make all the colnames the same
for (i in 1:length(all_tables)){
  print(i)
  names(all_tables[[i]]) <- col_names
}

## Bind together into one df
Table7 <- bind_rows(all_tables)
Table7[Table7==""] <- NA
Table7 <- Table7[complete.cases(Table7[,c(1,3:6)]),]

## Check for and remove erroneous rows
table(Table7$previous_category)
Table7 <- dplyr::filter(Table7, previous_category != "(2007)" & previous_category != "(2008)")

write.csv(Table7, "../Data/Table7.csv", row.names = FALSE)

#############################################################
