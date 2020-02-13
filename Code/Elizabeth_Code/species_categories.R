# This file is used to convert json files downloaded via the RedList API into csv files

# clear workspace
rm(list=ls())

#import dplyr
require(dplyr)
# files downloaded via RedList API on 6 February 2018 (DD data was downloaded via API on 4 March 2018)
library(jsonlite)

# convert JSON files to csv files for each category

# First define categories so that I can use a loop to convert the JSON files into csv files:

categories <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "LRcd", "LRnt", "LRlc", "DD")

#Each individual group:

list_JSON <- function(categories){
  
  JSON_files <- c()
  
  for (i in 1:length(categories)){
    
    new_file <-paste("../Data/", categories[i], ".json", sep="")
    
    JSON_files <-c(JSON_files, new_file )
  }
  return(JSON_files)
}

JSON_file_list <- list_JSON(categories)

# Now convert the JSON files to csv files using the list of file names collated above
JSON_csv <- function(JSON_file_list){
  
  for (i in 1:length(JSON_file_list)){
    
    write.csv(fromJSON(JSON_file_list[i]), file = paste("../Data/", categories[i], ".csv", sep = ""), row.names = FALSE)}
}

# use the above function to turn all the JSON files into csv files
JSON_csv(JSON_file_list)

