require(dplyr)

Species_History<-read.csv("../Data/Species_History050220.csv")

Assess_Clean <- function(Species_History){
  # Assigns an index for debugging
  #Species_History <- Species_History %>% mutate(row_ID = row_number())
  # Remove all pre 1994 listings as that is when the current system was implemented
  Species_History <- subset(Species_History, Species_History$year >= 1994)
  # Select which codes to remove
  lose_codes <- c("I","NR","K", "R", "CT")
  Species_History <- dplyr::filter(Species_History, !Species_History$category %in% lose_codes)
  # Generate a df of only the years with two assessments
  Duplicates <- Species_History %>% group_by(taxonid) %>% filter(duplicated(year)|duplicated(year, fromLast=TRUE))
  # Remove these assessments as there is no way to know which order they were in
  Species_History <- Species_History %>% filter(!Species_History$row_ID %in% Duplicates$row_ID)
  # Remove species with only one assessment remaining
  Species_History <- Species_History %>% group_by(taxonid) %>% filter(n()>1) %>% ungroup
}

# read in csv
#Table7 <- read.csv("../Data/Table7.csv", header=TRUE)

Fix_Nongen_Assess <- function(Table7){
  
}

