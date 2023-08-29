require(dplyr)
require(tidyverse)

`%!in%` = Negate(`%in%`)

#### Read in your files here #####

Species_History <- read.csv("../Data/Species_History_IDs_20223.csv", stringsAsFactors = T)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)

###############################

Correct_False_Extinctions <- function(Species_History_Tags){
  # Any time where a species has a true extant category post extinction, 
  # that extinction should be labelled as false
  for (i in unique(Species_History_Tags$taxonid)){
    species <- Species_History_Tags[Species_History_Tags$taxonid == i,]
    if ("EX" %in% species$category) { #If there are any EX assessments
      EX_assess <- which(species$category == "EX")
      non_EX_assess <- which(species$category != "EX")
      if (length(non_EX_assess)>0){  
        # Determines EX assessments happening before extant ones
        False_assess <- species[EX_assess[min(non_EX_assess) < EX_assess],]
        if (nrow(False_assess)>0){
          # Assign tags if there are false de-extinctions
          Species_History_Tags <- Species_History_Tags[-which(Species_History_Tags$row_ID %in% False_assess$row_ID),]
        }
      }
    }
  }
  return(Species_History_Tags)
}

Assess_clean_basic <- function(Species_History){
  # Assigns an index for debugging
  Species_History <- Species_History %>% mutate(row_ID = row_number())
  # Remove all subspecies
  Species_Data <- Species_Data[which(is.na(Species_Data$infra_rank)),]
  Species_History <- Species_History[which(Species_History$taxonid %in% Species_Data$taxonid),]
  # Remove all pre 1994 listings as that is when the current system was implemented
  Species_History <- subset(Species_History, Species_History$year >= 1994)
  # Select which codes to remove    
  lose_codes <- c("I","NR","K", "R", "CT", "E", "V", "Ex/E")
  # Remove those codes
  Species_History <- dplyr::filter(Species_History, !Species_History$category %in% lose_codes)
  # Now rename codes where they have several names
  Species_History$category <- recode(Species_History$category, "Ex" = "EX", "Ex?" = "EX", "EW" = "EX", "LR/lc" = "LC", "LR/nt" = "NT", "LR/cd" = "NT", "nt" = "NT")
  # Generate a df of only the years with two assessments
  Duplicates <- Species_History %>% group_by(taxonid) %>% filter(duplicated(year)|duplicated(year, fromLast=TRUE))
  # Remove these assessments as there is no way to know which order they were in
  Species_History <- Species_History %>% filter(!Species_History$row_ID %in% Duplicates$row_ID)
  # Remove DD species 
  Species_History <- Species_History %>% filter(!Species_History$category == "DD")
  # Check for and remove false extinctions
  Species_History <- Correct_False_Extinctions(Species_History)
  # Remove species with only one assessment remaining
  Species_History <- Species_History %>% group_by(taxonid) %>% filter(n()>1) %>% ungroup
  # Add species name to table
  Species_History <- inner_join(Species_History, Species_Data[,c(1,8)], by = "taxonid")
  Species_History$category<-droplevels(Species_History$category)
  return(Species_History)
}

Species_History <- Assess_clean_basic(Species_History)

