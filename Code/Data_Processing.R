require(dplyr)

Species_History <- read.csv("../Data/Species_History050220.csv")
Species_Data <- read.csv("../Data/Species_Data.csv")
#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

Assess_Clean <- function(Species_History){
  # Assigns an index for debugging
  Species_History <- Species_History %>% mutate(row_ID = row_number())
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
  # Add species name to table
  Species_History <- inner_join(Species_History, Species_Data[,c(1,8)], by = "taxonid")
  Species_History$category<-droplevels(Species_History$category)
  return(Species_History)
}

# read in csv
Table7 <- read.csv("../Data/Table7.csv", header=TRUE)

Species_History <- Assess_Clean(Species_History)

Add_table7_tags <- function(Table7, Species_History){
  Table7$year_sub <- Table7$year
  Table7$year <- floor(as.integer(readr::parse_number(as.character(Table7$year))))
  Species_History$scientific_name <- as.character(Species_History$scientific_name)
  Table7$scientific_name <- as.character(Table7$scientific_name)
  # Make a df of all assessment changes contained in Table 7
  Cat_Changes <- inner_join(Species_History, Table7[,c(1,3:6)], by = c("scientific_name", "year"))
  # Cooerce to character
  Cat_Changes$category <- as.character(Cat_Changes$category)
  Cat_Changes$new_category <- as.character(Cat_Changes$new_category)
  Cat_Changes[Cat_Changes=="CR(PE)"|Cat_Changes=="CR(PEW)"|Cat_Changes=="CR (PE)"] <- "CR"
  # Make a df of non-matching assessments as a check
  check <- Cat_Changes %>% filter(category != Cat_Changes$new_category)
  test2 <- Cat_Changes %>% filter(row_ID %in% check$row_ID)# 10 that don't line up for no obvious reason
  # Remove those, assume clerical error
  Cat_Changes <- Cat_Changes %>% filter(!row_ID %in% check$row_ID)
  return(Cat_Changes)
}

Cat_Changes <- Add_table7_tags(Table7, Species_History)

Same_cat_tag <- function(group_df){
  for (i in 2:nrow(group_df)){
    if (group_df$category[i] == group_df$category[i-1]){
      group_df$Verified[i] <- "True"
    } else {
      next
    }
  }
  return(group_df)
}


Assign_tags <- function(Cat_Changes, Species_History){
  # Use Cat_changes to assign tags where we can, need to accces both assessments where a change has happened
  Species_History$Verified <- NA
  # Assign all most recent changes as unknown
  Species_History <- Species_History %>% group_by(taxonid) %>% mutate(Verified=(ifelse(year==max(year),"Unknown",NA))) %>% ungroup
  # Create a reference df
  Reference <- Cat_Changes[,c("row_ID", "reason_for_change")]
  Reference$reason_for_change <- recode(Reference$reason_for_change, N = "False", G = "True")
  Reference <- Reference %>% mutate(row_ID = row_ID + 1) # Correct to be previous assessment
  # Add the True/False tags to the main df
  Species_History <- merge(Species_History, Reference, by = "row_ID", all.x = TRUE)
  x<-which(!is.na(Species_History$reason_for_change)) # select all rows with a true/false tag
  # Copy the tags to the main column
  Species_History$Verified[x] <- as.character(Species_History$reason_for_change[x])
  # Remove the column used to transfer the tags
  Species_History$reason_for_change <- NULL
  # Assign True tags where the assessment has been the same twice in a row
  Same_tags <- Species_History[NULL,]
  for (i in unique(Species_History$taxonid)){
    individual_history <- Species_History[Species_History$taxonid==i,]
    Same_tags <- rbind(Same_tags ,Same_cat_tag(individual_history))
  }
  Species_History <- Same_tags
  return(Species_History)
}

Spceies_History <- Assign_tags(Cat_Changes, Species_History)





