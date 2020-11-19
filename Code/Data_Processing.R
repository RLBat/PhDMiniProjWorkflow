require(dplyr)
require(tidyverse)

`%!in%` = Negate(`%in%`)

Species_History <- read.csv("../Data/Species_History050220.csv")
Species_Data <- read.csv("../Data/Species_Data.csv", stringsAsFactors = F)
#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

Assess_Clean <- function(Species_History){
  # Assigns an index for debugging
  Species_History <- Species_History %>% mutate(row_ID = row_number())
  # Remove all pre 1994 listings as that is when the current system was implemented
  Species_History <- subset(Species_History, Species_History$year >= 1994)
  # Select which codes to remove    
  lose_codes <- c("I","NR","K", "R", "CT", "E", "V", "nt", "Ex/E")
  # Remove those codes
  Species_History <- dplyr::filter(Species_History, !Species_History$category %in% lose_codes)
  # Now rename codes where they have several names
  Species_History$category <- recode(Species_History$category, "Ex" = "EX", "Ex?" = "EX", "EW" = "EX", "LR/lc" = "LC", "LR/nt" = "NT", "LR/cd" = "NT")
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
Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

Species_History <- Assess_Clean(Species_History)

Add_table7_tags <- function(Table7, Species_History){
  Table7$year_sub <- Table7$year
  Table7$year <- floor(as.integer(readr::parse_number(as.character(Table7$year))))
  Species_History$scientific_name <- as.character(Species_History$scientific_name)
  Table7$scientific_name <- as.character(Table7$scientific_name)
  # Make a df of all assessment changes contained in Table 7
  ## This removes all species that have changed name
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
  # Remove any non True/False reasons (E seems to stick around for some)
  Cat_Changes <- Cat_Changes[Cat_Changes$reason_for_change=="N"|Cat_Changes$reason_for_change=="G",]
  return(Cat_Changes)
}

Cat_Changes <- Add_table7_tags(Table7, Species_History)

Same_cat_tag <- function(group_df){
  # Function to identify where two assignments in a row are the same and mark the older one as true
  for (i in 2:nrow(group_df)){
    if (group_df$category[i] == group_df$category[i-1]){
      group_df$Verified[i] <- "True"
    } else {
      next
    }
  }
  return(group_df)
}

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
          Species_History_Tags[which(Species_History_Tags$row_ID %in% False_assess$row_ID),]$Verified <- "False"
        }
      }
    }
  }
  return(Species_History_Tags)
}


Assign_known_tags <- function(Cat_Changes, Species_History){
  # Use Cat_changes to assign tags where we can, need to accces both assessments where a change has happened
  Species_History$Verified <- NA
  # Do any changes that need to be done groupwise
  groupwise_df <- Species_History[NULL,]
  for (i in unique(Species_History$taxonid)){
    # subset to species
    species <- Species_History[(Species_History$taxonid == i),]
    # tag the newest entry as unknown
    species[(species$year==max(species$year)),]$Verified <- "Unknown"
    # Assign True tags where the assessment has been the same twice in a row
    species <- Same_cat_tag(species)
    # work out which years had DD classifications
    DD_years <- which(species$category=="DD")
    if (length(DD_years)>=1){
      # Assign from the DDyears backwards as false
      species[min(DD_years):nrow(species),]$Verified <- "False"
    }
    # Save to df
    groupwise_df <- rbind(groupwise_df, species)
  }
  Species_History <- groupwise_df
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
  #### Tag all false extinctions (where it's later extant) as False
  Species_History <- Correct_False_Extinctions(Species_History)
  # Assign False tags to all DD assessments
  Species_History$Verified[which(Species_History$category=="DD")] <- "False" 
  # Assign unknown values to all other assessments
  Species_History$Verified[which(is.na(Species_History$Verified))] <- "Unknown"
  return(Species_History)
}

Species_History <- Assign_known_tags(Cat_Changes, Species_History)

# checkpoint
# write.csv(Species_History, "../Data/SpeciesHistory_Tags.csv", row.names = FALSE)
#Species_History <- read.csv("../Data/SpeciesHistory_Tags_deextinct.csv", header = T, stringsAsFactors = F)
 write.csv(Species_History, "../Data/SpeciesHistory_Tags_deextinct.csv", row.names = FALSE)


Define_probabilities <- function(Species_History){
  # Probability of a change being true by cat # Very different! LC and EX are much more likely to be true
  Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")
  Cat_probs <- c(rep(NA, length(Categories)))
  names(Cat_probs) <- Categories
  # Works out the prop(true) for each cat and assigns to named list
  for (i in Categories){
    Cat_subset <- Species_History[which(Species_History$category==i & Species_History$Verified!= "Unknown"),]
    Cat_probs[[i]] <- length(which(Cat_subset$Verified=="True"))/nrow(Cat_subset)
  }
  return(Cat_probs)
}

Cat_probs <- Define_probabilities(Species_History)
# Last bit of deterministic stuff


#################################
####### Repeat this!!! #########
###############################

#### RANDOM BIT #####
Generate_tags <- function(Species_History, Cat_probs){
  # Randomly assigns T/F to unknown assessments based on category probabilities
  Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")  
  for (i in Categories){
    # Finds the index values of all unknown assesments fora category
    Cat_unknown <- which(Species_History$category==i & Species_History$Verified=="Unknown")
    # Samples T/F based on the probability for that category
    Tags <- sample(c("True","False"), size=length(Cat_unknown), replace=TRUE, prob=c(Cat_probs[[i]], 1-Cat_probs[[i]]))
    # Places the T/F values where the unknown ones are
    Species_History$Verified[Cat_unknown] <- Tags
  }
  return(Species_History)
}

# Will want to make this so it runs x times
# e.g. Species_History_1 <- replicate(3, Generate_tags(Species_History, Cat_probs))

# Use different name to preserve the original df pre-random assignment
Species_History_Tags <- Generate_tags(Species_History, Cat_probs)


################
### LONG! Needs work

Correction_func <- function(Species){
  if (Species["Verified"]=="False" && Species["Prev_Veri"]=="True" && Species["Prev_year"] <= 5){
    Species["Category"] <- lag(Species["Category"])
    Species["Verified"] <- "Corrected"
  } else if (Species["Verified"]=="False" && Species["Prev_Veri"]=="Corrected" && Species["Prev-year"] <= 5){
    Species["Category"] <- lag(Species["Category"])
    Species["Verified"] <- "Corrected" 
  }
  return("Hello")
}


Reassign_Cats <- function(Species_History_Tags){
  # Works out where/how to change any assessments labelled as false
  Corrected_cats <- Species_History_Tags[NULL,]
  # Splits by taxa
  for (i in unique(Species_History_Tags$taxonid)){
    species <- Species_History_Tags[Species_History_Tags$taxonid==i,]
    # Gets the index values of false assessments
    False_assess <- which(species$Verified=="False")
    if (length(False_assess)>0){
      for (j in False_assess){ # for each false assessment
        if (j > 1){
          # If the following assessment was true
          if (species$Verified[j-1]=="True"){
            # If the following assessment happened within 5 years
            if (species$year[j-1]-species$year[j]<=10){
              # Give assessment the same catetgory as the following.
              species$category[j] <- species$category[j-1]
              species$Verified[j] <- "Corrected"
            }
            #### Might remove this bit. Unsure.
          } else if (species$Verified[j-1]=="Corrected"){
            # Find the subsequent true assessment to compare dates
            True_assess <- which(species$Verified=="True")
            True_assess <- subset(True_assess, True_assess<j)
            True_assess <- max(True_assess)
            # If the next true assessment happened within 5 years, copy category
            if (species$year[True_assess]-species$year[j]<=5){
              species$category[j] <- species$category[True_assess]
              species$Verified[j] <- "Corrected"
            }
          }
        }
      }
      Corrected_cats <- rbind(Corrected_cats, species)
    } else {
      # Skips cat if there's no false assessments in it
      Corrected_cats <- rbind(Corrected_cats, species)
    }
  }
  return(Corrected_cats)
}

# Reassign_Cats <- function(Species_History_Tags){
#   # Works out where/how to change any assessments labelled as false
#   Corrected_cats <- Species_History_Tags[NULL,]
#   # Splits by taxa
#   for (i in unique(Species_History_Tags$taxonid)){
#     species <- Species_History_Tags[Species_History_Tags$taxonid==i,]
#     # Gets the index values of false assessments
#     False_assess <- which(species$Verified=="False")
#     if (length(False_assess)==0){
#       # Skips cat if there's no false assessments in it
#       Corrected_cats <- rbind(Corrected_cats, species)
#     } else {
#       ## Send to Correction_func
#       species <- species %>% mutate(Prev_Veri = lag(Verified))
#       species <- species %>% mutate(Prev_year = ifelse(is.na(lag(year)) == FALSE, lag(year)-year, 9999))
#       species <- apply(species, 1, Correction_func)
#      #  for (j in False_assess){ # for each false assessment
#      # #   if (j > 1){
#      #      # If the following assessment was true
#      #      if (species$Verified[j-1]=="True"){
#      #        # If the following assessment happened within 5 years
#      #        if (species$year[j-1]-species$year[j]<=5){
#      #          # Give assessment the same catetgory as the following.
#      #          species$category[j] <- species$category[j-1]
#      #          species$Verified[j] <- "Corrected"
#      #        }
#      #        #### Might remove this bit. Unsure.
#      #      } else if (species$Verified[j-1]=="Corrected"){
#      #        # Find the subsequent true assessment to compare dates
#      #        True_assess <- which(species$Verified=="True")
#      #        True_assess <- subset(True_assess, True_assess<j)
#      #        True_assess <- max(True_assess)
#      #        # If the next true assessment happened within 5 years, copy category
#      #        if (species$year[True_assess]-species$year[j]<=5){
#      #          species$category[j] <- species$category[True_assess]
#      #          species$Verified[j] <- "Corrected"
#      #    #    }
#      #      }
#      #    }
#      #  }
#       Corrected_cats <- rbind(Corrected_cats, species)
#     }
#   }
#   return(Corrected_cats)
# }

Corrected_cats <- Reassign_Cats(Species_History_Tags)

Final_clean <- function(Corrected_cats){
  # Any remaining as false must not meet the criteria so should be dropped
  Corrected_cats <- Corrected_cats[which(Corrected_cats$Verified!="False"),]
  False_rem <- nrow(Species_History_Tags)-nrow(Corrected_cats)
  paste("False assessments removed: ", False_rem, sep="")
  # Remove species with only EX assessments
  for (i in unique(Corrected_cats$taxonid)){
    species_cats <- unique(Corrected_cats[Corrected_cats$taxonid==i,]$category)
    if (length(species_cats)==1 && species_cats=="EX"){
      Corrected_cats[Corrected_cats$taxonid==i,]$Verified <- "False"
      }
  }
  Corrected_cats <- Corrected_cats[which(Corrected_cats$Verified!="False"),]
  # Remove species with only one assessment remaining
  Corrected_cats <- Corrected_cats %>% group_by(taxonid) %>% filter(n()>1) %>% ungroup
  paste("Species with only one assessment remaining removed: ", (nrow(Species_History_Tags)-nrow(Corrected_cats))-False_rem, sep="")
  return(Corrected_cats)
}

Corrected_cats <- Final_clean(Corrected_cats)





####################################

#Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)
#write.csv(Corrected_cats, "../Data/Corrected_SpeciesHistory_deextinct.csv", row.names = FALSE)


# Create a vector of all species remaining after processing
Final_Species <- unique(Corrected_cats$taxonid)


########################

## Uncertainty function

Species_list <- unique(Species_History$taxonid)

Uncertainty <- function(Species_History, Species_list){
  # Take a random half of the species
  sub <- sample(Species_list, size = length(Species_list)/2)
  # subset the overall df
  sub_species <- subset(Species_History, Species_History$taxonid %in% sub)
  # Randomly generate tags for subset
  sub_species <- Generate_tags(sub_species, Cat_probs)
  # Clean!
  sub_species <- Reassign_Cats(sub_species)
  sub_species <- Final_clean(sub_species)
  return(sub_species)
}


####################################

## Threat and Taxon processing


# Read in the threat data
Species_Threats <- read.csv("../Data/Species_Threats_7.csv", header = T, stringsAsFactors = F)


# Generates heat map of threats
Threats_map <- function(Species_Threats){
  # rename id column
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Create a broat threat category
  Species_Threats <- Species_Threats %>% mutate(result.code = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # Remove duplicates and subet to only needed data
  Species_Threats <- unique(Species_Threats[1:2])
  Species_Threats$result.code <- as.numeric(Species_Threats$result.code)
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # create a vector with numbers of each threat
  threat_num <- table(Species_Threats$result.code)
  V <- crossprod(table(Species_Threats[1:2]))
  diag(V) <- 0
  coul <- colorRampPalette(brewer.pal(8, "BuPu"))(25)
  for (i in 1:12){
    V[,i] <- V[,i]/threat_num
  }
  levelplot(V, col.regions = coul, xlab = "Threat type", ylab = "Proportion of threat coocurring with other threats") # try cm.colors() or terrain.colors()
}

Threats_number <- function(Species_Threats){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # subset
  Species_Threats <- Species_Threats[1:2]
  # Get a tally of number of threats recorded
  Species_Threats <- Species_Threats %>% group_by(taxonid) %>% add_tally() %>% rename(all_count = n)
  # Broaden threat cats
  Species_Threats <- Species_Threats %>% mutate(result.code = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # unique subsets so only different threat cats are counted
  Species_Threats <- unique(Species_Threats)
  # Get a tally of number of different threats faced by each species
  Species_Threats <- Species_Threats %>% group_by(taxonid) %>% add_tally() %>% rename(gen_count = n)
  # subset again
  Species_Threats <- unique(Species_Threats[c(1,3,4)])
  return(Species_Threats)
}

Corrected_cats<- merge(Corrected_cats, Species_Threats, all = TRUE)



# Function for use in Precess_Threats
# Assigns certain labels to sub-groups
Assign_threats <- function(Species_Threats, Threats_cat){
  Threat <- NA
  if (Species_Threats["Threat_mid"] %in% Threats_cat[,"Codes"]){
    Threat <- as.character(Threats_cat[which(Threats_cat[,"Codes"] %in% Species_Threats["Threat_mid"]), "Threats"])
  }
  return(Threat)
}


# Func to process the threat data into main threat types
Process_Threats <- function(Species_Threats, Final_Species){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # Put the highest level of threat in a new column
  #Species_Threats <- Species_Threats %>% mutate(Threat_broad = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # Does the same for mid-level
  Species_Threats <- Species_Threats %>% mutate(Threat_mid = str_extract(result.code, "([0-9]+\\.[0-9]+)"))
  # Make a reference df of my categories and their codes
  Threats_cat <- data.frame(Threats = c("Agriculture", "Agriculture", "Agriculture", "Hunting", "Logging", "Fishing", "Invasives", "Climate", "Climate", "Climate", "Climate", "Climate"), Codes = c(2.1,2.2,2.3,5.1,5.3,5.4,8.1,11.1,11.2,11.3,11.4,11.5))
  # Assign those cats
  Threats_cat$Codes <- as.numeric(Threats_cat$Codes)
  Species_Threats$Threat_mid <- as.numeric(Species_Threats$Threat_mid)
  for (i in 1:nrow(Species_Threats)){
    Species_Threats$Threat_broad[i]<-Assign_threats(Species_Threats[i,], Threats_cat)
  }
  # Subset to only required columns
  Species_Threats <- Species_Threats[,c("taxonid", "Threat_broad")]
  # Work out a list of species with NO threats listed
  threatened_ids <- unique(Species_Threats$taxonid[is.na(Species_Threats$Threat_broad)==FALSE])
  NA_IDs <- unique(Species_Threats$taxonid[Species_Threats$taxonid %!in% threatened_ids])
  # Make a reference vector of the threats
  Threats <- unique(Species_Threats$Threat_broad[is.na(Species_Threats$Threat_broad)==FALSE])
  # Get a list of all the taxon ids of the species for each threat
  Threat_index <- lapply(Threats, function(i) unique(Species_Threats$taxonid[which(Species_Threats$Threat_broad==i)])) 
  # rename to the actual threats for clarity
  names(Threat_index) <- Threats
  Threat_index[["NA"]] <- NA_IDs
  return(Threat_index)
}
  
Threat_index <- Process_Threats(Species_Threats, Final_Species)


###### More generic version of above function, just sorts into base mid level cats.

# Func to process the threat data into main threat types
Process_Threats_generic <- function(Species_Threats, Final_Species){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # Put the highest level of threat in a new column
  # Does the same for mid-level
  Species_Threats <- Species_Threats %>% mutate(Threat_broad = str_extract(result.code, "([0-9]+)(?=\\.)"))
  ##### Optional line to go forward w/o cat 12
  Species_discard <- Species_Threats[which(Species_Threats$Threat_broad == "12"),]$taxonid
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %!in% Species_discard)
  # Subset to only required columns
  Species_Threats <- Species_Threats[,c("taxonid", "Threat_broad")]
  # Remove extra rows
  Species_Threats <- unique(Species_Threats)
  # Make a reference vector of the threats
  Threats <- unique(Species_Threats$Threat_broad)
  # Get a list of all the taxon ids of the species for each threat
  Threat_index <- lapply(Threats, function(i) unique(Species_Threats$taxonid[which(Species_Threats$Threat_broad==i)])) 
  # rename to the actual threats for clarity
  names(Threat_index) <- Threats
  return(Threat_index)
}

Threat_index_generic <- Process_Threats_generic(Species_Threats, Final_Species)

## Use Species_Data to subset by taxon.
Filter_taxa <-function(Species){
  classification <- NA
  if (Species["kingdom_name"] == "PLANTAE"){
    classification <- "Plant"
  } else if (Species["phylum_name"] != "CHORDATA"){
    classification <- "Invertebrate"    
  } else if (Species["class_name"] == "AMPHIBIA"){
    classification <- "Herptile"
  } else if (Species["class_name"] == "AVES"){
    classification <- "Bird"
  } else if (Species["class_name"] == "MAMMALIA"){
    classification <- "Mammal"
  } else if (Species["class_name"] == "REPTILIA"){
    classification <- "Herptile"
  } else {
    classification <- "Fish"
  }
  return(classification)
}

# Filter to highest sensible taxa and get ids for each
Process_taxa <- function(Species_Data, Final_Species){
  # Filter the data down to species that are included in the modelling
  Species_Data <- dplyr::filter(Species_Data, taxonid %in% Final_Species)
  # Label each species with its highest taxon
  Species_Data$Taxon <- apply(Species_Data, 1, Filter_taxa)
  # List of used taxa
  Taxa <- c("Plant", "Invertebrate", "Amphibian", "Bird", "Mammal", "Herptile", "Fish")
  # Get ids for each used taxa
  Taxa_index <- lapply(Taxa, function(i) unique(Species_Data$taxonid[which(Species_Data$Taxon==i)])) 
  # rename lists
  names(Taxa_index) <- Taxa
  return(Taxa_index)
}

Taxa_index<-Process_taxa(Species_Data, Final_Species)

Assign_taxa <- function(Species_History, Taxa_index){
    Species_History$Taxon <- NA
    for (i in 1:length(Taxa_index)){
      #Taxon_species <- which(Species_History$taxonid %in% Taxa_index[[i]])
      Species_History$Taxon[Species_History$taxonid %in% Taxa_index[[i]]] <- names(Taxa_index)[i]
    }
    return(Species_History)
}

Corrected_cats <- Assign_taxa(Corrected_cats, Taxa_index)










