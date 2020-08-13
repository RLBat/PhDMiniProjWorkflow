
## Code for the HPC! ##

############################
## PACKAGES

require(dplyr)
require(tidyverse)
require(doParallel)
require(foreach)

`%!in%` = Negate(`%in%`)


############################
## DATA

Species_History <- read.csv("../Data/SpeciesHistory_Tags_deextinct.csv", header = T, stringsAsFactors = F)

###########################
## FUNCTIONS

# Generate probabilities to use in random assignment
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

#### Generate the random tags
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
            # If the following assessment happened within 10 years
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
            # If the next true assessment happened within 10 years, copy category
            if (species$year[True_assess]-species$year[j]<=10){
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

registerDoParallel(12)
foreach(i = 1:100) %dopar% {
  ##########################
  ## SCRIPT
  
  # Gets the iteration number from the HPC
  #iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
  iter <- i
  
  # sets the random seed based on that iter number
  set.seed(iter)
  
  # Generate probabilities
  Cat_probs <- Define_probabilities(Species_History)
  
  # Assign tags based on probabilities
  Species_History_Tags <- Generate_tags(Species_History, Cat_probs)
  
  # Reassign false tags where possible
  Corrected_cats <- Reassign_Cats(Species_History_Tags)
  
  # Final cleaning steps
  Corrected_cats <- Final_clean(Corrected_cats)
  
  ###########################
  ## SAVE
  write.csv(Corrected_cats, file = paste("../Data/2020_full_random", as.character(iter), ".csv", sep=""), row.names = FALSE)
}


