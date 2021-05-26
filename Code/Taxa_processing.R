
# Create a vector of all species remaining after processing
Final_Species <- unique(Corrected_cats$taxonid)


########################

## Uncertainty function

# Species_list <- unique(Species_History$taxonid)
# 
# Uncertainty <- function(Species_History, Species_list){
#   # Take a random half of the species
#   sub <- sample(Species_list, size = length(Species_list)/2)
#   # subset the overall df
#   sub_species <- subset(Species_History, Species_History$taxonid %in% sub)
#   # Randomly generate tags for subset
#   sub_species <- Generate_tags(sub_species, Cat_probs)
#   # Clean!
#   sub_species <- Reassign_Cats(sub_species)
#   sub_species <- Final_clean(sub_species)
#   return(sub_species)
# }


####################################

## Threat and Taxon processing


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




