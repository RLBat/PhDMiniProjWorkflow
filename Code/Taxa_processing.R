require(ggplot2)

Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = T, stringsAsFactors = F)
#Species_Data <- read.csv("../Data/Species_Data.csv", stringsAsFactors = F)

########################

## Uncertainty function_

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
  } else if (Species["kingdom_name"] != "PLANTAE" && Species["kingdom_name"] != "ANIMALIA"){
    classification <- "Misc"
  } else if (Species["phylum_name"] != "CHORDATA" && Species["kingdom_name"] == "ANIMALIA"){
    classification <- "Invertebrate"    
  } else if (Species["class_name"] == "AMPHIBIA"){
    classification <- "Amphibian"
  } else if (Species["class_name"] == "AVES"){
    classification <- "Bird"
  } else if (Species["class_name"] == "MAMMALIA"){
    classification <- "Mammal"
  } else if (Species["class_name"] == "REPTILIA"){
    classification <- "Reptile"
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
  Taxa <- c("Plant", "Misc", "Invertebrate", "Amphibian", "Bird", "Mammal", "Reptile", "Fish")
  # Get ids for each used taxa
  Taxa_index <- lapply(Taxa, function(i) unique(Species_Data$taxonid[which(Species_Data$Taxon==i)])) 
  # rename lists
  names(Taxa_index) <- Taxa
  return(Taxa_index)
}

Assign_taxa <- function(Species_History, Taxa_index){
  Species_History$Taxon <- NA
  for (i in 1:length(Taxa_index)){
    #Taxon_species <- which(Species_History$taxonid %in% Taxa_index[[i]])
    Species_History$Taxon[Species_History$taxonid %in% Taxa_index[[i]]] <- names(Taxa_index)[i]
  }
  return(Species_History)
}


# write.csv(taxonomic_models[[1]], file = "../Data/taxa_mammal.csv")
# write.csv(not_taxonomic_models[[1]], file = "../Data/taxa_notmammal.csv")
# write.csv(taxonomic_models[[2]], file = "../Data/taxa_reptile.csv")
# write.csv(not_taxonomic_models[[2]], file = "../Data/taxa_notreptile.csv")
# write.csv(taxonomic_models[[3]], file = "../Data/taxa_fish.csv")
# write.csv(not_taxonomic_models[[3]], file = "../Data/taxa_notfish.csv")
# write.csv(taxonomic_models[[4]], file = "../Data/taxa_invert.csv")
# write.csv(not_taxonomic_models[[4]], file = "../Data/taxa_notinvert.csv")
# write.csv(taxonomic_models[[5]], file = "../Data/taxa_amphib.csv")
# write.csv(not_taxonomic_models[[5]], file = "../Data/taxa_notamphib.csv")
# write.csv(taxonomic_models[[6]], file = "../Data/taxa_plant.csv")
# write.csv(not_taxonomic_models[[6]], file = "../Data/taxa_notplant.csv")
# write.csv(taxonomic_models[[7]], file = "../Data/taxa_bird.csv")
# write.csv(not_taxonomic_models[[7]], file = "../Data/taxa_notbird.csv")

mammal <- read.csv(file = "../Data/taxa_mammal.csv")
notmammal <- read.csv(file = "../Data/taxa_notmammal.csv")
reptile <- read.csv(file = "../Data/taxa_reptile.csv")
notreptile <- read.csv(file = "../Data/taxa_notreptile.csv")
fish <- read.csv(file = "../Data/taxa_fish.csv")
notfish <- read.csv(file = "../Data/taxa_notfish.csv")
invert <- read.csv(file = "../Data/taxa_invert.csv")
notinvert <- read.csv(file = "../Data/taxa_notinvert.csv")
amphib <- read.csv(file = "../Data/taxa_amphib.csv")
notamphib <- read.csv(file = "../Data/taxa_notamphib.csv")
plant <- read.csv(file = "../Data/taxa_plant.csv")
notplant <- read.csv(file = "../Data/taxa_notplant.csv")
bird <- read.csv(file = "../Data/taxa_bird.csv")
notbird <- read.csv(file = "../Data/taxa_notbird.csv")

##### calculate the ratios

## start with just the mean

Mammals <- full_join(mammal, notmammal, by = c("X","Source", "Threat_level"))
Reptiles <- full_join(reptile, notreptile, by = c("X","Source", "Threat_level"))
Fish<- full_join(fish, notfish, by = c("X","Source", "Threat_level"))
Invertebrates<- full_join(invert, notinvert, by = c("X","Source", "Threat_level"))
Amphibians <- full_join(amphib, notamphib, by = c("X","Source", "Threat_level"))
Plants <- full_join(plant, notplant, by = c("X","Source", "Threat_level"))
Birds <- full_join(bird, notbird, by = c("X","Source", "Threat_level"))

Taxa_comp <- combine(Mammals, Reptiles, Fish, Invertebrates, Amphibians, Plants, Birds)
Taxa_comp$ratio <- log(Taxa_comp$Probability.x/Taxa_comp$Probability.y)
Taxa_comp$Threat_level <- as.factor(Taxa_comp$Threat_level)


#aight let's plot this bish

#split by threat
p <- ggplot(data = subset(Taxa_comp, Source %in% c("Mean") & Threat_level %in% c("5")), aes(x = source, y = ratio))
p <- p + geom_bar(stat = "identity", position = "dodge", fill = "black") + #scale_x_discrete(labels=c("Mammal", "Reptile", "Fish", "Invertebrate", "Amphibian", "Plant", "Bird")) +
  labs(x = "Taxonomic Group", tag = "Critically Endangered") + ylim(-2,2)
  #+  scale_fill_manual(values = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
               axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.title = element_text(size=20), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.9), 
               legend.title = element_text(size=14), strip.text = element_text(size=14), plot.tag.position = "top")
p









