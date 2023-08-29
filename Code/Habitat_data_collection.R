### Libraries

require(rredlist)
require(jsonlite)

API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b"


### Data

Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = T, stringsAsFactors = F)

####################
#### Collection ####

Final_Species_List <- unique(Historic_assess$scientific_name)
# List all mammals
iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
# Generate a list of all viable mammals
viable_mammals <- iucnmammals[which(iucnmammals %in% Final_Species_List)]

gather_habitat <- function(species){
  result <- rl_habitats(name = species, key = API_key)
  result$result$Species <- result$name
  species_habitat <- as.data.frame(result$result)
  return(species_habitat)
}

All_Mammals_Habitat <- lapply(viable_mammals, gather_habitat)

All_Mammals_Habitat <- bind_rows(All_Mammals_Habitat)

#write.csv(All_Mammals_Habitat, file = "../Data/Habitat_Data.csv", row.names = FALSE)

All_Habitat <- lapply(Final_Species_List, gather_habitat)
All_Habitat <- bind_rows(All_Habitat)

#write.csv(All_Habitat, file = "../Data/Habitat_Data.csv", row.names = FALSE)


