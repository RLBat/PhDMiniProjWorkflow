### Libraries

require(rredlist)

### Data

Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = T, stringsAsFactors = F)

####################
#### Collection ####

Final_Species_List <- Historic_assess$scientific_name
# List all mammals
iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
# Generate a list of all viable mammals
viable_mammals <- iucnmammals[which(iucnmammals %in% Final_Species_List)]

All_Mammals_Habitat <- unlist(rl_habitats(name=viable_mammals[1], key = IUCN_REDLIST_KEY))

All_Mammals_Habitat <- sapply(viable_mammals[c(1,2,3)], rl_habitats, key=API_key, simple=FALSE)
