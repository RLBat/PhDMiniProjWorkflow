require(dplyr)

`%!in%` = Negate(`%in%`)

#######################################

Species <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header=T, stringsAsFactors = F)
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = T, stringsAsFactors = F)

Final_Species_List <- Historic_assess$scientific_name

## read in a list of viable species to save processing time

iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
## reduce this list to only viable species - i.e. ones that pass the cleaning steps.

mammal_bm <- read.csv("../Data/mammal_bodymass_2022.csv")

matched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %in% iucnmammals),]

matched_mammal <- matched_mammal[,c(1,3,5)]


### checked and corrected issues with out of date matches
# unmatched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %!in% iucnmammals),]
# mammal_bm$IUCN_name[2394] <- "Micronomus norfolkensis"
# mammal_bm$IUCN_name[3545] <- "Desmalopex leucopterus"
# mammal_bm$IUCN_name[4012] <- "Sorex monticola"
# mammal_bm$IUCN_name[4200] <- "Tamiops mcclellandii"
# mammal_bm$IUCN_name[4222] <- "Cephalopachus bancanus"
# 
# write.csv(mammal_bm, "../Data/mammal_bodymass_2022.csv", row.names = FALSE)
