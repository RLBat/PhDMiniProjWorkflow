require(dplyr)

`%!in%` = Negate(`%in%`)

#######################################

Species_Data <- read.csv("../Data/Species_Data.csv")
Species <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header=T, stringsAsFactors = F)
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = T, stringsAsFactors = F)

# Grab the species with enough data to model post-cleaning
Final_Species_List <- Historic_assess$scientific_name

split_bm <- function(bm_data, split_no = 3){
  # initialise vecotr
  splits <- c()
  for(i in 1:split_no){
    # create a vector of the splits
    splits <- append(splits, i/split_no)
  }
  #remove any that are 0 or one to avoid errors
  splits <- splits[! splits %in% c(0,1)]
  
  #threshold for the split
  bm_threshold <- quantile(bm_data$body_mass, splits)
  
  return(bm_threshold)
}

########### MAMMALS ######

# List all mammals
iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
# Generate a list of all viable mammals
viable_mammals <- iucnmammals[which(iucnmammals %in% Final_Species_List)]
# import body mass data (which was manually taxon matched by Yuheng)
mammal_bm <- read.csv("../Data/mammal_bodymass_2022.csv")
# reduce body mass data down to viable species
matched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %in% viable_mammals),c(1,3,5)]

# make a df with only the required species and add the body mass to each
Mammal_bm_assessments <- Historic_assess[which(Historic_assess$scientific_name %in% matched_mammal$IUCN_name),]
# rename cols for merge
names(matched_mammal) <- c("taxonid", "scientific_name", "body_mass")
# merge dfs to add body mass to the historic assessment data 
Mammal_bm_assessments <- merge(Mammal_bm_assessments, matched_mammal)

mammal_median <- split_bm(mammal_bm, 2)
# split into heavy and light
mammal_heavy <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass > mammal_median),]
mammal_light <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass < mammal_median),]

mammal_thirds <- split_bm(mammal_bm, 3)
# split into 3
mammal_bottom <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass < mammal_thirds[1]),]
mammal_middle <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass > mammal_thirds[1] & Mammal_bm_assessments$body_mass < mammal_thirds[2]),]
mammal_top <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass > mammal_thirds[2]),]

############ BIRDS ####

# import bird body mass data
birds_bm <- read.csv("../Data/Yuheng/viablebirds_mass_complete.csv")
birds_bm <- birds_bm[,c(2:5)]
names(birds_bm)[4] <- "body_mass"

# List all birds
iucnbirds <- Species_Data[which(Species_Data$class_name == "AVES"),"scientific_name"]
# Generate a list of all viable birds
viable_birds <- iucnbirds[which(iucnbirds %in% Final_Species_List)]
# reduce body mass data down to viable species
matched_birds <- birds_bm[which(birds_bm$IUCN_name %in% viable_birds),c(1,3,4)]
unmatched_birds <- birds_bm[which(birds_bm$IUCN_name %!in% viable_birds),c(1,3,4)]

# make a df with only the required species and add the body mass to each
bird_bm_assessments <- Historic_assess[which(Historic_assess$scientific_name %in% matched_birds$IUCN_name),]
# rename cols for merge
names(matched_birds) <- c("taxonid", "scientific_name", "body_mass")
# merge dfs to add body mass to the historic assessment data 
bird_bm_assessments <- merge(bird_bm_assessments, matched_birds)

bird_median <- split_bm(birds_bm, 2)
# split into two
birds_light <- bird_bm_assessments[which(bird_bm_assessments$body_mass < bird_median),]
birds_heavy <- bird_bm_assessments[which(bird_bm_assessments$body_mass > bird_median),]

bird_third <- split_bm(birds_bm, 3)
# split into three
birds_bottom <- bird_bm_assessments[which(bird_bm_assessments$body_mass < bird_third[1]),]
birds_middle <- bird_bm_assessments[which(bird_bm_assessments$body_mass > bird_third[1] & bird_bm_assessments$body_mass < bird_third[2]),]
birds_top <- bird_bm_assessments[which(bird_bm_assessments$body_mass > bird_third[2]),]

###############################

### graphing ###

Mammal_Heavy_100[,4] <- "Heavy"
Mammal_Light_100[,4] <- "Light"

Mammal_bm_100 <- rbind(Mammal_Heavy_100, Mammal_Light_100)
Mammal_bm_100 <- Mammal_bm_100[which(Mammal_bm_100$Source == "Mean"),]

Mammal_bm_100 <- Mammal_bm_100 %>% mutate(V4 = fct_relevel(V4, "Light", "Heavy"))

p <- ggplot(data = Mammal_bm_100, aes(x = Threat_level, y = Probability, fill = V4)) + scale_fill_manual(values = c("cyan3", "tomato3"))
p <- p + geom_bar(stat = "identity", position = "dodge")
p


Mammal_bm_100$










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
