require(dplyr)
require(tidyverse)

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

## Run model as per the workflow

Boot_100 <- function(Boot_Probs){
  cats <- c("LC","NT","VU", "EN","CR", "EX")
  
  Boot_means <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, mean)
  Boot_top <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
  Boot_bottom <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))
  
  hundred_year <- rbind(Boot_means[100,2:6], Boot_bottom[100,2:6], Boot_top[100,2:6])
  hundred_year["Source"] <- c("Mean", "Bottom", "Top")
  hundred_year <- as.data.frame(hundred_year)
  hundred_year <- gather(hundred_year, key = "Threat_level", value = "Probability", 1:5)
  hundred_year$Threat_level <- recode(hundred_year$Threat_level, "LC" = 1, "NT" = 2, "VU" = 3, "EN" = 4, "CR" = 5)
  hundred_year$Threat_level <- as.factor(hundred_year$Threat_level)
  hundred_year$Probability <- as.numeric(hundred_year$Probability)
  return(hundred_year)
}


### graphing ###

birds_heavy_100 <- Boot_100(Bird_Heavy_boot)
birds_light_100 <- Boot_100(Bird_Light_boot)

birds_heavy_100[,4] <- "Heavy"
birds_light_100[,4] <- "Light"

birds_bm_100 <- rbind(birds_heavy_100, birds_light_100)
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]

birds_bm_100 <- birds_bm_100 %>% mutate(V4 = fct_relevel(V4, "Light", "Heavy"))

Plot_100 <- function(hundred_year){
  p <- ggplot(data = subset(hundred_year, Source %in% c("Mean")), aes(x = Threat_level, y = Probability, fill = V4)) + scale_fill_manual(values = c("cyan3", "tomato3"), name = "Body Mass")
  p <- p + geom_bar(stat = "identity", position = "dodge") + scale_x_discrete(breaks = 1:5, labels=c("LC","NT","VU", "EN","CR"))
  p <- p + labs(y = "Probability of extinction at t=100", x = "IUCN Species Threat Assessment")
  p <- p + geom_errorbar(aes(ymin= hundred_year$Probability[hundred_year$Source == "Bottom"], ymax=hundred_year$Probability[hundred_year$Source == "Top"]), width=.2, position=position_dodge(.9)) 
  p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                 axis.text.y = element_text(size=16), axis.title = element_text(size=20), axis.text.x = element_text(size=16), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
  return(p)
}

Plot_100(birds_bm_100)

t.test(Bird_Heavy_boot[which(Bird_Heavy_boot$Time == 100), "EN"], Bird_Light_boot[which(Bird_Light_boot$Time == 100), "EN"])


# 
# 
# Mammal_bm_100$
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Final_Species_List <- Historic_assess$scientific_name
# 
# ## read in a list of viable species to save processing time
# 
# iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
# ## reduce this list to only viable species - i.e. ones that pass the cleaning steps.
# 
# mammal_bm <- read.csv("../Data/mammal_bodymass_2022.csv")
# 
# matched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %in% iucnmammals),]
# 
# matched_mammal <- matched_mammal[,c(1,3,5)]


### checked and corrected issues with out of date matches
# unmatched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %!in% iucnmammals),]
# mammal_bm$IUCN_name[2394] <- "Micronomus norfolkensis"
# mammal_bm$IUCN_name[3545] <- "Desmalopex leucopterus"
# mammal_bm$IUCN_name[4012] <- "Sorex monticola"
# mammal_bm$IUCN_name[4200] <- "Tamiops mcclellandii"
# mammal_bm$IUCN_name[4222] <- "Cephalopachus bancanus"
# 
# write.csv(mammal_bm, "../Data/mammal_bodymass_2022.csv", row.names = FALSE)
