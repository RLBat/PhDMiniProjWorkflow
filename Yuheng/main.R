# Author: Rachel Bates (r.bates18@imperial.ac.uk), Yuheng Sun (yuheng.sun20@imperial.ac.uk)
# Date: 08/03/2021

require(rjson)
require(dplyr)
require(tidyverse)
require(rredlist)
require(jsonlite)
require(doParallel)

`%!in%` = Negate(`%in%`)


######################################
### DATA PROCESSING ###
################################

# HISTORICAL ASSESSMENT DATA

# Read in files:
Table7 <- read.csv("Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)
Species_History <- read.csv("Data/Species_History.csv")
Species_Data <- read.csv("Data/Species_Data.csv")
source("Code/Data_Processing.R")
# Initial data cleaning, removing invalid or unusable assessments:
Species_History <- Assess_Clean(Species_History)
# Clean up table 7 data for use:
Cat_Changes <- Add_table7_tags(Table7, Species_History)
# Assign any known tags:
# (Takes a long time!!!)
Species_History <- Assign_known_tags(Cat_Changes, Species_History)
# Work out the probabilities of an assessment being gen or non-gen:
Cat_probs <- Define_probabilities(Species_History)
Species_History_Tags <- Generate_tags(Species_History, Cat_probs)
# Reassign false tags where possible:
Corrected_cats <- Reassign_Cats(Species_History_Tags)
# Final cleaning steps:
Corrected_cats <- Final_clean(Corrected_cats)
# Save the file:
write.csv(Corrected_cats, "My_Workflow/Corrected_SpeciesHistory_deextinct.csv", row.names = FALSE)

#################################
# MAMMAL BODY MASS DATA

# Read in files:
rachlist <- read.csv("Data/All_viable_species_Rach.csv") # This is a list of all viable species prepared by Rachel.
iucnmammals <- fromJSON(file = "Data/iucnmammals.json") # A list of mammals from IUCN API.
# Extract scientific names that used by IUCN:
iucnmammals <- iucnmammals$result
mammalnames <- character()
for (i in (1:length(iucnmammals))) {
  mammalnames <- c(mammalnames, iucnmammals[[i]]$scientific_name)
}
mammalnames <- sort(mammalnames)
# Extract viable mammal species using Rach's list:
mammalviable <- character()
for (i in mammalnames) {
  if (i%in%rachlist$scientific_name) {
    mammalviable <- c(mammalviable, i)
  }
}
# Data input (mammals bodymass):
mamfuncdat <- read.csv("raw/MammalBodyMassDatabase/EltonTraits 1.0(2013)/MamFuncDat.csv")
mammalbodymass <- mamfuncdat[c(2,24)]
mammalbodymass <- arrange(mammalbodymass, Scientific)
# Scientific names matching
namesmatching <- data.frame(SciName=character(), Match=logical(), OnlyIUCN=logical(), OnlyMass=logical())
for (i in (1:length(mammalviable))) {
  if (mammalviable[i]%in%mammalbodymass$Scientific) {
    temp <- data.frame(SciName=mammalviable[i], Match=TRUE, OnlyIUCN=FALSE, OnlyMass=FALSE)
  } else {
    temp <- data.frame(SciName=mammalviable[i], Match=FALSE, OnlyIUCN=TRUE, OnlyMass=FALSE)
  }
  namesmatching <- rbind(namesmatching, temp)
}
OnlyInIUCN <- namesmatching%>%filter(OnlyIUCN==TRUE)
# I found that there are 628 unmatches, so must do something:
OnlyInMass <- data.frame(SciName=character(), Match=logical(), OnlyIUCN=logical(), OnlyMass=logical())
for (i in (1:nrow(mammalbodymass))) {
  if (!mammalbodymass[i,1]%in%mammalviable) {
    temp <- data.frame(SciName=mammalbodymass[i,1], Match=FALSE, OnlyIUCN=FALSE, OnlyMass=TRUE)
    OnlyInMass <- rbind(OnlyInMass, temp)
  }
}
namesmatching <- rbind(namesmatching, OnlyInMass)
# Noticed that some unmatches are caused by subspecies recorded in IUCN, check how many they are:
ssps <- grep("ssp.", as.vector(OnlyInIUCN$SciName))
length(ssps) # 239
# Remove all the subspecies and save the file:
IUCNwithoutSSPs <- OnlyInIUCN[-ssps,]
write.csv(IUCNwithoutSSPs, "unmatchedmammals.csv")

# Manually investigate taxonomy & find body mass data and record in Excel


#######################################
# HABITAT DATA

IUCN_REDLIST_KEY <- "*"

# Download habitat data:
# Mammals:
All_Mammals <- rl_comp_groups('mammals', key = IUCN_REDLIST_KEY)
All_Mammals_ID <- All_Mammals[[2]][,1]
All_Mammals_Habitat <- rl_habitats(id=All_Mammals_ID[1], key = IUCN_REDLIST_KEY)
for (i in (2:length(All_Mammals_ID))) {
  temp <- rl_habitats(id=All_Mammals_ID[i], key = IUCN_REDLIST_KEY)
  All_Mammals_Habitat <- rbind(All_Mammals_Habitat, temp)
  print(i)
}
write_json(All_Mammals_Habitat, "Mammal_Habitat.json")
All_Mammals_Habitat <- read_json("Mammal_Habitat.json", simplifyVector = FALSE)

# Birds:
All_Birds <- rl_comp_groups('birds', key = IUCN_REDLIST_KEY)
All_Birds_ID <- All_Birds[[2]][,1]
All_Birds_Habitat <- rl_habitats(id=All_Birds_ID[1], key = IUCN_REDLIST_KEY)
for (i in (2:length(All_Birds_ID))) {
  temp <- rl_habitats(id=All_Birds_ID[i], key = IUCN_REDLIST_KEY)
  All_Birds_Habitat <- rbind(All_Birds_Habitat, temp)
  print(i)
}
write_json(All_Birds_Habitat, "Bird_Habitat.json")


#######################################
### GROUPING & MODELLING ###
#################################

# Read in historical assessment data (prepared by Rach in 2020):
Corrected_cats <- read.csv("Data/Corrected_SpeciesHistory_deextinct.csv", stringsAsFactors = T)

source("Code/Markov_Modelling.R")

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

##########################
# BODY MASS

# MAMMALS
# Read in data:
Bodymass_Mammal <- read.csv("Data/bodymass_mammal_ready.csv")
# Read in historical assessments.
Species_History_All <- read.csv("Data/Species_History.csv", stringsAsFactors = T)
# Reduce the data down to what we need.
Species_History_Mammal <- Species_History_All%>%filter(Species_History_All$taxonid%in%Bodymass_Mammal$IUCN_ID)
length(unique(Species_History_Mammal$taxonid))
# The result is 4483. 26 species are lost here. Find out which they are:
lost_species <- Bodymass_Mammal%>%filter(Bodymass_Mammal$IUCN_ID%!in%Species_History_All$taxonid)
lost_species
# Read in Rach's species data:
Species_Data_All <- read.csv("Data/Species_Data.csv", stringsAsFactors = F)
# Recode those species' ID to be consistent with Rach's data:
lost_species_recode <- lost_species
for (i in (1:nrow(lost_species))) {
  if (lost_species$IUCN_name[i]%in%Species_Data_All$scientific_name) {
    temp <- Species_Data_All%>%filter(Species_Data_All$scientific_name==lost_species$IUCN_name[i])
    lost_species_recode$IUCN_ID[i] <- temp$taxonid
  } else {
    print(paste(i, ": cannot find the name"))
  }
}
Bodymass_Mammal_Recode <- Bodymass_Mammal
for (i in (1:nrow(Bodymass_Mammal))) {
  if (Bodymass_Mammal$IUCN_name[i]%in%lost_species_recode$IUCN_name) {
    temp <- lost_species_recode%>%filter(lost_species_recode$IUCN_name==Bodymass_Mammal$IUCN_name[i])
    Bodymass_Mammal_Recode$IUCN_ID[i] <- temp$IUCN_ID
  }
}
# Check:
Species_History_Mammal <- Species_History_All%>%filter(Species_History_All$taxonid%in%Bodymass_Mammal_Recode$IUCN_ID)
length(unique(Species_History_Mammal$taxonid))
# Group all mammals into heavy (> the median) and light (<= the median):
med <- median(Bodymass_Mammal$body_mass)
Mammal_Mass_Heavy <- character()
Mammal_Mass_Light <- character()
for (i in (1:nrow(Bodymass_Mammal_Recode))) {
  if (Bodymass_Mammal_Recode[i,5]>med) {
    Mammal_Mass_Heavy <- c(Mammal_Mass_Heavy, Bodymass_Mammal_Recode[i,1])
  } else {
    Mammal_Mass_Light <- c(Mammal_Mass_Light, Bodymass_Mammal_Recode[i,1])
  }
}
# Group assessments:
Historic_assess_Mammal_Heavy <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Mass_Heavy)
Historic_assess_Mammal_Light <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Mass_Light)
summary(Historic_assess_Mammal_Heavy$category) # This is to look at how many EX assessments are included in each group, to make sure there is enough for the model to work.
summary(Historic_assess_Mammal_Light$category)
# Model:
model_mammal_heavy_med <- Run_Markov(Historic_assess_Mammal_Heavy, Q)
model_mammal_light_med <- Run_Markov(Historic_assess_Mammal_Light, Q)
# Bootstrapping, this takes a while (~15 mins):
Boot_models_h <- boot.msm(model_mammal_heavy_med, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_l <- boot.msm(model_mammal_light_med, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_h[!sapply(Boot_models_h, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_heavy_mammal_med.csv", row.names = FALSE)
# Repeat the process in the light group:
boot.list2 <- Boot_models_l[!sapply(Boot_models_l, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_light_mammal_med.csv", row.names = FALSE)

# Group all mammals into heavy (> 3 kg) and light (<= 3 kg):
Mammal_Mass_Heavy <- character()
Mammal_Mass_Light <- character()
for (i in (1:nrow(Bodymass_Mammal_Recode))) {
  if (Bodymass_Mammal_Recode[i,5]>3000) {
    Mammal_Mass_Heavy <- c(Mammal_Mass_Heavy, Bodymass_Mammal_Recode[i,1])
  } else {
    Mammal_Mass_Light <- c(Mammal_Mass_Light, Bodymass_Mammal_Recode[i,1])
  }
}
# Group assessments:
Historic_assess_Mammal_Heavy <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Mass_Heavy)
Historic_assess_Mammal_Light <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Mass_Light)
summary(Historic_assess_Mammal_Heavy$category) # This is to look at how many EX assessments are included in each group, to make sure there is enough for the model to work.
summary(Historic_assess_Mammal_Light$category)
# Model:
model_mammal_heavy_3kg <- Run_Markov(Historic_assess_Mammal_Heavy, Q)
model_mammal_light_3kg <- Run_Markov(Historic_assess_Mammal_Light, Q)
# Bootstrapping, this takes a while (~15 mins):
Boot_models_h <- boot.msm(model_mammal_heavy_3kg, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_l <- boot.msm(model_mammal_light_3kg, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_h[!sapply(Boot_models_h, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_heavy_mammal.csv", row.names = FALSE)
# Repeat the process in the light group:
boot.list2 <- Boot_models_l[!sapply(Boot_models_l, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_light_mammal.csv", row.names = FALSE)

# For PE ratio and threshold relationship:
# Make a vector of thresholds:
threshold <- c(seq(10, 200, 10), seq(300, 3500, 100))
# A function that outputs a list of 100 y extinction probabilities in heavy and light mammals at a single threshold:
threshold_list <- function(threshold) {
  heavy_mammals <- character()
  light_mammals <- character()
  for (i in (1:nrow(Bodymass_Mammal_Recode))) {
    if (Bodymass_Mammal_Recode[i,5]>threshold) {
      heavy_mammals <- c(heavy_mammals, Bodymass_Mammal_Recode[i,1])
    } else {
      light_mammals <- c(light_mammals, Bodymass_Mammal_Recode[i,1])
    }
  }
  print(c(threshold, "heavy:", length(heavy_mammals), "light", length(light_mammals)))
  Historic_assess_Heavy <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%heavy_mammals)
  Historic_assess_Light <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%light_mammals)
  print(c(threshold, "heavy(assessments):", nrow(Historic_assess_Heavy), "light(assessments)", nrow(Historic_assess_Light)))
  model_heavy <- Run_Markov(Historic_assess_Heavy, Q)
  model_light <- Run_Markov(Historic_assess_Light, Q)
  # Bootstrapping:
  Boot_output_h <- boot.msm(model_heavy, stat = NULL, B=100, cores = (detectCores()-1))
  Boot_output_h <- Boot_output_h%>%mutate(Group="heavy")
  print(c(threshold, "heavy"))
  Boot_output_l <- boot.msm(model_light, stat = NULL, B=100, cores = (detectCores()-1))
  Boot_output_l <- Boot_output_l%>%mutate(Group="light")
  print(c(threshold, "light"))
  output <- rbind(Boot_output_h, Boot_output_l)
  return(output)
}
# Make a dataframe saving the bootstrapping results at each threshold:
threshold_loop <- data.frame()
for (ii in threshold) {
  temp2 <- threshold_list(ii)
  temp2 <- temp2%>%mutate(Threshold=ii)
  threshold_loop <- rbind(threshold_loop, temp2)
  print(ii)
}
# Save the file
write.csv(threshold_loop, file = "threshold_mammal.csv", row.names = FALSE)

# BIRDS
# Data input (prepared bird body mass data):
Bodymass_Bird <- read.csv("viablebirds_mass_complete.csv")
# Reduce the dataset:
Species_History_Bird <- Species_History_All%>%filter(Species_History_All$taxonid%in%Bodymass_Bird$IUCN_ID)
length(unique(Species_History_Bird$taxonid)) # 11 species are lost.
# Recode. Same as in mammals:
lost_species <- Bodymass_Bird%>%filter(Bodymass_Bird$IUCN_ID%!in%Species_History_All$taxonid)
lost_species_recode <- lost_species
for (i in (1:nrow(lost_species))) {
  if (lost_species$IUCN_name[i]%in%Species_Data_All$scientific_name) {
    temp <- Species_Data_All%>%filter(Species_Data_All$scientific_name==lost_species$IUCN_name[i])
    lost_species_recode$IUCN_ID[i] <- temp$taxonid
  } else {
    print(paste(i, ": cannot find the name"))
  }
}
lost_species
lost_species_recode
Bodymass_Bird_Recode <- Bodymass_Bird
for (i in (1:nrow(Bodymass_Bird))) {
  if (Bodymass_Bird$IUCN_name[i]%in%lost_species_recode$IUCN_name) {
    temp <- lost_species_recode%>%filter(lost_species_recode$IUCN_name==Bodymass_Bird$IUCN_name[i])
    Bodymass_Bird_Recode$IUCN_ID[i] <- temp$IUCN_ID
  }
}
# Check:
Species_History_Bird <- Species_History_All%>%filter(Species_History_All$taxonid%in%Bodymass_Bird_Recode$IUCN_ID)
length(unique(Species_History_Bird$taxonid))
# Group all mammals into heavy (> 80 g) and light (<= 80 g):
med <- median(Bodymass_Bird_Recode$body_mass)
Bird_Mass_Heavy <- character()
Bird_Mass_Light <- character()
for (i in (1:nrow(Bodymass_Bird_Recode))) {
  if (Bodymass_Bird_Recode[i,5]>80) {
    Bird_Mass_Heavy <- c(Bird_Mass_Heavy, Bodymass_Bird_Recode[i,1])
  } else {
    Bird_Mass_Light <- c(Bird_Mass_Light, Bodymass_Bird_Recode[i,1])
  }
}
# Group assessments:
Historic_assess_Bird_Heavy <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Mass_Heavy)
Historic_assess_Bird_Light <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Mass_Light)
summary(Historic_assess_Bird_Heavy$category) # This is to look at how many EX assessments are included in each group, to make sure there is enough for the model to work.
summary(Historic_assess_Bird_Light$category)
# Model:
model_bird_heavy <- Run_Markov(Historic_assess_Bird_Heavy, Q)
model_bird_light <- Run_Markov(Historic_assess_Bird_Light, Q)
# Bootstrapping, this takes a while (~15 mins):
Boot_models_h_b <- boot.msm(model_bird_heavy, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_l_b <- boot.msm(model_bird_light, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_h_b[!sapply(Boot_models_h, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_heavy_bird80.csv", row.names = FALSE)
# Repeat the process in the light group:
boot.list2 <- Boot_models_l_b[!sapply(Boot_models_l, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_light_bird80.csv", row.names = FALSE)

# For PE ratio and threshold relationship:
# Make a vector of thresholds:
threshold <- c(seq(10, 200, 10), seq(250, 1000, 50))
# A function that outputs a list of 100 y extinction probabilities in heavy and light birds at a single threshold:
threshold_list_bird <- function(threshold) {
  heavy_birds <- character()
  light_birds <- character()
  for (i in (1:nrow(Bodymass_Bird_Recode))) {
    if (Bodymass_Bird_Recode[i,5]>threshold) {
      heavy_birds <- c(heavy_birds, Bodymass_Bird_Recode[i,2])
    } else {
      light_birds <- c(light_birds, Bodymass_Bird_Recode[i,2])
    }
  }
  print(c(threshold, "heavy:", length(heavy_birds), "light", length(light_birds)))
  Historic_assess_Heavy <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%heavy_birds)
  Historic_assess_Light <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%light_birds)
  print(c(threshold, "heavy(assessments):", nrow(Historic_assess_Heavy), "light(assessments)", nrow(Historic_assess_Light)))
  model_heavy <- Run_Markov(Historic_assess_Heavy, Q)
  model_light <- Run_Markov(Historic_assess_Light, Q)
  # Bootstrapping:
  Boot_output_h <- boot.msm(model_heavy, stat = NULL, B=100, cores = (detectCores()-1))
  Boot_output_h <- Boot_output_h%>%mutate(Group="heavy")
  print(c(threshold, "heavy"))
  Boot_output_l <- boot.msm(model_heavy, stat = NULL, B=100, cores = (detectCores()-1))
  Boot_output_l <- Boot_output_l%>%mutate(Group="light")
  print(c(threshold, "light"))
  output <- rbind(Boot_output_h, Boot_output_l)
  return(output)
}
# Make a dataframe saving the bootstrapping results at each threshold:
threshold_loop <- data.frame()
for (ii in threshold) {
  temp2 <- threshold_list_bird(ii)
  temp2 <- temp2%>%mutate(Threshold=ii)
  threshold_loop <- rbind(threshold_loop, temp2)
  print(ii)
}
# Save the file:
write.csv(threshold_loop, file = "threshold_bird.csv", row.names = FALSE)

# Trisection in birds:
# Arrange the body mass from light to heavy:
Bodymass_Bird_Recode <- arrange(Bodymass_Bird_Recode, Mass)
# Calculate the number of rows in each group:
x <- nrow(Bodymass_Bird_Recode)/3
# Trisect the dataset:
light <- Bodymass_Bird_Recode[(1:x),]
middle <- Bodymass_Bird_Recode[(x:(2*x)),]
heavy <- Bodymass_Bird_Recode[((2*x):nrow(Bodymass_Bird_Recode)),]
# Group the assessments:
Historic_assess_l <- Historic_assess%>%filter(Historic_assess$taxonid%in%light$IUCN_ID)
Historic_assess_m <- Historic_assess%>%filter(Historic_assess$taxonid%in%middle$IUCN_ID)
Historic_assess_h <- Historic_assess%>%filter(Historic_assess$taxonid%in%heavy$IUCN_ID)
# Model:
model_bird_triL <- Run_Markov(Historic_assess_l, Q)
model_bird_triM <- Run_Markov(Historic_assess_m, Q)
model_bird_triH <- Run_Markov(Historic_assess_h, Q)
# Bootstrap:
Boot_models_1 <- boot.msm(model_mammal_triL, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_2 <- boot.msm(model_mammal_triM, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_3 <- boot.msm(model_mammal_triH, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_1[!sapply(Boot_models_1, function(x) class(x) == "try-error")]
Boot_probs <- lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_1_bird.csv", row.names = FALSE)
# Repeat the process in the 2nd group:
boot.list2 <- Boot_models_2[!sapply(Boot_models_2, function(x) class(x) == "try-error")]
Boot_probs <- lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_2_bird.csv", row.names = FALSE)
# Repeat the process in the 3rd group:
boot.list2 <- Boot_models_3[!sapply(Boot_models_3, function(x) class(x) == "try-error")]
Boot_probs <- lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_3_bird.csv", row.names = FALSE)

####################################
# HABITAT BREADTH

# Read in files:
All_Mammals_Habitat <- read_json("Mammal_Habitat.json", simplifyVector = FALSE)
All_Birds_Habitat <- read_json("Bird_Habitat.json", simplifyVector = FALSE)
# Recode 9.8.1 ~ 9.8.6 (different types of coral reef habitat) all into 9.8 (coral reef) and convert all codes into numeric:
coral <- c("9.8.1", "9.8.2", "9.8.3", "9.8.4", "9.8.5", "9.8.6")
Recode_Coral <- function(All_Habitat, coral) {
  for (i in (1:length(All_Habitat))) {
    if (length(All_Habitat[[i]][[2]])>0) {
      for (ii in (1:length(All_Habitat[[i]][[2]]))) {
        if (sum(All_Habitat[[i]][[2]][[ii]]$code==coral)>0) {
          print(c(i, ii, All_Habitat[[i]][[2]][[ii]]$code))
          All_Habitat[[i]][[2]][[ii]]$code <- "9.8"
        }
        All_Habitat[[i]][[2]][[ii]]$code <- as.numeric(All_Habitat[[i]][[2]][[ii]]$code)
      }
    }
  }
  return(All_Habitat)
}
All_Mammals_Habitat <- Recode_Coral(All_Mammals_Habitat, coral)
All_Birds_Habitat <- Recode_Coral(All_Birds_Habitat, coral)
# Calculate number of habitat types that a species live in:
Habitat_Types <- function(All_Habitat) {
  Habitat_Types_count <- data.frame()
  for (i in (1:length(All_Habitat))) {
    if (length(All_Habitat[[i]][[2]])>0) {
      temp <- numeric()
      for (ii in (1:length(All_Habitat[[i]][[2]]))) {
        if (All_Habitat[[i]][[2]][[ii]]$code<18) {
          temp <- c(temp, floor(All_Habitat[[i]][[2]][[ii]]$code)) # Round down all the codes and save them in a temporary vector
        }
      }
      temp2 <- c(All_Habitat[[i]][[1]][[1]], length(unique(temp)))
      Habitat_Types_count <- rbind(Habitat_Types_count, temp2)
    }
  }
  return(Habitat_Types_count)
}
Habitat_Types_Mammal <- Habitat_Types(All_Mammals_Habitat)
Habitat_Types_Mammal[,2] <- as.numeric(Habitat_Types_Mammal[,2])
Habitat_Types_Bird <- Habitat_Types(All_Birds_Habitat)
Habitat_Types_Bird[,2] <- as.numeric(Habitat_Types_Bird[,2])

# MAMMAL
# Group mammals into (1) only inhabit 1 category of habitat, (2) inhabitat 2 or more categories of habitat:
Mammal_Habitat_Single <- Habitat_Types_Mammal%>%filter(Habitat_Types_Mammal[,2]==1)
Mammal_Habitat_Multiple <- Habitat_Types_Mammal%>%filter(Habitat_Types_Mammal[,2]>1)
# Group assessments:
Historic_assess_Mammal_sg <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Habitat_Single[,1])
Historic_assess_Mammal_mt <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Habitat_Multiple[,1])
summary(Historic_assess_Mammal_sg$category) # This is to look at how many EX assessments are included in each group, to make sure there is enough for the model to work.
summary(Historic_assess_Mammal_mt$category)
# Model:
model_mammal_sg <- Run_Markov(Historic_assess_Mammal_sg, Q)
model_mammal_mt <- Run_Markov(Historic_assess_Mammal_mt, Q)
# Bootstrap:
Boot_models_sg <- boot.msm(model_mammal_sg, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_mt <- boot.msm(model_mammal_mt, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_sg[!sapply(Boot_models_sg, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_sg_mammal.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_mt[!sapply(Boot_models_mt, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_mt_mammal.csv", row.names = FALSE)

# Group mammals into (1) only inhabit 2 category of habitat, (2) inhabitat 3 or more categories of habitat:
Mammal_Habitat_Single2 <- Habitat_Types_Mammal%>%filter(Habitat_Types_Mammal[,2]==1)
Mammal_Habitat_Multiple2 <- Habitat_Types_Mammal%>%filter(Habitat_Types_Mammal[,2]>1)
# Group assessments:
Historic_assess_Mammal_sg2 <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Habitat_Single2[,1])
Historic_assess_Mammal_mt2 <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Habitat_Multiple2[,1])
summary(Historic_assess_Mammal_sg$category) # This is to look at how many EX assessments are included in each group, to make sure there is enough for the model to work.
summary(Historic_assess_Mammal_mt$category)
# Model:
model_mammal_sg2 <- Run_Markov(Historic_assess_Mammal_sg2, Q)
model_mammal_mt2 <- Run_Markov(Historic_assess_Mammal_mt2, Q)
# Bootstrap:
Boot_models_sg2 <- boot.msm(model_mammal_sg2, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_mt2 <- boot.msm(model_mammal_mt2, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_sg2[!sapply(Boot_models_sg2, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_sg2_mammal.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_mt2[!sapply(Boot_models_mt2, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_mt2_mammal.csv", row.names = FALSE)

# BIRDS
# Group birds into (1) only inhabit 1 category of habitat, (2) inhabitat 2 or more categories of habitat:
Bird_Habitat_Single <- Habitat_Types_Bird%>%filter(Habitat_Types_Bird[,2]==1)
Bird_Habitat_Multiple <- Habitat_Types_Bird%>%filter(Habitat_Types_Bird[,2]>1)
# Group assessments:
Historic_assess_Bird_sg <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Habitat_Single[,1])
Historic_assess_Bird_mt <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Habitat_Multiple[,1])
summary(Historic_assess_Bird_sg$category)
summary(Historic_assess_Bird_mt$category)
# Model:
model_bird_sg <- Run_Markov(Historic_assess_Bird_sg, Q)
model_bird_mt <- Run_Markov(Historic_assess_Bird_mt, Q)
# Bootstrap:
Boot_models_sg <- boot.msm(model_bird_sg, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_mt <- boot.msm(model_bird_mt, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_sg[!sapply(Boot_models_sg, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_sg_bird.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_mt[!sapply(Boot_models_mt, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_mt_bird.csv", row.names = FALSE)

# Group birds into (1) only inhabit 2 category of habitat, (2) inhabitat 3 or more categories of habitat:
Bird_Habitat_Single2 <- Habitat_Types_Bird%>%filter(Habitat_Types_Bird[,2]==1)
Bird_Habitat_Multiple2 <- Habitat_Types_Bird%>%filter(Habitat_Types_Bird[,2]>1)
# Group assessments:
Historic_assess_Bird_sg2 <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Habitat_Single[,1])
Historic_assess_Bird_mt2 <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Habitat_Multiple[,1])
summary(Historic_assess_Bird_sg2$category)
summary(Historic_assess_Bird_mt2$category)
# Model:
model_bird_sg2 <- Run_Markov(Historic_assess_Bird_sg2, Q)
model_bird_mt2 <- Run_Markov(Historic_assess_Bird_mt2, Q)
# Bootstrap:
Boot_models_sg2 <- boot.msm(model_bird_sg2, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_mt2 <- boot.msm(model_bird_mt2, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_sg2[!sapply(Boot_models_sg2, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_sg2_bird.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_mt2[!sapply(Boot_models_mt2, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_mt2_bird.csv", row.names = FALSE)

############################################
# HABITAT TYPE

# Group species into (1) live only in forests, (2) never live in forests:
# Mammals:
Mammal_Habitat_Forest <- numeric()
Mammal_Habitat_Nonforest <- numeric()
for (i in (1:length(All_Mammals_Habitat))) {
  if (length(All_Mammals_Habitat[[i]][[2]])>0) {
    for (ii in (1:length(All_Mammals_Habitat[[i]][[2]]))) {
      if (All_Mammals_Habitat[[i]][[2]][[ii]]$code<2) {
        Mammal_Habitat_Forest <- c(Mammal_Habitat_Forest, All_Mammals_Habitat[[i]][[1]][[1]])
      } else {
        Mammal_Habitat_Nonforest <- c(Mammal_Habitat_Nonforest, All_Mammals_Habitat[[i]][[1]][[1]])
      }
    }
  }
}
Mammal_Habitat_Forest <- unique(Mammal_Habitat_Forest)
Mammal_Habitat_Nonforest <- unique(Mammal_Habitat_Nonforest)
Mammal_Habitat_both <- intersect(Mammal_Habitat_Forest, Mammal_Habitat_Nonforest)
Mammal_Habitat_Forest <- setdiff(Mammal_Habitat_Forest, Mammal_Habitat_both) # Mammals that live only in forest habitats
Mammal_Habitat_Nonforest <- setdiff(Mammal_Habitat_Nonforest, Mammal_Habitat_both) # Mammals that live only in non-forest habitats
# Find forest specialized species:
Mammal_NF_Specialist <- intersect(Mammal_Habitat_Nonforest, Mammal_Habitat_Single[,1])
# Group assessments:
Historic_assess_Mammal_f <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_Habitat_Forest)
Historic_assess_Mammal_NFS <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Mammal_NF_Specialist)
summary(Historic_assess_Mammal_f$category)
summary(Historic_assess_Mammal_NFS$category)
model_mammal_f <- Run_Markov(Historic_assess_Mammal_f, Q)
model_mammal_nfs <- Run_Markov(Historic_assess_Mammal_NFS, Q)
# Bootstrap:
Boot_models_f <- boot.msm(model_mammal_f, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_nfs <- boot.msm(model_mammal_nfs, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_f[!sapply(Boot_models_f, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_forest_mammal.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_nfs[!sapply(Boot_models_nfs, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_nonforest_mammal.csv", row.names = FALSE)

# Birds:
Bird_Habitat_Forest <- numeric()
Bird_Habitat_Nonforest <- numeric()
for (i in (1:length(All_Birds_Habitat))) {
  if (length(All_Birds_Habitat[[i]][[2]])>0) {
    for (ii in (1:length(All_Birds_Habitat[[i]][[2]]))) {
      if (All_Birds_Habitat[[i]][[2]][[ii]]$code<2) {
        Bird_Habitat_Forest <- c(Bird_Habitat_Forest, All_Birds_Habitat[[i]][[1]][[1]])
      } else {
        Bird_Habitat_Nonforest <- c(Bird_Habitat_Nonforest, All_Birds_Habitat[[i]][[1]][[1]])
      }
    }
  }
}
Bird_Habitat_Forest <- unique(Bird_Habitat_Forest)
Bird_Habitat_Nonforest <- unique(Bird_Habitat_Nonforest)
Bird_Habitat_both <- intersect(Bird_Habitat_Forest, Bird_Habitat_Nonforest)
Bird_Habitat_Forest <- setdiff(Bird_Habitat_Forest, Bird_Habitat_both) # Birds that live only in forest habitats
Bird_Habitat_Nonforest <- setdiff(Bird_Habitat_Nonforest, Bird_Habitat_both) # Birds that live only in non-forest habitats
# Find forest specialized species:
Bird_NF_Specialist <- intersect(Bird_Habitat_Nonforest, Bird_Habitat_Single[,1])
# Group assessments:
Historic_assess_Bird_f <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_Habitat_Forest)
Historic_assess_Bird_NFS <- Corrected_cats%>%filter(Corrected_cats$taxonid%in%Bird_NF_Specialist)
summary(Historic_assess_Bird_f$category)
summary(Historic_assess_Bird_NFS$category)
# Model:
model_bird_f <- Run_Markov(Historic_assess_Bird_f, Q)
model_bird_nfs <- Run_Markov(Historic_assess_Bird_NFS, Q)
# Bootstrap:
Boot_models_f <- boot.msm(model_bird_f, stat = NULL, B=100, cores = (detectCores()-1))
Boot_models_nfs <- boot.msm(model_bird_nfs, stat = NULL, B=100, cores = (detectCores()-1))
# If not all converge:
boot.list2 <- Boot_models_f[!sapply(Boot_models_f, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_forest_bird.csv", row.names = FALSE)
# Repeat the process in the generalist group:
boot.list2 <- Boot_models_nfs[!sapply(Boot_models_nfs, function(x) class(x) == "try-error")]
Boot_probs <-lapply(boot.list2, Extract_probs, years = years)
Boot_probs <- bind_rows(Boot_probs, .id = "column_label")
# Save the file:
write.csv(Boot_probs, file = "Boots_nonforest_bird.csv", row.names = FALSE)


########################################
### U TESTS ###
####################

# Read in files:
BootsData1 <- read.csv("boots/Boots_light_mammal_med.csv")
BootsData2 <- read.csv("boots/Boots_heavy_mammal_med.csv")
#BootsData1 <- read.csv("boots/Boots_light_mammal.csv")
#BootsData2 <- read.csv("boots/Boots_heavy_mammal.csv")
#BootsData1 <- read.csv("boots/Boots_light_bird80.csv")
#BootsData2 <- read.csv("boots/Boots_heavy_bird80.csv")
#BootsData1 <- read.csv("boots/Boots_1_bird.csv")
#BootsData2 <- read.csv("boots/Boots_2_bird.csv")
#BootsData1 <- read.csv("boots/Boots_3_bird.csv")
#BootsData1 <- read.csv("boots/Boots_gn_mammal.csv")
#BootsData2 <- read.csv("boots/Boots_sp_mammal.csv")
#BootsData1 <- read.csv("boots/Boots_gn2_mammal.csv")
#BootsData2 <- read.csv("boots/Boots_sp2_mammal.csv")
#BootsData1 <- read.csv("boots/Boots_gn_bird.csv")
#BootsData2 <- read.csv("boots/Boots_sp_bird.csv")
#BootsData1 <- read.csv("boots/Boots_gn2_bird.csv")
#BootsData2 <- read.csv("boots/Boots_sp2_bird.csv")
#BootsData1 <- read.csv("boots/Boots_forest_mammal.csv")
#BootsData2 <- read.csv("boots/Boots_nonforest_mammal.csv")
#BootsData1 <- read.csv("boots/Boots_forest_bird.csv")
#BootsData2 <- read.csv("boots/Boots_nonforest_bird.csv")

Boots100_1 <- BootsData1[,(1:7)]%>%filter(BootsData1$Time==100)%>%mutate(group="light")
Boots100_2 <- BootsData2[,(1:7)]%>%filter(BootsData2$Time==100)%>%mutate(group="heavy")
test <- rbind(Boots100_1, Boots100_2)
wilcox.test(LC ~ group, data = test)
wilcox.test(NT ~ group, data = test)
wilcox.test(VU ~ group, data = test)
wilcox.test(EN ~ group, data = test)
wilcox.test(CR ~ group, data = test)
# Repeat the test in every pair!


##################################
### PLOTS ###
########################

# BODY MASS

# Define a function that prepare the dataframe for plotting:
Pre_plot <- function(Boot_probs) {
  Boot_means <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, mean)
  Boot_top <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
  Boot_bottom <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))
  # Bind them together into one df for graphing
  Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
  Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
  Boot_output <- Boot_output[,1:7]
  # Convert to long format
  Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
  Boot_output <- spread(Boot_output, key = "Type", value = "Probability")
  # Relevel so that the categories are in the right order
  Boot_output$Threat_level <- factor(Boot_output$Threat_level, levels =  c("LC", "NT", "VU", "EN", "CR"))
  #last 100 years only
  Boot_output <- Boot_output[which(Boot_output$Time == 100),]
  return(Boot_output)
}

# MAMMALS
# PE(h)/PE(l) changes with threshold:
# Read in data:
PE_threshold <- read.csv("boots/threshold_mammal.csv")
# Prepare the dataframe for plotting:
PE_threshold_fig <- gather(PE_threshold, key = "Category", value = "Ratio", LC, NT, VU, EN, CR)
# Plot:
ggplot(PE_threshold_fig, aes(x=threshold, y=Ratio, colour=Category)) + 
  geom_line(size=1) +
  geom_point() +
  ylab("PE(h)/PE(l) (t=100)") + xlab("Threshold(g)") +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.line = element_line(size=1, colour = "black", arrow = arrow(length = unit(0.5, 'cm'))),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))  +
  geom_hline(aes(yintercept=1), colour="red", size=1.5) +
  geom_vline(aes(xintercept=91.27), colour="grey", size=1, linetype="dashed")
# Threshold = median in mammals:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_light_mammal_med.csv")
Boot_probs2 <- read.csv("boots/Boots_heavy_mammal_med.csv")
# Prepare the df for plotting:
lightmammals_bootM <- Pre_plot(Boot_probs1)
heavymammals_bootM <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(lightmammals_bootM, heavymammals_bootM)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Light", "Heavy"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.3) + scale_fill_manual(values = c("cyan3", "tomato3"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(title = "threshold = 91.27 g", y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p
# Barplot when threshold = 3000 g:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_light_mammal.csv")
Boot_probs2 <- read.csv("boots/Boots_heavy_mammal.csv")
# Prepare the df for plotting:
lightmammals_boot <- Pre_plot(Boot_probs1)
heavymammals_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(lightmammals_boot, heavymammals_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Light", "Heavy"))
# Plot
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.3) + scale_fill_manual(values = c("cyan3", "tomato3"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(title = "threshold = 3.0 kg", y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# BIRDS
# Read in data:
PE_threshold <- read.csv("boots/threshold_bird.csv")
# Prepare the dataframe for plotting:
PE_threshold_fig <- gather(PE_threshold, key = "Category", value = "Ratio", LC, NT, VU, EN, CR)
# Plot:
ggplot(PE_threshold_fig, aes(x=threshold, y=Ratio, colour=Category)) + 
  geom_line(size=1) +
  geom_point() +
  ylab("PE(h)/PE(l) (t=100)") + xlab("Threshold(g)") +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.line = element_line(size=1, colour = "black", arrow = arrow(length = unit(0.5, 'cm'))),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))  +
  geom_hline(aes(yintercept=1), colour="red", size=1.5) +
  geom_vline(aes(xintercept=38.1), colour="grey", size=1, linetype="dashed")
# Barplot when threshold = 80 g:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_light_bird80.csv")
Boot_probs2 <- read.csv("boots/Boots_heavy_bird80.csv")
# Prepare the df for plotting:
lightbirds_boot <- Pre_plot(Boot_probs1)
heavybirds_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(lightbirds_boot, heavybirds_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Light", "Heavy"))
# Plot
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.3) + scale_fill_manual(values = c("cyan3", "tomato3"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(title = "threshold = 80 g", y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p
# Barplot for trisection:
# Read in files:
bird1_boot <- read.csv("boots/Boots_1_bird.csv")
bird2_boot <- read.csv("boots/Boots_2_bird.csv")
bird3_boot <- read.csv("boots/Boots_3_bird.csv")
# Join the three datasets:
Boot_bind <- rbind(bird1_boot, bird2_boot, bird3_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Light", "Middle" ,"Heavy"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.45) + scale_fill_manual(values = c("#F3E2A9", "#F7D358", "#FF8000"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

##################################################
# HABITAT BREADTH

# Habitat generalists vs specialists in mammals:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_gn_mammal.csv")
Boot_probs2 <- read.csv("boots/Boots_sp_mammal.csv")
# Prepare the df for plotting:
gnmammals_boot <- Pre_plot(Boot_probs1)
spmammals_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(gnmammals_boot, spmammals_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Generalist", "Specialist"))
# Plot
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.35) + scale_fill_manual(values = c("#5858FA", "#F78181"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# Habitat generalists vs specialists in birds:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_gn_bird.csv")
Boot_probs2 <- read.csv("boots/Boots_sp_bird.csv")
# Prepare the df for plotting:
gnbirds_boot <- Pre_plot(Boot_probs1)
spbirds_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(gnbirds_boot, spbirds_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Generalist", "Specialist"))
# Polt:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.35) + scale_fill_manual(values = c("#5858FA", "#F78181"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# Habitat generalists vs specialists (inhabit 2 types of habitat) in mammals:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_gn2_mammal.csv")
Boot_probs2 <- read.csv("boots/Boots_sp2_mammal.csv")
# Prepare the df for plotting:
gn2mammals_boot <- Pre_plot(Boot_probs1)
sp2mammals_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(gn2mammals_boot, sp2mammals_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Generalist", "Specialist"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.35) + scale_fill_manual(values = c("#5858FA", "#F78181"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# Habitat generalists vs specialists (inhabit 2 types of habitat) in birds:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_gn2_bird.csv")
Boot_probs2 <- read.csv("boots/Boots_sp2_bird.csv")
# Prepare the df for plotting:
gn2birds_boot <- Pre_plot(Boot_probs1)
sp2birds_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(gn2birds_boot, sp2birds_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Generalist", "Specialist"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.35) + scale_fill_manual(values = c("#5858FA", "#F78181"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

###################################################
# FOREST HABITAT

# Forest specialists vs non-forest specialists in mammals:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_forest_mammal.csv.csv")
Boot_probs2 <- read.csv("boots/Boots_nonforest_mammal.csv")
# Prepare the df for plotting:
fmammals_boot <- Pre_plot(Boot_probs1)
nfmammals_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(fmammals_boot, nfmammals_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Forest", "Non-forest"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.75) + scale_fill_manual(values = c("#0B6121", "#DBA901"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# Forest specialists vs non-forest specialists in birds:
# Read in data:
Boot_probs1 <- read.csv("boots/Boots_forest_bird.csv")
Boot_probs2 <- read.csv("boots/Boots_nonforest_bird.csv")
# Prepare the df for plotting:
fbirds_boot <- Pre_plot(Boot_probs1)
nfbirds_boot <- Pre_plot(Boot_probs2)
# Join the two datasets:
Boot_bind <- rbind(fbirds_boot, nfbirds_boot)
Boot_bind["Group"] <- factor(Boot_bind$Group, levels = c("Forest", "Non-forest"))
# Plot:
p <- ggplot(data = Boot_bind, aes(x = Threat_level, y = Mean, xmax=100, fill = Group)) + ylim(0,0.75) + scale_fill_manual(values = c("#0B6121", "#DBA901"))
p <- p + geom_bar(stat="identity", position = "dodge") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2, position=position_dodge(.9)) + scale_x_discrete(labels= c("LC", "NT", "VU", "EN", "CR"))
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p