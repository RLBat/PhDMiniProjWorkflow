# Author: Rachel Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###########################
#### Read in files ######

# Read in the table 7 data from 07-19
Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

set.seed(333)

###########################

###### DATA COLLECTION #####

# source("Data_Collection.R")
# 
# # Set API Key
# API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b" #use your personal key here
# 
# # Collect all species in the Red List and their base information
# Species_Data <- Species_Info_Collect(API_key = API_key)
# 
# # Collect historical assessment data for each species
# Species_History<-Species_History_Collect()

### OR SKIP AND USE THE FOLLOWING ##

Species_History <- read.csv("../Data/Species_History_IDs_20223.csv", stringsAsFactors = T)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)

#########################

##### DATA PROCESSING #####

source("Data_Processing.R")

# Does initial data cleaning, removing invalid or unusable assessments
Species_History <- Assess_Clean(Species_History)

# Cleans up table 7 data for use
Cat_Changes <- Add_table7_tags(Table7, Species_History)

# Assigns any known tags
##### Takes a long time!!! #######
Species_History <- Assign_known_tags(Cat_Changes, Species_History)

# Works out the probabilities of an assessment being gen or non-gen
Cat_probs <- Define_probabilities(Species_History)

###############

### If running once, takes a good while
Species_History_Tags <- Generate_tags(Species_History, Cat_probs)

# Final cleaning steps
Corrected_cats <- Final_clean(Species_History_Tags)

#######################

######### MODELLING #########

source("Bootstrapping.R")

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

### Run the bootstrapped model
Boot_Probs <- Run_bootmarkov(Historic_assess = Corrected_cats, Q)

######### PLOTTING ##########

cats <- c("LC","NT","VU", "EN","CR", "EX")
Boot_means <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, mean)
Boot_top <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
Boot_bottom <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))

# Bind them together into one df for graphing
Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
Boot_output <- Boot_output[,1:7]
# Convert to long format
Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
Boot_output <- spread(Boot_output, key = "Type", value = "Probability")

Boot_output$Threat_level <- factor(Boot_output$Threat_level, levels = c("CR", "EN", "VU", "NT", "LC"))

######### Graphing ##############

p <- ggplot(data = Boot_output, aes(x = Time, y = Mean, colour = Threat_level, xmax = 100)) + scale_color_manual(values = c("darkred", "orangered3", "darkorange", "orange", "darkcyan", "lightblue"))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,1,0.1))
p <- p + geom_ribbon(aes(ymin=Bottom, ymax=Top, alpha=0.5),fill="lightgrey", linetype = 2, show.legend = FALSE)
p <- p + labs(y = "Probability of extinction", x= "Years", colour = "Threat Level") 
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p


###########################

########### SPECIES ATTRIBUTES  ############

#### BODY MASS ######

source("Body_Mass_Processing.R")

## MAMMALS ##

# Grab the species with enough data to model post-cleaning
Final_Species_List <- Historic_assess$scientific_name
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

## BIRDS ##

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



