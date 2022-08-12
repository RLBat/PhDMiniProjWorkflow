# Author: Rachel Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###########################
#### Read in files ######

# Read in the table 7 data from 07-19
Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

### If not done data collection, use files from 03-2020
Species_History <- read.csv("../Data/Species_History_IDs_20223.csv", stringsAsFactors = T)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)

set.seed(333)

###########################

###### DATA COLLECTION #####

source("Data_Collection.R")

# Set API Key
API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b" #use your personal key here

# Collect all species in the Red List and their base information
Species_Data <- Species_Info_Collect(API_key = API_key)

# Collect historical assessment data for each species
Species_History<-Species_History_Collect()

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

################
##### If running multiple
### To create files with the genuine tagging randomised
### TAKES HOURS

#source("HPC_Random_gen.R")

#######################

######### MODELLING #########

source("Bootstrapping.R")

Historic_assess <- Corrected_cats

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

### Overall model
msm_model <- Run_Markov(Historic_assess, Q)

## Bootstrap the model
Boot_models <- Bootstrap_msm(msm_model, repeats = 100)

### If not all converge
Boot_models <- Boot_models[!sapply(Boot_models, function(x) class(x) == "try-error")]

# Extract the probabilities to save or graph
Boot_Probs <- Boot_probs(Boot_models = Boot_models)


###########################

########### SPECIES ATTRIBUTES  ############

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








