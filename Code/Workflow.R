# Author: Rachel Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###########################
#### Read in files ######

# Read in the table 7 data from 07-19
Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

### If not done data collection, use files from 03-2020
# Species_History <- read.csv("../Data/Species_History.csv", stringsAsFactors = T)
# Species_Data <- read.csv("../Data/Species_Data.csv", stringsAsFactors = F)

###########################

###### DATA COLLECTION #####

source("Data_Collection.R")

# Set API Key
API_key = #use your personal key here

# Collect all species in the Red List and their base information
Species_Data<-Species_Info_Collect()

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

# Reassign false tags where possible
Corrected_cats <- Reassign_Cats(Species_History_Tags)

# Final cleaning steps
Corrected_cats <- Final_clean(Corrected_cats)

################
##### If running multiple
### To create files with the genuine tagging randomised
### TAKES HOURS

source("HPC_Random_gen.R")

#######################

######### MODELLING #########

source("Markov_Modelling.R")

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

### Overall model
msm_model <- Run_Markov(Historic_assess, Q)


