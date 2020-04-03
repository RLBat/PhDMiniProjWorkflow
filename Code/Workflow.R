# Author: Rachel Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###### DATA COLLECTION #####

source("Data_Collection.R")

# Set API Key
API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b"

# Collect all species in the Red List and their base information
Species_Data<-Species_Info_Collect()

# Collect historical assessment data for each species
Species_History<-Species_History_Collect()

##### DATA PROCESSING #####

source("Data_Processing.R")

# Does initial data cleaning, removing invalid or unusable assessments
Species_History<-Assess_Clean()

# Read in the table 7 data from 07-19
Table7 <- read.csv("../Data/Table7.csv", header=TRUE)

# Assigns every assessment a genuine or non-genuine tag
Fix_Nongen_Assess()

##### ADDITIONAL DATA COLLECTION #####

# This additional data collection step post-processing should be 
# faster due to species being removed than if it was done pre-processing

# Collect various metadata including criteria and date of most recent assessment and habitat information.
Species_Meta_Collect()

# Collect threat data on each species
Species_Threat_Collect()

##### MODELLING #####

