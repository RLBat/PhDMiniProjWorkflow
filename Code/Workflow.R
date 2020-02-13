# Author: Rachel Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###### DATA COLLECTION #####

source("Data_Collection.R")

# Set API Key
API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b"

# Collect all species in the Red List and their base information
Species_Info_Collect()

# Collect historical assessment data for each species
Species_History_Collect()

# Collect various metadata including criteria and date of most recent assessment and habitat information.
Species_Meta_Collect()

##### DATA PROCESSING #####

source("Data_Processing.R")

# Does initial data cleaning, removing invalid or unusable assessments
Assess_Clean()
