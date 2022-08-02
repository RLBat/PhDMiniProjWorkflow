# Author: Rachel Bates (r.bates18@imperial.ac.uk), Yuheng Sun (yuheng.sun20@imperial.ac.uk)
# Date: 

###########################
#### Read in files ######

# Read in the table 7 data from 07-19
Table7 <- read.csv("My_Workflow/Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

###########################

##### DATA PROCESSING #####

# Read in data
Species_History <- Species_History_Bird
Species_Data <- Species_Data_All

source("My_Workflow/Code/Data_Processing.R")

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

# Reassign false tags where possible:
Corrected_cats <- Reassign_Cats(Species_History_Tags)

# Final cleaning steps:
Corrected_cats <- Final_clean(Corrected_cats)
length(unique(Corrected_cats$taxonid)) # 9953 species are used in the model.

# Tag heavy and light animals:
Corrected_cats <- Corrected_cats%>%mutate(Group=as.character(NA))
med <- median(Bodymass_Bird_Recode$Mass)
for (i in (1:nrow(Corrected_cats))) {
  temp <- Bodymass_Bird_Recode%>%filter(Bodymass_Bird_Recode$IUCN_ID==Corrected_cats$taxonid[i])
  if (temp$Mass>600) {
    Corrected_cats$Group[i] <- "heavy"
  } else {
    Corrected_cats$Group[i] <- "light"
  }
}
length(unique((Corrected_cats%>%filter(Group=="light"))$taxonid)) # 3555 light species
length(unique((Corrected_cats%>%filter(Group=="heavy"))$taxonid)) # 882 heavy species

write.csv(Corrected_cats, "My_Workflow/Corrected_SpeciesHistory_deextinct_BBM_med.csv", row.names = FALSE)

######### MODELLING #########

source("My_Workflow/Code/Markov_Modelling.R")

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

### Overall model
Historic_assess <- Corrected_cats
Historic_assess_l <- Historic_assess%>%filter(Historic_assess$Group=="light")
Historic_assess_h <- Historic_assess%>%filter(Historic_assess$Group=="heavy")
msm_model_l <- Run_Markov(Historic_assess_l, Q)
msm_model_l
msm_model_h <- Run_Markov(Historic_assess_h, Q)
msm_model_h

# Plot
plot.msm(msm_model_l, legend.pos = c(0, 0.7), range = c(0, 100))
pmatrix.msm(msm_model_l, t=100)
pmatrix.msm(msm_model_h, t=100)
