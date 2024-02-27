setwd("Documents/PhDMiniProjWorkflow/Code/")
set.seed(333)

source("Bootstrapping.R")
source("RLI2.R")


Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_Aug2023.csv", stringsAsFactors = T)
Species_History <- Corrected_cats

Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)
#Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", stringsAsFactors = T)
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))
cats <- c("LC","NT","VU", "EN","CR", "EX")
Boot_output <- read.csv("../Data/Overallbootoutput_Aug23.csv", stringsAsFactors = FALSE, header = T)


