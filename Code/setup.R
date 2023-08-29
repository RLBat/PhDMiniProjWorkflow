setwd("Documents/PhDMiniProjWorkflow/Code/")
set.seed(333)

source("Bootstrapping.R")
source("RLI2.R")

Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)
Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", stringsAsFactors = T)
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))
cats <- c("LC","NT","VU", "EN","CR", "EX")
