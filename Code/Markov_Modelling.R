
## Import packages
require(msm)
require(markovchain)
require(dplyr)
require(diagram)
require(ggplot2)
require(xtable)

## Import data
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

##########

Transition_intensity_matrix <- function(Categories){
  # Build transition intensity matrix
  # This defines that they have to pass through all states to reach death
  # Slightly increases extinction chance but not much
  Q <- rbind(c(0.5, 0.5, 0, 0, 0, 0),
             c(0.25, 0.5, 0.25, 0, 0, 0),
             c(0, 0.25, 0.5, 0.25, 0, 0),
             c(0, 0, 0.25, 0.5, 0.25, 0),
             c(0, 0, 0, 0.25, 0.5, 0.25),
             c(0, 0, 0, 0, 0, 1))
  row.names(Q) <- Categories
  colnames(Q) <- Categories
  return(Q)
}

Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

# Plot the state changes
plotmat(Q, pos = c(1,1,1,1,1,1), relsize = 0.87, box.type = "round", box.size = 0.04, box.cex = 2, arr.lwd = 2, self.cex = 1, self.arrpos = 1.5,
        box.col = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"), cex = 0.00000001, shadow.size = 0, arr.type = "simple",
        arr.length = 0.3,  self.shifty = 0, self.shiftx = 0.1, endhead = FALSE, self.lwd = 2)


Run_Markov <- function(Historic_assess, Q){
  # Reverse year order for the state table
  Historic_assess<-arrange(Historic_assess, taxonid, year)
  
  # Change cats to numbers for modelling
  Historic_assess$category <- recode(Historic_assess$category, "LC" = "1", "NT" = "2", "VU" = "3", "EN"= "4", "CR" = "5", "EX" = "6")
  Historic_assess$category <- as.integer(Historic_assess$category)
  
  # Create state table
  state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)
  
  # Add a Time since first assessment column (TSFA)
  Historic_assess <- Historic_assess %>% group_by(taxonid) %>% mutate(TSFA = year-min(year))
  
  # Actual model!
  msm_model <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q, gen.inits = TRUE, control=list(fnscale=60000,maxit=500))
  
  return(msm_model)
}

Taxa <- c("Plant", "Invertebrate", "Bird", "Mammal", "Herptile", "Fish")
# Run models for each taxonomic group.
Taxon_msm <- lapply(Taxa, function(i) Run_Markov(subset(Historic_assess, Taxon == i), Q))
names(Taxon_msm) <- Taxa

Threats <-  c("HabitatChange", "Overexploitation", "Invasives", "Pollution", "ClimateChange", "Natural")
Threat_msm <- lapply(Threats, function(i) Run_Markov(subset(Historic_assess, taxonid %in% Threat_index[[i]]), Q))
names(Threat_msm) <- Threats

Build_Ex_prob_table <- function(msm_model, time=500){
  Model_ex_prob <- data.frame(matrix(ncol=6, nrow=0))
  names(Model_ex_prob) <- c("LC", "NT", "VU", "EN", "CR", "EX")
  for (i in 1:length(msm_model)){
    Model_ex_prob[names(msm_model)[i],] <- pmatrix.msm(msm_model[[i]], time)[,"EX"]
  }
  Model_ex_prob[,"EX"] <- NULL
  return(xtable(Model_ex_prob))
}

Threat_500 <- Build_Ex_prob_table(Threat_msm)

# Create state table
# state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)
# Reorder correctly
# Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")  
# state_table <- state_table[Categories,Categories]
# # Make probabilistic
# state_table_prob <- apply(state_table, 1, function(i) round(i/sum(i), 3))


# Plot the model
plot(Taxon_msm$Invertebrate, range = c(0, 500), legend.pos = c(0.1, 0.5))

pmatrix.msm(Taxon_msm$Invertebrate, t=100)[,"EX"]



