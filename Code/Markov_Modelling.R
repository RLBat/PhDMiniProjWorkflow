
## Import packages
require(msm)
#require(markovchain)
require(dplyr)
#require(diagram)
require(ggplot2)
#require(xtable)
#require(splitstackshape)
require(tidyverse)

`%!in%` = Negate(`%in%`)

############################

## Import data
#Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

################################

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

# Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

# Plot the state changes
# plotmat(Q, pos = c(1,1,1,1,1,1), relsize = 0.87, box.type = "round", box.size = 0.04, box.cex = 2, arr.lwd = 2, self.cex = 1, self.arrpos = 1.5,
#         box.col = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"), cex = 0.00000001, shadow.size = 0, arr.type = "simple",
#         arr.length = 0.3,  self.shifty = 0, self.shiftx = 0.1, endhead = FALSE, self.lwd = 2)


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
  msm_model <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q, gen.inits = TRUE, control=list(fnscale=60000,maxit=500))# covariates = ~ gen_count)
  
  return(msm_model)
}

########################################

