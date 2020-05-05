
## Import packages
require(msm)
require(markovchain)
require(dplyr)
require(diagram)
require(ggplot2)

## Import data
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

##########

State_Change_Plot <- function(Q){
  # Plot the state changes
  plotmat(Q, pos = c(2,2,2), relsize = 0.87, box.type = "round", box.size = 0.04, box.cex = 2, arr.lwd = 0.5, self.cex = 2,
          box.col = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"), cex = 0.00000001, shadow.size = 0,
          self.shifty = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05), self.shiftx = 0, )
}

# Reverse year order for the state table
Historic_assess<-arrange(Historic_assess, taxonid, year)
# Make a probability based state change table
# Create state table
state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)
# Reorder correctly
Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")  
state_table <- state_table[Categories,Categories]
# Make probabilistic
state_table_prob <- apply(state_table, 1, function(i) round(i/sum(i), 3))

# Change cats to numbers for modelling
Historic_assess$category <- recode(Historic_assess$category, "LC" = "1", "NT" = "2", "VU" = "3", "EN"= "4", "CR" = "5", "EX" = "6")
Historic_assess$category <- as.integer(Historic_assess$category)

# Create state table
state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)

# Build transition intensity matrix
# This defines that they have to pass through all states to reach death
# Slightly increases extinction chance but not much
Q <- rbind(c(0.5, 0.5, 0, 0, 0, 0),
           c(0.25, 0.5, 0.25, 0, 0, 0),
           c(0, 0.25, 0.5, 0.25, 0, 0),
           c(0, 0, 0.25, 0.5, 0.25, 0),
           c(0, 0, 0, 0.25, 0.5, 0.25),
           c(0, 0, 0, 0, 0, 1))
row.names(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")

# Add a Time since first assessment column (TSFA)
Historic_assess <- Historic_assess %>% group_by(taxonid) %>% mutate(TSFA = year-min(year))

# Actual model!
changes.msm <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q, gen.inits = TRUE, control=list(fnscale=60000,maxit=500))

# Plot the model
plot(changes.msm, range = c(0, 500), legend.pos = c(0.1, 0.5))

pmatrix.msm(changes.msm, t=100)



