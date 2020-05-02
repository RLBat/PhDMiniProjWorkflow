
## Import packages
require(msm)
require(markovchain)
require(dplyr)

## Import data
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

##########

# Reverse year order for the state table
Historic_assess<-arrange(Historic_assess, taxonid, year)

Historic_assess$category <- recode(Historic_assess$category, "LC" = "1", "NT" = "2", "VU" = "3", "EN"= "4", "CR" = "5", "EX" = "6")
Historic_assess$category <- as.integer(Historic_assess$category)

# Create state table
state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)

# From Elizabeth's code - define what changes are possible (no skipping states)
Q <- rbind(c(0, 0.25, 0, 0, 0, 0.25),
           c(0.166, 0, 0.166, 0, 0, 0.166),
           c(0, 0.166, 0, 0.166, 0, 0.166),
           c(0, 0, 0.166, 0, 0.166, 0.166),
           c(0, 0, 0, 0.25, 0, 0.25),
           c(0, 0, 0, 0, 0, 0))
row.names(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")

# Add a Time since first assessment column
Historic_assess <- Historic_assess %>% group_by(taxonid) %>% mutate(TSFA = year-min(year))

## More Elizabeth code "Calculates crude initial values for transition intensities"
Q.crude <- crudeinits.msm(Historic_assess$category ~ Historic_assess$TSFA, subject = Historic_assess$taxonid, qmatrix = Q)

# Actual model!
changes.msm <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q.crude)

# Plot the model
plot(changes.msm, range = c(0, 500), legend.pos = c(0.1, 0.5))
