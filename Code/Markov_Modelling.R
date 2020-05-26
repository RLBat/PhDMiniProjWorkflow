
## Import packages
require(msm)
require(markovchain)
require(dplyr)
require(diagram)
require(ggplot2)
require(xtable)
require(splitstackshape)
require(tidyverse)

`%!in%` = Negate(`%in%`)

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

### Overall model

msm_model <- Run_Markov(Historic_assess, Q)

### Taxonomic modelling

Taxa <- c("Plant", "Invertebrate", "Bird", "Mammal", "Herptile", "Fish")
# Run models for each taxonomic group.
Taxon_msm <- lapply(Taxa, function(i) Run_Markov(subset(Historic_assess, Taxon == i), Q))
names(Taxon_msm) <- Taxa

### Threat based modelling

# Only some threat cats
Threats <- names(Threat_index)
Threats_gen <- sort(names(Threat_index_generic))

## Add an index of those that don't appear at all in the threat doc! (About half of them!)
All_threatened <- c()

for (i in 1:length(Threat_index)){
  All_threatened <- c(All_threatened, Threat_index[[i]])
}
All_threatened <- unique(All_threatened)

#No_threat <- unique(Historic_assess$taxonid[Historic_assess$taxonid %!in% All_threatened])
Historic_assess_threat <- subset(Historic_assess, taxonid %in% All_threatened)

Threat_msm <- lapply(Threats, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %in% Threat_index[[i]]), Q))
Threat_opp_msm <- lapply(Threats, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %!in% Threat_index[[i]]), Q))
names(Threat_msm) <- Threats
names(Threat_opp_msm) <- paste("Not_", Threats, sep="")

#  All 45 generic threats
Threat_gen_msm <- lapply(Threats_gen, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %in% Threat_index_generic[[i]]), Q))
Threat_gen_opp_msm <- lapply(Threats_gen, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %!in% Threat_index_generic[[i]]), Q))
names(Threat_gen_msm) <- Threats_gen
names(Threat_gen_opp_msm) <- paste("Not_", Threats_gen, sep="")

Build_Ex_prob_table <- function(msm_model, time=500){
  Model_ex_prob <- data.frame(matrix(ncol=6, nrow=0))
  names(Model_ex_prob) <- c("LC", "NT", "VU", "EN", "CR", "EX")
  for (i in 1:length(msm_model)){
    Model_ex_prob[names(msm_model)[i],] <- pmatrix.msm(msm_model[[i]], time)[,"EX"]
  }
  Model_ex_prob[,"EX"] <- NULL
  return(Model_ex_prob)
}

Threat_500 <- Build_Ex_prob_table(Threat_msm)
Not_Threat_500 <- Build_Ex_prob_table(Threat_opp_msm)
Threat_both_500 <- Threat_500 - Not_Threat_500

Threat_500 <- Build_Ex_prob_table(Threat_gen_msm)
Not_Threat_500 <- Build_Ex_prob_table(Threat_gen_opp_msm)
Threat_both_500 <- Threat_500 - Not_Threat_500

Taxon_500 <- Build_Ex_prob_table(Taxon_msm, 500)



# Create state table
# state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)
# Reorder correctly
# Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")  
# state_table <- state_table[Categories,Categories]
# # Make probabilistic
# state_table_prob <- apply(state_table, 1, function(i) round(i/sum(i), 3))


# # Plot the model
# plot(Taxon_msm$Invertebrate, range = c(0, 500), legend.pos = c(0.1, 0.5))
# 
# pmatrix.msm(Taxon_msm$Invertebrate, t=100)[,"EX"]

###### Plotting the split models #######

Taxon_500$Taxa <- row.names(Taxon_500)
Taxa_df <- gather(Taxon_500, "Category", "Probability", -Taxa)
Taxa_size <- as.data.frame(table(Historic_assess$Taxon, Historic_assess$category))
colnames(Taxa_size) <- c("Taxa", "Category", "Assessments")
Taxa_size <- subset(Taxa_size, Taxa_size$Category!="EX")
Taxa_df <- merge(Taxa_df, Taxa_size)

# Order factor
Taxa_df$Category <- factor(Taxa_df$Category, levels = c("LC","NT","VU","EN","CR"))

# Bar chart panel graph
  p <- ggplot(data = Taxa_df, aes(x=Category, y=Probability, fill=Category))
  p <- p + geom_bar(stat="identity") + facet_wrap(~ Taxa) + scale_fill_manual(values = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"))
  p <- p + labs(y = "Probability of extinction (t=100)\n", x= "IUCN Category") 
  p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_blank(),
                 axis.text.y = element_text(size=16), axis.text.x = element_blank(), axis.title = element_text(size=20), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

# Taxa_df$Category <- recode(Taxa_df$Category, LC = 1, NT = 2, VU = 3, EN = 4, CR = 5)
Taxa_df <- arrange(Taxa_df, Taxa, Category)

## Calculate the relative width needed for each
Taxa_df <- Taxa_df %>% group_by(Taxa) %>% mutate(Percent = sum(Assessments)/100) %>% mutate(Proportional = Assessments/Percent) %>% mutate(Cum_prop = cumsum(Proportional)) %>% ungroup()
Taxa_df$Priorcumprop <- NA

for (i in 1:nrow(Taxa_df)){
  Taxa_df$Priorcumprop[i] <- ifelse(Taxa_df$Category[i]!=1, Taxa_df$Cum_prop[i-1], 0)
}

Taxa_df <- Taxa_df %>% group_by(Taxa) %>% mutate(mid_pos = Priorcumprop + (Proportional/2)) %>% ungroup()


#Taxa_df <- expandRows(Taxa_df, "Assessments")
# row.names(Taxa_df)<-NULL
# Taxa_df$Index <- row.names(Taxa_df)



p <- ggplot(data = Taxa_df, aes(x=mid_pos, y=Probability, colour = Taxa, xmin = 0, ymax=1), ylim = c(0,1))
p <- p + geom_point(aes(shape=Taxa)) + geom_line() + theme_bw()
p <- p + scale_x_continuous(breaks = seq(0,100,10)) + labs(y = "Probability of extinction at 500 years", x = "")
p <- p+ scale_color_viridis_d()
p
