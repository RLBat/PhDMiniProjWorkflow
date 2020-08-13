
## Import packages
require(msm)
#require(markovchain)
require(dplyr)
#require(diagram)
require(ggplot2)
require(xtable)
require(splitstackshape)
require(tidyverse)

`%!in%` = Negate(`%in%`)

## Import data
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

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
  msm_model <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q, gen.inits = TRUE, control=list(fnscale=60000,maxit=500), covariates = ~ gen_count)
  
  return(msm_model)
}

########################################

### Overall model

msm_model <- Run_Markov(Historic_assess, Q)

### Taxonomic modelling

Taxa <- c("Plant", "Invertebrate", "Bird", "Mammal", "Herptile", "Fish")
# Run models for each taxonomic group.
Taxon_msm <- lapply(Taxa, function(i) Run_Markov(subset(Historic_assess, Taxon == i), Q))
names(Taxon_msm) <- Taxa

### Threat based modelling

# Threat number modelling

Threat_num_model <- Run_Markov(Historic_assess, Q)

pmatrix.msm(Threat_num_model,100)

##################################

# Func to compare like with like!

#### Testing with mammals


Compare_group <- function(){
  # Tell it which group it is
  group <- subset(Historic_assess, Historic_assess$Taxon == "Mammal")
  not_group <- subset(Historic_assess, Historic_assess$Taxon != "Mammal")
  # Work out whether the group or not_group is smaller
  group_smaller <- length(unique(group$taxonid)) < length(unique(not_group$taxonid))
  # Work out the threat category spread of the smaller group
  if(group_smaller == TRUE){
    small_group <- group
    big_group <- not_group
  } else {
    small_group <- not_group
    big_group <- group
  }
  group_recent <- small_group %>% group_by(taxonid) %>% slice(which.max(year))
  cat_split <- table(group_recent$category)
  # Select a subset of bigger group of the same size and same category spread
  big_group_recent <- big_group %>% group_by(taxonid) %>% slice(which.max(year))
  for (i in 1:length(cat_split)){
    lapply()
    big_group_sample <- big_group_recent %>% filter(category == "NT")
  }
  
  
}


##################################

# Only some threat cats
Threats <- names(Threat_index)
Threats_gen <- sort(names(Threat_index_generic))

## Add an index of those that don't appear at all in the threat doc! (About half of them!)
All_threatened <- c()

for (i in 1:length(Threat_index_generic)){
  All_threatened <- c(All_threatened, Threat_index_generic[[i]])
}
All_threatened <- unique(All_threatened)

## Subset to only those species that had any threats recorded for them
Historic_assess_threat <- subset(Historic_assess, taxonid %in% All_threatened)


## Modelling for the smaller categories
Threat_msm <- lapply(Threats, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %in% Threat_index[[i]]), Q))
Threat_opp_msm <- lapply(Threats, function(i) Run_Markov(subset(Historic_assess_threat, taxonid %!in% Threat_index[[i]]), Q))
names(Threat_msm) <- Threats
names(Threat_opp_msm) <- paste("Not_", Threats, sep="")

#  All 12 top level threats modelling
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

Threat_gen_500 <- Build_Ex_prob_table(Threat_gen_msm, 100)
Not_Threat_gen_500 <- Build_Ex_prob_table(Threat_gen_opp_msm, 100)
Threat_both_gen_500 <- Threat_gen_500 - Not_Threat_gen_500

Taxon_500 <- Build_Ex_prob_table(Taxon_msm, 100)

######## PLOTTING #############

#### Overall ####

plot.msm(msm_model, range=c(0,100), legend.pos = c(0.1, 0.5), xlab = "Years", ylab = "Probability of Survival")

Plot.msm <- function (x, from = NULL, to = NULL, range = NULL, covariates = "mean", 
          legend.pos = NULL, xlab = "Time", ylab = "Fitted survival probability", 
          lwd = 1, ...) 
{
  if (!inherits(x, "msm")) 
    stop("expected x to be a msm model")
  if (is.null(from)) 
    from <- transient.msm(x)
  else {
    if (!is.numeric(from)) 
      stop("from must be numeric")
    if (any(!(from %in% 1:x$qmodel$nstates))) 
      stop("from must be a vector of states in 1, ..., ", 
           x$qmodel$nstates)
  }
  if (is.null(to)) {
    if (length(absorbing.msm(x)) == 0) 
      stop("\"to\" not specified, and no absorbing state in the model")
    to <- max(absorbing.msm(x))
  }
  else {
    if (!is.numeric(to)) 
      stop("to must be numeric")
    if (!(to %in% absorbing.msm(x))) 
      stop("to must be an absorbing state")
  }
  if (is.null(range)) 
    rg <- range(model.extract(x$data$mf, "time"))
  else {
    if (!is.numeric(range) || length(range) != 2) 
      stop("range must be a numeric vector of two elements")
    rg <- range
  }
  timediff <- (rg[2] - rg[1])/50
  times <- seq(rg[1], rg[2], timediff)
  pr <- numeric()
  cols <- c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")
  for (t in times) pr <- c(pr, pmatrix.msm(x, t, times[1], 
                                           covariates)[from[1], to])
  plot(times, pr, type = "l", xlab = xlab, ylab = ylab, 
       lwd = lwd, ylim = c(0, 0.3), lty = 1, col = cols[1], cex.lab = 1.5, cex.axis = 1.2, ...)
  lt <- 2
  for (st in from[-1]) {
    pr <- numeric()
    for (t in times) pr <- c(pr, pmatrix.msm(x, t, times[1], 
                                             covariates)[st, to])
    lines(times, pr, type = "l", lty = 1, lwd = lwd, 
          col = cols[lt], ...)
    lt <- lt + 1
  }
  abline(v=10, lty=5, col="grey")
  abline(v=50, lty = 5, col="grey")
  if (!is.numeric(legend.pos) || length(legend.pos) != 2) 
    legend.pos <- c(max(times) - 15 * timediff, 1)
  legend(legend.pos[1], legend.pos[2], legend = c("LC", "NT", "VU", "EN", "CR"), lty = 1, col = cols, lwd = lwd)

  invisible()
}

Plot.msm(msm_model, range = c(0,100), legend.pos = c(0.1, 0.3), xlab = "Years", ylab = "Probability of Extinction\n", lwd = 3)

###### Plotting the split models #######

##### Taxa #######

# Make a df of probability per category + no of assessments
Taxon_500$Taxa <- row.names(Taxon_500)
Taxa_df <- gather(Taxon_500, "Category", "Probability", -Taxa)
Taxa_size <- as.data.frame(table(Historic_assess$Taxon, Historic_assess$category))
colnames(Taxa_size) <- c("Taxa", "Category", "Assessments")
Taxa_size <- subset(Taxa_size, Taxa_size$Category!="EX") # Remove extinct assessments
Taxa_df <- merge(Taxa_df, Taxa_size)

# Order factor
Taxa_df$Category <- factor(Taxa_df$Category, levels = c("LC","NT","VU","EN","CR"))

### Bar chart panel graph
  p <- ggplot(data = Taxa_df, aes(x=Category, y=Probability, fill=Category))
  p <- p + geom_bar(stat="identity") + facet_wrap(~ Taxa) + scale_fill_manual(values = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"))
  p <- p + labs(y = "Probability of extinction (t=100)\n", x= "IUCN Category") 
  p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_blank(),
                 axis.text.y = element_text(size=16), axis.text.x = element_blank(), axis.title = element_text(size=20), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

##### Line graph

Taxa_df <- arrange(Taxa_df, Taxa, Category)


Taxa_df <- expandRows(Taxa_df, "Assessments")
Taxa_df$Rank <- NA
for ( i in 1:length(Taxa)){
  Taxa_df[which(Taxa_df$Taxa==Taxa[i]),]$Rank <- seq(1,nrow(Taxa_df[Taxa_df$Taxa==Taxa[i],]))/nrow(Taxa_df[Taxa_df$Taxa==Taxa[i],])
}

p <- ggplot(data = Taxa_df, aes(x=Rank, y=Probability, colour = Taxa, xmin = 0, ymax=0.8))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,1,0.1))
p <- p + scale_x_continuous(breaks = seq(0,1,0.10)) + labs(y = "Probability of extinction at 100 years\n", x = "\nCumulative Proportion of Assessments") 
p <- p+ scale_color_brewer(palette = "Set2")
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=16),  axis.title = element_text(size=20), legend.text = element_text(size=14), legend.title = element_text(size=14), strip.text = element_text(size=14))

p

## Calculate the relative width needed for each
# Taxa_df <- Taxa_df %>% group_by(Taxa) %>% mutate(Percent = sum(Assessments)/100) %>% mutate(Proportional = Assessments/Percent) %>% mutate(Cum_prop = cumsum(Proportional)) %>% ungroup()
# Taxa_df$Priorcumprop <- NA
# 
# for (i in 1:nrow(Taxa_df)){
#   Taxa_df$Priorcumprop[i] <- ifelse(Taxa_df$Category[i]!=1, Taxa_df$Cum_prop[i-1], 0)
# }
# 
# Taxa_df <- Taxa_df %>% group_by(Taxa) %>% mutate(mid_pos = Priorcumprop + (Proportional/2)) %>% ungroup()


####### Threat #########


Threat_both_gen_500$Threat <- row.names(Threat_both_gen_500)
Threat_df <- gather(Threat_both_gen_500, "Category", "Probability", -Threat)
Threat_df <- subset(Threat_df, Threat_df$Threat!="NA")
Threat_df$Threat <- as.numeric(Threat_df$Threat)
Threat_df$Category <- factor(Threat_df$Category, levels = c("LC","NT","VU","EN","CR"))
Threat_df$Threat <- factor(Threat_df$Threat, levels = 1:11)
Threat_df$Threat <- recode(Threat_df$Threat, "1" = "Development", "2" = "Agriculture and Aquaculture", "3" = "Energy production and Mining", "4" = "Transportation", "5" = "Biological Resource Use", "6" = "Human Disturbance", "7" = "Natural system modifications", "8" = "Invasives", "9" = "Pollution", "10" = "Geological Events", "11" = "Climate Change and Weather")#, "12" = "Other")


### Bar chart panel graph
p <- ggplot(data = Threat_df, aes(x=Category, y=Probability, fill=Category))
p <- p + geom_bar(stat="identity") + facet_wrap(~ Threat) + scale_fill_manual(values = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"))
p <- p + labs(y = "Probability of extinction (t=100)\n", x= "IUCN Category") 
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_blank(),
               axis.text.y = element_text(size=16), axis.text.x = element_blank(), axis.title = element_text(size=20), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=12))
p

#####
Threat_sizes <- lapply(Threats_gen, function(i) table(subset(Historic_assess_threat, taxonid %in% Threat_index_generic[[i]])$category))
names(Threat_sizes) <- names(Threat_index_generic)
Threat_size <-data.frame(Category=factor(), Assessments = factor(), Threat = factor())
for (i in 1:length(Threat_sizes)){
  freq <- as.data.frame(Threat_sizes[[i]])
  freq$Threat <- names(Threat_sizes)[i]
  names(freq) <- c("Category", "Assessments", "Threat")
  Threat_size<-rbind(Threat_size, freq)
}
Threat_size <- subset(Threat_size, Threat_size$Threat!="NA")
Threat_size$Threat <- recode(Threat_size$Threat, "1" = "Development", "2" = "Agriculture and Aquaculture", "3" = "Energy production and Mining", "4" = "Transportation", "5" = "Biological Resource Use", "6" = "Human Disturbance", "7" = "Natural system modifications", "8" = "Invasives", "9" = "Pollution", "10" = "Geological Events", "11" = "Climate Change and Weather", "12" = "Other")
Threat_size <- subset(Threat_size, Threat_size$Category != "EX")

Threat_df <- merge(Threat_df, Threat_size)

# Order factor
Threat_df$Category <- factor(Threat_df$Category, levels = c("LC","NT","VU","EN","CR"))

Threat_df <- arrange(Threat_df, Threat, Category)

Threat_df <- expandRows(Threat_df, "Assessments")
Threat_df$Rank <- NA
Threats_gen <- unique(Threat_df$Threat)
for (i in 1:length(Threats_gen)){
  Threat_df[which(Threat_df$Threat==Threats_gen[i]),]$Rank <- seq(1,nrow(Threat_df[Threat_df$Threat==Threats_gen[i],]))/nrow(Threat_df[Threat_df$Threat==Threats_gen[i],])
}

Sub_threats <- c("Agriculture and Aquaculture", "Biological Resource Use", "Invasives", "Pollution", "Climate Change and Weather", "Other")
Threat_df <- subset(Threat_df, Threat_df$Threat %in% Sub_threats)

p <- ggplot(data = Threat_df, aes(x=Rank, y=Probability, colour = Threat, xmin = 0))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(-1,1,0.1))
p <- p + scale_x_continuous(breaks = seq(0,1,0.10)) + labs(y = "Probability of extinction at 100 years\n", x = "\nCumulative Proportion of Assessments") 
p <- p+ scale_color_brewer(palette = "Set2")
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=16),  axis.title = element_text(size=20), legend.text = element_text(size=14), legend.title = element_text(size=14), strip.text = element_text(size=14))
p <- p + geom_hline(yintercept = 0)
p
