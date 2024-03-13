## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)
require(expm)
require(ggplot2)
require(gridExtra)

`%!in%` = Negate(`%in%`)

set.seed(333)

#######################

## Global variables ##

#######################

Categories = c("LC", "NT", "VU", "EN", "CR", "EX")
props = as.numeric(c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89))

##############################
## 2050 scenario modelling ##
##############################

## DATA ###

# import transition tables
NoEX_tt <- as.matrix(read.csv("../Data/NoExtinction_TransitionTables.csv", header = T)[,2:7])
NoEXorTr_tt <- as.matrix(read.csv("../Data/NoEXorThreatened_TransitionTables.csv", header = T)[,2:7])
NoTr_tt <- as.matrix(read.csv("../Data/NoThreatened_TransitionTables.csv", header = T)[,2:7])
NoRecover_tt <- as.matrix(read.csv("../Data/Norecovery_transitiontable.csv", header = T)[,2:7])
Standard_tt <- as.matrix(read.csv("../Data/Standard_TransitionTables.csv", header = T)[,2:7])
TenthExtinct_tt <- as.matrix(read.csv("../Data/TenthExtinct_TransitionTables.csv", header = T))

scenarios <- list( Standard_tt, NoEX_tt, NoTr_tt, NoEXorTr_tt, NoRecover_tt)
scenario_names <- c("Standard_tt","NoEX_tt", "NoTr_tt", "NoEXorTr_tt", "NoRecover_tt")

## FUNCTIONS ##

# generates transition table for a given number of years in the future
# only works for a list of transition tables - list can be length = 1
transitions_ot <- function(transition_tables = scenarios, time){
  scenario_tts <- list()
  for(i in transition_tables){
    # matrix multiplication to predict movements
    i <- i %^% time
    scenario_tts<-append(scenario_tts, list(i))
  }
  return(scenario_tts)
}

# Predicts IUCN category composition for a list of transition tables at a given times
predict_movements <- function(props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89), 
                              scenarios, time = 27){
  final_sums <- data.frame()
  for(i in scenarios){
    # matrix multiplication to predict movements
    i <- i %^% time
    # convert to df
    test <- data.frame(i)
    test[,7] <- data.frame(nums = props)
    # multiply proportions by no. of species
    test <- test %>% dplyr::mutate(across(c(1:6), function(x) x*nums))
    # sum to get no. of species in each group after time
    final_sums <- rbind(final_sums, colSums(test))
    
  }
  colnames(final_sums) <- c("LC", "NT", "VU", "EN", "CR", "EX", "tot")
  return(final_sums)
}

#grab the difference between the starting values and future values
calc_diff <- function(future_tt, props){
  future_tt[,1:6] = apply(future_tt[,1:6], 2, function(x) as.numeric(x))
  future_tt[,1:6] <- t(apply(future_tt[,1:6], 1, function(x) x-props))
  ## prep formatting for plotting
  future_tt <- future_tt[,c(1:6,8)]
  future_tt <- gather(future_tt, "category", "PEX", -scenario)
  future_tt$category <- factor(future_tt$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))
  return(future_tt)
}

## WORKFLOW ##

# generate 27 year predictions
predict27 <- predict_movements(scenarios = list(NoEX_tt, NoEXorTr_tt, NoTr_tt, NoRecover_tt, Standard_tt), time = 27)
predict27$scenario <- scenario_names
predict27_diff <- predict27

#grab the difference between the starting values and after 27 years
predict27_diff[,1:6] = apply(predict27_diff[,1:6], 2, function(x) as.numeric(x))
predict27_diff[,1:6] <- t(apply(predict27_diff[,1:6], 1, function(x) x-props))

## plot those differences after 27 years

#remove total row and format for plotting
predict27_diff <- predict27_diff[,c(1:6,8)]
predict27_diff_long <- predict27_diff
predict27_diff <- gather(predict27_diff, "category", "PEX", -scenario)
predict27_diff$category <- factor(predict27_diff$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))

# generate the actual plot
ggplot(data=predict27_diff, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_abline(intercept = 0) +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

##
## Extending the modelling to get 100 year predictions
##

# generate the first 27 years
scenario_27 <- transitions_ot(scenarios, time=27)

# generate the next 73 years using the BAU tt
iter=0
for (i in scenario_27){
  iter=iter+1
  for (j in 1:73){
    i <- i %*% Standard_tt
  }
  scenario_27[[iter]] <- i
}

# generate the proportions of species for all scenarios using the t=100 tts
predictions_100 <- predict_movements(scenarios = scenario_27, time = 1)
predictions_100$scenario <- scenario_names

# differences from t=0
predict100_diff <- calc_diff(predictions_100, props = props)

# plot it
ggplot(data=predict100_diff, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = 0) +
  labs(y = "Difference of number of species in category", x = "Conservation Scenario") +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

##
## changes if we downlist every threatened species by 2050
##

# 27 year species category movements - all threatened species moved down a category
props27 <- as.numeric(c(LC=18302, NT = 4439, VU= 2429, EN = 1564, CR = 0, EX = 89))

# grab 73 year movements
standard_73tt <- as.matrix(Standard_tt %^% 73)

# apply to each other to get species per category after 73 years
predict_uplist <- predict_movements(props = props27, scenarios = list(standard_73tt), time = 1)
predict_uplist[,1:6] <- apply(predict_uplist[,1:6],2,function(x) as.numeric(x))
predict_uplist[,1:6] <- t(apply(predict_uplist[,1:6], 1, function(x) x-props))

## plotting?? ##

########################
## Changes over time ##
#######################

## FUNCTIONS ##

# calculates the distribution of species between categories given a starting
# distribution and a transition table
calc_movements <- function(props,
                           scenario){
  prop_pred <- data.frame(scenario)
  prop_pred[,7] <- data.frame(nums = props)
  # multiply proportions by no. of species
  prop_pred <- prop_pred %>% dplyr::mutate(across(c(1:6), function(x) x*nums))
  # sum to get no. of species in each group after time
  prop_pred <- colSums(prop_pred[1:6])
  return(prop_pred)
}

# uses a transition table and forecasts how a given distribution of species
# across categories changes over a given time. 
# requires calc_movements
movementsOT <- function(conservation_scenario = Standard_tt, time = 100, 
                        props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89)){ 
  scenario <- conservation_scenario
  predOT <- data.frame(matrix(ncol=6, nrow=0))
  predOT <- rbind(predOT, calc_movements(scenario = conservation_scenario, props = props))
  names(predOT) <- Categories
  # names(predOT) <- Categories
  for(i in 1:time){
    scenario = scenario %*% conservation_scenario
    predOT <- rbind(predOT, calc_movements(scenario = scenario, props = props))
  }
  return(predOT)
}

# formats the output from movementsOT ready for plotting
format_for_plot <- function(predOT){
  predOT <- rownames_to_column(predOT, "Time")
  predOT <- rbind(c(0,props), predOT)
  predOT$Time <- as.numeric(predOT$Time)
  predOT<-gather(predOT, key = "ThreatLevel", value = "Value", -Time)
  return(predOT)
}

# plots the output from format_for_plot
Plot_TT <- function(transitionTable, time = 100){
    standard100<-movementsOT(conservation_scenario = transitionTable, time = time)
    standard100 <- format_for_plot(standard100)
    standard100$ThreatLevel <- factor(standard100$ThreatLevel, levels = c("EX", "CR", "EN", "VU", "NT", "LC"))
    p <- ggplot(data = standard100, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue"))
    p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,25000,5000))
    #p <- p + geom_ribbon(aes(ymin=0, ymax=Value, alpha=0.5),fill="lightgrey", linetype = 0, show.legend = FALSE)
    p <- p + labs(y = "Species in category", x= "Time (years)", colour = "Red List Cateogry") 
    p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                   axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), 
                   legend.position = c(0.2,0.8), legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14))
    return(p)
}



## DATA ##

DoubleDownAll <- as.matrix(read.csv(file = "../Data/Transition_tables/DoubleDownlistAll.csv", row.names = 1))
HalfUpAll <- as.matrix(read.csv(file = "../Data/Transition_tables/HalveUplistAll.csv", row.names = 1))

DoubleDownThreat <- as.matrix(read.csv(file = "../Data/Transition_tables/DoubleDownlistThreatened.csv", row.names = 1))
DoubleDownENCR <- as.matrix(read.csv(file = "../Data/Transition_tables/DoubleDownlistEN_CR.csv", row.names = 1))
DoubleDownCR <- as.matrix(read.csv(file = "../Data/Transition_tables/DoubleDownlistCR.csv", row.names = 1))

HalfUpThreat <- as.matrix(read.csv(file = "../Data/Transition_tables/HalveUplistThreatened.csv", row.names = 1))
HalfUpENCR <- as.matrix(read.csv(file = "../Data/Transition_tables/HalveUplistEN_CR.csv", row.names = 1))
HalfUpCR <- as.matrix(read.csv(file = "../Data/Transition_tables/HalveUplistCR.csv", row.names = 1))

## WORKFLOW ##

##
## Conservation strategy modelling ##
##

# generate 100 year predictions (gets no. species)
ConSce <- predict_movements(scenarios = list(DoubleDownAll, HalfUpAll, Standard_tt), time = 100)
ConSce$scenario <- c("Double Downlisting", "Halve Uplisting", "Business as usual")

# calculate differences
ConSce <- calc_diff(ConSce, props = props)

# plot
ggplot(data=ConSce, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_abline(intercept = 0) +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  scale_y_continuous(breaks = seq(-3000,5000,500), minor_breaks = seq(-3000,5000,500)) +
  labs(y= "Difference from t=0 of number of\n species in each category", x = "Conservation Scenario", fill = "Red List Category") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16, colour = c( NA, "black")) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

ConSceAll <- predict_movements(scenarios = list(Standard_tt, DoubleDownAll, 
                              HalfUpAll, DoubleDownThreat, HalfUpThreat, 
                              DoubleDownENCR, HalfUpENCR, DoubleDownCR, 
                              HalfUpCR), time = 26)
ConSceAll<- ConSceAll[,1:6]

ConSceAll$scenario <- c("Business as usual", "Double Downlisting", "Halve Uplisting", 
                     "Double Downlisting (Threatened)", "Halve Uplisting (Threatened)", 
                     "Double Downlisting (EN & CR)", "Halve Uplisting (EN & CR)", 
                     "Double Downlisting (CR)", "Halve Uplisting (CR)")

ConSceAll[,1:6] = apply(ConSceAll[,1:6], 2, function(x) as.numeric(x))
ConSceAll[,1:6] <- t(apply(ConSceAll[,1:6], 1, function(x) x-props))

# add the original 4 scenarios
predict27_diff_long$scenario <- c("No Extinctions", "Can't become Extinct or threatened", "Can't become threatened", "Only Uplisting", "Standard")
ConSceAll <- rbind(ConSceAll, predict27_diff_long[c(4,1,3,2),])
ConSceAll <- ConSceAll[,c(7,1:6)]
ConSceAll[,c(2:7)] <- round(ConSceAll[,c(2:7)])

# condense down
ConSceCond <- ConSceAll %>% mutate(NotThreat = LC + NT) %>% mutate(Threat = VU + EN + CR)
ConSceCond <- ConSceCond[,c(1,8,9,7)]

####
# Plot Over Time

Prep_for_plot <- function(transitionTable, time = 100){
  standard100<-movementsOT(conservation_scenario = transitionTable, time = time)
  standard100<-sweep(standard100, 2, props)
  standard100 <- format_for_plot(standard100)
  standard100[which(standard100$Time == 0),"Value"] <- 0
  standard100$ThreatLevel <- factor(standard100$ThreatLevel, levels = c("EX", "CR", "EN", "VU", "NT", "LC"))
  return(standard100)
}

No_EX_OT <- Prep_for_plot(NoEX_tt)
p1 <-ggplot(data = No_EX_OT, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue")) +
  geom_line(size=1.2) + scale_y_continuous(breaks = seq(-5000,5000,100)) + ylim(-1500,3000) +
  labs(y = "Comparative species in category", x= "", colour = "Red List Cateogry", title = "No Extinctions") + geom_hline(yintercept = 0) + geom_vline(xintercept = 29, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16), axis.text.x = element_text(size=0, colour = c(NA, "black")), axis.title = element_text(size=20), 
        legend.position = "none", legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14),
        plot.title = element_text(size = 18, hjust = 1, vjust = -3))

NoRecoverOT <- Prep_for_plot(NoRecover_tt)
p2 <-ggplot(data = NoRecoverOT, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue")) +
  geom_line(size=1.2) + scale_y_continuous(breaks = seq(-10000,5000,1000)) +
  labs(y = "", x= "", colour = "Red List Cateogry", title = "No Downlistings") + geom_hline(yintercept = 0) + geom_vline(xintercept = 29, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16), axis.text.x = element_text(size=0), axis.title = element_text(size=20), 
        legend.position = "none", legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14),
        plot.title = element_text(size = 18, hjust = 1, vjust = -3))


HalfUpOT <- Prep_for_plot(HalfUpThreat)
p3 <-ggplot(data = HalfUpOT, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue")) +
  geom_line(size=1.2) + scale_y_continuous(breaks = seq(-5000,5000,100)) + ylim(-1500,3000) +
  labs(y = "Comparative species in category", x= "Time (years)", colour = "Red List Cateogry", title = "Halved Uplistings") + geom_hline(yintercept = 0) + geom_vline(xintercept = 29, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), 
        legend.position = "none", legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14),
        plot.title = element_text(size = 18, hjust = 1, vjust = -3))

DoubleDownOT <- Prep_for_plot(DoubleDownThreat)
p4 <-ggplot(data = DoubleDownOT, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue")) +
  geom_line(size=1.2) + scale_y_continuous(breaks = seq(-5000,5000,100)) + ylim(-1500,3000) +
  labs(y = "", x= "Time (years)", colour = "Red List Cateogry", title = "Doubled Downlistings") + geom_hline(yintercept = 0) + geom_vline(xintercept = 29, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), 
        legend.position = "none", legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14),
        plot.title = element_text(size = 18, hjust = 1, vjust = -3))

BAU_OT <- Prep_for_plot(Standard_tt)
p5 <- ggplot(data = BAU_OT, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue")) +
  geom_line(size=1.2) + scale_y_continuous(breaks = seq(-5000,5000,500), minor_breaks = seq(-5000,5000,500)) + ylim(-1500,3000) +
  labs(y = "", x= "", colour = "Red List Cateogry", title = "Business as Usual") + geom_hline(yintercept = 0) + geom_vline(xintercept = 29, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), 
        legend.position = "none", legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14),
        plot.title = element_text(size = 18, hjust = 1, vjust = -3) )

leg <- ggplot(data = BAU_OT, aes(Time, Value, colour = ThreatLevel)) + geom_line() +
  scale_colour_manual(values = c("lightblue", "darkcyan", "gold", "darkorange", "darkred", "black"), name = "Red List Category", 
                    labels = c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Extinct")) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 14), legend.position = "bottom")
leg <- cowplot::get_legend(leg)

grid.arrange(p1, p5, p3, p4, leg, nrow = 3, ncol=2, layout_matrix = cbind(c(5,1,3), c(5,2,4)), heights = c(1,4,4))

#############
# SANDBOX #
############

BAU <- Standard_tt %^% 100

# Overall species extinction risk
sum(BAU[1:5,6] * props[1:5]/sum(props[1:5]))

















