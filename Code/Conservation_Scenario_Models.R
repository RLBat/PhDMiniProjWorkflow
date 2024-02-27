## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)
require(expm)

`%!in%` = Negate(`%in%`)

set.seed(333)

#######################

## Global variables ##

#######################

Categories = c("LC", "NT", "VU", "EN", "CR", "EX")

# import transition tables
NoEX_tt <- as.matrix(read.csv("../Data/NoExtinction_TransitionTables.csv", header = T)[,2:7])
NoEXorTr_tt <- as.matrix(read.csv("../Data/NoEXorThreatened_TransitionTables.csv", header = T)[,2:7])
NoTr_tt <- as.matrix(read.csv("../Data/NoThreatened_TransitionTables.csv", header = T)[,2:7])
NoRecover_tt <- as.matrix(read.csv("../Data/Norecovery_transitiontable.csv", header = T)[,2:7])
Standard_tt <- as.matrix(read.csv("../Data/Standard_TransitionTables.csv", header = T)[,2:7])

scenarios <- list(NoEX_tt, NoEXorTr_tt, NoTr_tt, NoRecover_tt, Standard_tt)


scenario_names <- c("NoEX_tt", "NoEXorTr_tt", "NoTr_tt", "NoRecover_tt", "Standard_tt")

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

# generate 27 year predictions
predict27 <- predict_movements(scenarios = list(NoEX_tt, NoEXorTr_tt, NoTr_tt, NoRecover_tt, Standard_tt), time = 27)
predict27$scenario <- scenario_names

# grab the original values
props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89)
props <- as.numeric(props)

predict27_diff <- predict27

#grab the difference between the starting values and after 27 years
predict27_diff[,1:6] = apply(predict27_diff[,1:6], 2, function(x) as.numeric(x))
predict27_diff[,1:6] <- t(apply(predict27_diff[,1:6], 1, function(x) x-props))

## plot those differences after 27 years

#remove total row
predict27_diff <- predict27_diff[,c(1:6,8)]
predict27_diff <- gather(predict27_diff, "category", "PEX", -scenario)
predict27_diff$category <- factor(predict27_diff$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))

ggplot(data=predict27_diff, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_abline(intercept = 0) +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

##############################################

# generate the fire
scenario_27 <- transitions_ot(scenarios, time=27)

iter=0

for (i in scenario_27){
  iter=iter+1
  for (j in 1:73){
    i <- i %*% Standard_tt
  }
  scenario_27[[iter]] <- i
}

predictions_100 <- predict_movements(scenarios = scenario_27, time = 1)
predictions_100$scenario <- scenario_names

# differences from t=0
predict100_diff <- predictions_100
predict100_diff[,1:6] <- apply(predict100_diff[,1:6], 2, function(x) as.numeric(x))
predict100_diff[,1:6] <- t(apply(predict100_diff[,1:6], 1, function(x) x-props))

predict100_diff <- predict100_diff[,c(1:6,8)]
predict100_diff <- gather(predict100_diff, "category", "PEX", -scenario)
predict100_diff$category <- factor(predict100_diff$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))


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

###############################################

## FUNCTIONS for plotting changes over time ##

###############################################

calc_movements <- function(props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89),
                           scenario){
  prop_pred <- data.frame(scenario)
  prop_pred[,7] <- data.frame(nums = props)
  # multiply proportions by no. of species
  prop_pred <- prop_pred %>% dplyr::mutate(across(c(1:6), function(x) x*nums))
  # sum to get no. of species in each group after time
  prop_pred <- colSums(prop_pred[1:6])
  return(prop_pred)
}

movementsOT <- function(conservation_scenario = Standard_tt, time = 100){ 
  scenario <- conservation_scenario
  predOT <- data.frame(matrix(ncol=6, nrow=0))
  predOT <- rbind(predOT, calc_movements(scenario = conservation_scenario))
  names(predOT) <- Categories
  # names(predOT) <- Categories
  for(i in 1:time){
    scenario = scenario %*% conservation_scenario
    predOT <- rbind(predOT, calc_movements(scenario = scenario))
  }
  return(predOT)
}

format_for_plot <- function(predOT){
  predOT <- rownames_to_column(predOT, "Time")
  predOT$Time <- as.numeric(predOT$Time)-1
  predOT<-gather(predOT, key = "ThreatLevel", value = "Value", -Time)
  return(predOT)
}

Plot_TT <- function(transitionTable){
    standard100<-movementsOT(conservation_scenario = transitionTable)
    standard100 <- format_for_plot(standard100)
    standard100$ThreatLevel <- factor(standard100$ThreatLevel, levels = c("EX", "CR", "EN", "VU", "NT", "LC"))
    
    p <- ggplot(data = standard100, aes(x = Time, y = Value, colour = ThreatLevel)) + scale_color_manual(values = c("black","darkred", "darkorange", "gold", "darkcyan", "lightblue"))
    p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,22000,5000))
    #p <- p + geom_ribbon(aes(ymin=0, ymax=Value, alpha=0.5),fill="lightgrey", linetype = 0, show.legend = FALSE)
    p <- p + labs(y = "Species in category", x= "Time (years)", colour = "Red List Cateogry") 
    p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                   axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), 
                   legend.position = c(0.2,0.8), legend.text = element_text(size=14), legend.title = element_text(size=16), strip.text = element_text(size=14))
    return(p)
}
###############################################
## changes if we uplist every threatened species by 2050
##############################################

# 27 year species category movements
props27 <- as.numeric(c(LC=18302, NT = 4439, VU= 2429, EN = 1564, CR = 0, EX = 89))

# grab 73 year movements
standard_73tt <- as.matrix(Standard_tt %^% 73)

# apply to each other to get species per category after 73 years
predict_uplist <- predict_movements(props = props27, scenarios = list(standard_73tt), time = 1)

predict_uplist[,1:6] <- apply(predict_uplist[,1:6],2,function(x) as.numeric(x))
predict_uplist[,1:6] <- t(apply(predict_uplist[,1:6], 1, function(x) x-props))

#######################################
## Conservation strategy modelling ##
####################################

DoubleDownAll <- as.matrix(read.csv(file = "../Data/Transition_tables/DoubleDownlistAll.csv", row.names = 1))
HalfUpAll <- as.matrix(read.csv(file = "../Data/Transition_tables/HalveUplistAll.csv", row.names = 1))

# generate 27 year predictions
ConSce <- predict_movements(scenarios = list(DoubleDownAll, HalfUpAll, Standard_tt), time = 100)
ConSce$scenario <- c("Double Downlisting", "Halve Uplisting", "Business as usual")

# grab the original values
props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89)
props <- as.numeric(props)

#grab the difference between the starting values and after 27 years
ConSce[,1:6] = apply(ConSce[,1:6], 2, function(x) as.numeric(x))
ConSce[,1:6] <- t(apply(ConSce[,1:6], 1, function(x) x-props))

## plot those differences after x years
ConSce <- ConSce[,c(1:6,8)]
ConSce <- gather(ConSce, "category", "PEX", -scenario)
ConSce$category <- factor(ConSce$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))

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


#############
# SANDBOX #
############

BAU <- Standard_tt %^% 100

# Overall species extinction risk
sum(BAU[1:5,6] * props[1:5]/sum(props[1:5]))

















