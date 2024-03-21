
##### PACKAGES ######

require(dplyr)
require(tidyverse)
require(xtable)
require(ggplot2)

###### DATA ######

Overall_probs_full <- read.csv("../Data/Overallbootoutput.csv", stringsAsFactors = T)
Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")

# For some reason writing this more efficiently breaks it I don't know why
# Grabbing 100yr pEX
Overall_probs <- Overall_probs_full[Overall_probs_full$Time==100,c("Threat_level", "Mean")]
names(Overall_probs) <- c("category", "Bates_pex")
Overall_probs$category <- as.character(Overall_probs$category)
# Add EX
Overall_probs[nrow(Overall_probs)+1,c(1,2)] <- c("EX", 1)
# reorder the df
Overall_probs <- Overall_probs %>% slice(match(Categories, category))

# Overall_PEX <- Overall_probs$Mean
# names(Overall_PEX) <- c(as.character(Overall_probs$Threat_level))
# Overall_PEX <- append(Overall_PEX, 1)
# names(Overall_PEX)[6] <- "EX"

Overall_probs$RLI_weighting <- c(0,1,2,3,4,5)
Overall_probs$CritE_weighting <- c(0.0001, 0.01, 0.1, 0.667, 0.999, 1)
#add the uncertainty
Overall_probs_full <- Overall_probs_full[which(Overall_probs_full$Time==100),c("Threat_level", "Top", "Bottom")]
names(Overall_probs_full) <- c("category", "Top", "Bottom")
Overall_probs <- Overall_probs %>% left_join(Overall_probs_full, by = "category")
Overall_probs[which(Overall_probs$category=="EX"), c("Top", "Bottom")] <- 1

Overall_probs[,2:5] <- sapply(Overall_probs[,2:5], as.numeric)

###### FUNCTIONS ######

Edit_codes <- function(RLI_data){
  RLI_data$category <- recode(RLI_data$category, "CR (PE)" = "EX", "EW" = "EX", "CR(PE)"= "EX", "CR(PEW)"= "EX")
  RLI_data <- filter(RLI_data, RLI_data$category!= "DD")
}

Calc_weightings <- function(RLI_data){
  # generate the max values
  Max_value <- length(unique(RLI_data$binomial)) # Bates pEX
  Max_value_RLI <- Max_value*5 # RLI
  
  # Add weightings to df
  RLI_data$category <- as.character(RLI_data$category)
  RLI_data <- RLI_data %>% left_join(Overall_probs, by = "category")
  # force numeric
  RLI_data[,c(4:8)] <- sapply(RLI_data[,c(4:8)],as.numeric)
  # Calculate Index values for each year
  PEX <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Bates_pex))/Max_value)
  RLI <- RLI_data %>% group_by(year) %>% summarise((Max_value_RLI - sum(RLI_weighting))/Max_value_RLI)
  CritE <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(CritE_weighting))/Max_value)
  PEX_Top <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Top))/Max_value)
  PEX_Btm <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Bottom))/Max_value)
  names(PEX) <- c("year", "category")
  names(RLI) <- c("year", "category")
  names(CritE) <- c("year", "category")
  names(PEX_Top) <- c("year", "min")
  names(PEX_Btm) <- c("year", "max")
  PEX <- left_join(PEX, PEX_Top) %>% left_join(.,PEX_Btm)
  RLI_values <- bind_rows("Bates" = PEX, "RLI" = RLI, "CritE" = CritE, .id = "id")
  return(RLI_values)
}

Calc_uncertainty <- function(RLI_data){
  
}

##### BIRBS #######

## Import RLI Data
Bird_RLI <- read.csv("../Data/Bird_RLI_original.csv", stringsAsFactors = T)

# Format the Bird RLI data for modelling
Format_BirdRLI <- function(Bird_RLI){
  Bird_RLI <- Bird_RLI[,c(2, 4:10)]
  # rename columns
  names(Bird_RLI) <- c("binomial", "1988", "1994", "2000","2004","2008","2012","2016")
  # pivot to long formatting
  Bird_RLI <- pivot_longer(Bird_RLI, cols = c("1988", "1994", "2000","2004","2008","2012","2016")
                           , names_to = "year", values_to = "category")
  Bird_RLI$year <- as.numeric(Bird_RLI$year)
  return(Bird_RLI)
}

Bird_RLI <- Format_BirdRLI(Bird_RLI)
# Condense categories down + remove DD
Bird_RLI <- Edit_codes(Bird_RLI)


################################

##### Mammal ####

Mammal_RLI <- read.csv("../Data/RLI/Mammal_RLI.csv", stringsAsFactors = T)
#subset to relevant columns
Mammal_RLI <- Mammal_RLI[,c(6,7,8)]
names(Mammal_RLI) <- c("binomial", "1996","2008")
# convert to long format
Mammal_RLI <- gather(Mammal_RLI, key = "year", value = "category", "1996", "2008")
Mammal_RLI[,3] <- as.factor(Mammal_RLI[,3])
Mammal_RLI[,1] <- as.character(Mammal_RLI[,1])
Mammal_RLI[,2] <- as.numeric(Mammal_RLI[,2])
# remove empty rows
Mammal_RLI <- Mammal_RLI[which(Mammal_RLI$binomial!=""),]
Mammal_RLI <- Edit_codes(Mammal_RLI)

###############

## Cycads ##

Cycad_RLI <- read.csv("../Data/RLI/cycad.rli.phy.match.csv", stringsAsFactors = T)
Cycad_RLI <- Cycad_RLI[,c(2,5,6)]
names(Cycad_RLI) <- c("binomial", "2003", "2014")
Cycad_RLI <- gather(Cycad_RLI, key = "year", value = "category", "2003", "2014")
# recode the categories cause they're numeric by default
Cycad_RLI$category <- recode(Cycad_RLI$category, "0" = "LC", "1" = "NT", "2" = "VU", "3" = "EN", "4" = "CR", "5" = "EX")
Cycad_RLI$year <- as.numeric(Cycad_RLI$year)
Cycad_RLI<- Edit_codes(Cycad_RLI)

################

# ## Corals ##
# 
# Coral_RLI <- read.csv("../Data/RLI/Coral_RLI_data.csv", stringsAsFactors = T)
# 
# Coral_RLI <- Coral_RLI[,c(2,5,7)]

#################

## Amphibians ##

Amphib_RLI <- read.csv("../Data/RLI/Amphibian_RLI_original.csv", stringsAsFactors = F)
Amphib_RLI <- Amphib_RLI[,c(3,4,5,6)]
# combine the columns into one column of scientific names
Amphib_RLI <- unite(Amphib_RLI, "binomial", c(1,2), sep = " ")
names(Amphib_RLI) <- c("binomial", "2004", "1980")
Amphib_RLI <- gather(Amphib_RLI, key = "year", value = "category", "1980", "2004")
# remove DD species and edit any existing CR(PE) values to CR etc
Amphib_RLI$year <- as.numeric(Amphib_RLI$year)
Amphib_RLI <- Edit_codes(Amphib_RLI)

################

#### graphing and adding weightings ####

Mammal <- Calc_weightings(Mammal_RLI)
Bird <- Calc_weightings(Bird_RLI)
Amphibian <- Calc_weightings(Amphib_RLI)
Cycad <- Calc_weightings(Cycad_RLI)

All_RLI <- bind_rows("Mammal" = Mammal, "Bird" = Bird, "Amphibian" = Amphibian, "Cycad" = Cycad, .id = "clade")

# ggplot(data = All_RLI[which(All_RLI$id=="Bates"),], aes(x = year, y = category)) + 
#   geom_point(aes(shape=clade, colour = clade)) +geom_line(aes(colour = clade)) +
#   ylim(0,1) + labs (colour = "Bates", shape = "Bates")
# 
# ggplot(data = All_RLI[which(All_RLI$id=="RLI"),], aes(x = year, y = category)) + 
#   geom_point(aes(shape=clade, colour = clade)) +geom_line(aes(colour = clade)) +
#   ylim(0,1) + labs(colour = "RLI", shape = "RLI")

ggplot(data = All_RLI, aes(x = year, y = category, group = interaction(id, clade))) + 
  geom_ribbon(aes(ymin=min, ymax=max, alpha = 0.3), colour = "grey", fill = "lightgrey", linetype = 2, show.legend = FALSE)+
  geom_line(aes(colour = id), linewidth = 1) + geom_point(aes(shape=clade, colour = id), size = 2) + 
  scale_colour_manual(values = c("darkcyan", "orange","darkred")) + scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  ylim(0.5,1) + labs(colour = "RLI", shape = "RLI") + labs(y = "Index Value/\nSurvival Probability (t=100)", x = "Year") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "right", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

###################

### Collect Slope values for all clade/id combos ###

RLI_slope <- All_RLI %>% group_by(clade, id) %>% summarise(Gradient <- lm(category ~ year)[[1]][2])
names(RLI_slope) <- c("Clade", "Id", "Gradient")
RLI_slope <- spread(RLI_slope, Clade, Gradient)

###################

### Predictions

##### REQUIRES Conservation_Scenario_Models.R ######
# base values from just RLI
RLI_predict<-All_RLI[which(All_RLI$id=="RLI"),c(1,3,4)]

## Scenario to 2050 then BAU
calcWeightingsPredict2050 <- function(clade = "Bird", clade_props = Bird_RLI, RLIweight = Overall_probs$RLI_weighting){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  timeDiff <- 2050-maxYear
  props2050 <- predict_movements(props = maxYearProps, scenarios = scenarios, time = timeDiff) # predict to 2050
  # grab max RLI value
  total = props2050$tot[1]*max(RLIweight)
  #remove total column
  props2050<-props2050[,1:6]
  # convert to weightings
  props2050 <- sweep(props2050, 2, RLIweight, "*")
  # calc values
  props2050 <- (total - rowSums(props2050))/total
  propsAll<- rbind(data.frame(clade = clade, year = "2050", scenario = scenario_names, value = props2050),
                        data.frame(clade = clade, year = maxYear, scenario = scenario_names, 
                                   value = as.numeric(RLI_predict[which(RLI_predict$clade==clade 
                                                                        & RLI_predict$year==maxYear),"category"])))
  return(list(props2050, propsAll))
} # outputs RLI values at 2050 for all scenarios and a formatted table

Bird_2050_all <- calcWeightingsPredict2050("Bird", Bird_RLI)

#grabs proportions of species in each category in 2016 (most recent assessment year)
Bird_props_2016 <- table(Bird_RLI[which(Bird_RLI$year==2016),"category"])
# coerce to numeric
Bird_props_2016 <- as.numeric(Bird_props_2016[order(factor(names(Bird_props_2016), levels = Categories))])[1:6]
# feed the proportions into the movement predictor with group of scenarios
Bird_props <- predict_movements(props = Bird_props_2016, scenarios = scenarios, time = 34) # predict to 2050
# grab max RLI value
total = Bird_props$tot[1]*5
#remove total column
Bird_props<-Bird_props[,1:6]
# convert to weightings
Bird_2050 <- sweep(Bird_props, 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
Bird_2050<-(total-rowSums(Bird_2050))/total
#Place into df with 2016 value
Bird_2050_all<- rbind(data.frame(clade = "Bird", year = "2050", scenario = scenario_names, value = Bird_2050),
          data.frame(clade = "Bird", year = "2016", scenario = scenario_names, 
                     value = as.numeric(RLI_predict[which(RLI_predict$clade=="Bird" 
                      & RLI_predict$year==2016),"category"])))

print(xtable(spread(Bird_2050_all, year, value),display = c("d", "s", "s", "G", "g"), digits = 3),include.rownames=FALSE)


## BAU to 2050 then scenario
## GBF

#grabs the proportions of species in each category, at a given point in the future, given a list of
#scenarios. Uses calculated proportions from the last year of the RLI
calcRLI <- function(clade = "Bird", clade_props = Bird_RLI, years=10, scenarios = list(Standard_tt)){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  futureProps <- predict_movements(props = maxYearProps, scenarios = scenarios, time = years)
  return(futureProps[1:6])
}


## BAU to 2050 then scenarios to 2100
calcWeightingsGBF2100 <- function(clade = "Bird", 
                                  clade_props = Bird_RLI, 
                                  scenarios = list(Standard_tt, NoEX_tt, NoTr_tt, NoEXorTr_tt, NoRecover_tt), 
                                  RLIweight = Overall_probs$RLI_weighting,
                                  scenario_names){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  total = sum(maxYearProps)*max(RLIweight)
  # years til 2050
  timeDiff <- 2050-maxYear
  # years til next multiple of 10
  next10 <- (timeDiff %% 10)
  maxYearRLI <- maxYearProps * RLIweight
  maxYearRLI <- (total-sum(maxYearRLI))/total
  propsOT <- data.frame(clade = clade, year = maxYear, scenario = "Standard_tt", value = maxYearRLI)
  
  for(i in seq(next10, timeDiff, 10)){
    tempProps <- predict_movements(props = as.numeric(maxYearProps), scenarios = list(Standard_tt), time = i)
    tempProps <- tempProps[,1:6]
    #convert to weighted values
    tempProps <- sweep(tempProps, 2, RLIweight, "*")
    #calculate Index value
    tempProps <- (total-rowSums(tempProps))/total
    propsOT <- rbind(propsOT, data.frame(clade = clade, year = maxYear+i, scenario = "Standard_tt", value = tempProps))
  }
  #get proportions at 2050 for BAU
  props2050 <- calcRLI(clade = clade, clade_props = clade_props, years = timeDiff)
  RLI2050 <- as.numeric(calcWeightingsPredict2050(clade, clade_props, RLIweight = RLIweight)[[1]])[1]
  propsOT <- rbind(propsOT,data.frame(clade = clade, year = 2050, scenario = scenario_names, value = RLI2050))
  # grab at every 10 years
  for(i in seq(10,50,10)){
    tempProps <- predict_movements(props = as.numeric(props2050), scenarios = scenarios, time = i)
    tempProps <- tempProps[,1:6]
    #convert to weighted values
    tempProps <- sweep(tempProps, 2, RLIweight, "*")
    #calculate Index value
    tempProps <- (total-rowSums(tempProps))/total
    propsOT <- rbind(propsOT, data.frame(clade = clade, year = 2050+i, scenario = scenario_names, value = tempProps))
  }
  return(propsOT)
}

test <- calcWeightingsGBF2100("Mammal", Mammal_RLI, RLIweight = as.numeric(Overall_probs$Bates_pex))

BirdStd2050<-as.numeric(Bird_props[5,])
Bird2100<- predict_movements(props = BirdStd2050, scenarios = scenarios, time =50)

Bird2100<-Bird2100[,1:6]
# convert to weightings
Bird_GBF <- sweep(Bird2100, 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
Bird_GBF<-(total-rowSums(Bird_GBF))/total
#Place into df with 2016 value
Bird_GBF<- rbind(data.frame(clade = "Bird", year = "2100", scenario = scenario_names, value = Bird_GBF),
          data.frame(clade = "Bird", year = "2016", scenario = scenario_names, 
                     value = as.numeric(RLI_predict[which(RLI_predict$clade=="Bird" 
                                                          & RLI_predict$year==2016),"category"]))) %>%
    rbind(., data.frame(clade = "Bird", year = 2050, scenario = scenario_names, value = Bird_2050[5]))

print(xtable(spread(Bird_GBF, year, value),display = c("d", "s", "s", "G", "g", "g"), digits = 3),include.rownames=FALSE)

## BAU to 2025 then scenario





BirdStd2025 <- predict_movements(props = Bird_props_2016, scenarios = scenarios[5], time = 9)
a <- sweep(BirdStd2025[,1:6], 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
BirdRLI_BAU_2025<-(total-rowSums(a))/total
Bird_con_2050 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 25)
Bird_con_2075 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 50)
Bird_con_2100 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 75)
Bird_con_2125 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 100)

calc_values <- function(predicted_props){
  predicted_props<-predicted_props[,1:6]
  # convert to weightings
  predicted_props <- sweep(predicted_props, 2, c(0,1,2,3,4,5), "*")
  # Calculate RLI value
  predicted_props<-(total-rowSums(predicted_props))/total
  return(predicted_props)
}

c<- data.frame(scenario = c("Standard", "HalfUpThreat", "DoubleDownThreat"), "2016" = 0.898, "2025" = BirdRLI_BAU_2025, "2050" = calc_values(Bird_con_2050),
           "2075" = calc_values(Bird_con_2075), "2100" = calc_values(Bird_con_2100), "2125" = calc_values(Bird_con_2125))

print(xtable(c,display = c("d", "s", "G", "g", "g", "g", "g", "g"), digits = 3),include.rownames=FALSE)

######################

All_RLIpredict <- All_RLI[which(All_RLI$id=="RLI"),c(1,3,4)]
All_RLIpredict$scenario <- "BAU"
names(All_RLIpredict)[3] <- "Index"

formatCalcWeightings <- function(clade, 
                                 clade_props, 
                                 scenarios = list(Standard_tt, NoEX_tt, NoTr_tt, NoEXorTr_tt, NoRecover_tt), 
                                 RLIweight = Overall_probs$RLI_weighting,
                                 scenario_names = c("Standard_tt","NoEX_tt", "NoTr_tt", "NoEXorTr_tt", "NoRecover_tt")){
  GBFdf<- calcWeightingsGBF2100(clade, clade_props, scenarios = scenarios, RLIweight= RLIweight, scenario_names)
  #GBFdf[which(GBFdf$scenario=="Standard_tt"),"scenario"] <- "BAU"
  GBFdf$year <- as.numeric(GBFdf$year)
  names(GBFdf)[4] <- "Index"
  All_RLIpredict <- rbind(All_RLIpredict, GBFdf)
  return(All_RLIpredict)
}

#Birds
All_RLIpredict <- formatCalcWeightings("Bird", Bird_RLI)
#Mammals
All_RLIpredict <- formatCalcWeightings("Mammal", Mammal_RLI)
# Amphib
All_RLIpredict <- formatCalcWeightings("Amphibian", Amphib_RLI)
#Cycad
All_RLIpredict <- formatCalcWeightings("Cycad", Cycad_RLI)

All_RLIpredict[which(All_RLIpredict$scenario == "BAU"),"scenario"] <- "RLI"
All_RLIpredict[which(All_RLIpredict$scenario == "Standard_tt"),"scenario"] <- "BAU"
#RLIyears <- All_RLIpredict[which(All_RLIpredict$year <2025),]
#RLIyears <- RLIyears %>% group_by(clade) %>% filter(year == max(year)) %>% mutate(scenario = "BAU")
#All_RLIpredict <- rbind(All_RLIpredict, RLIyears)

All_RLIpredict$scenario <- factor(All_RLIpredict$scenario, levels = c("RLI", "BAU", "NoEX_tt", "NoTr_tt", "NoEXorTr_tt", "NoRecover_tt"))



## swap to faceting for clade or it'll be a mess
p <-ggplot(data = All_RLIpredict, aes(x = year, y = Index, group = interaction(scenario,clade))) + 
  geom_line(aes(colour = scenario), linewidth = 1) + geom_point(aes(colour = scenario), size = 1) + 
  scale_colour_manual(values = c("black", "darkcyan", "pink", "orange","darkgreen", "darkred"), 
                      labels = c("RLI", "Business as Usual", "No Extinctions",  "Cannot become Threatened", "Cannot become Extinct or Threatened", "Worst Case Scenario")) + 
  scale_x_continuous(breaks = seq(1980, 2100, 10)) + ylim(0.4,1) + labs(colour = "RLI", shape = "RLI") + 
  geom_vline(xintercept = 2050, linetype="dotted") +
  labs(y = "Index Value", x = "Year", colour = "Scenario") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "top", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

p + facet_wrap(~clade)

### plotting with different weightings shouuuld just be as so:

All_RLIpredict <- All_RLI[which(All_RLI$id=="Bates"),c(1,3,4)]
All_RLIpredict$scenario <- "BAU"
names(All_RLIpredict)[3] <- "Index"

#Birds
All_RLIpredict <- formatCalcWeightings("Bird", Bird_RLI, RLIweight = as.numeric(Overall_probs$Bates_pex))
#Mammals
All_RLIpredict <- formatCalcWeightings("Mammal", Mammal_RLI, RLIweight = Overall_probs$Bates_pex)
# Amphib
All_RLIpredict <- formatCalcWeightings("Amphibian", Amphib_RLI, RLIweight = Overall_probs$Bates_pex)
#Cycad
All_RLIpredict <- formatCalcWeightings("Cycad", Cycad_RLI, RLIweight = Overall_probs$Bates_pex)

All_RLIpredict[which(All_RLIpredict$scenario == "BAU"),"scenario"] <- "RLI"
All_RLIpredict[which(All_RLIpredict$scenario == "Standard_tt"),"scenario"] <- "BAU"
All_RLIpredict$scenario <- factor(All_RLIpredict$scenario, levels = c("RLI", "BAU", "NoEX_tt", "NoTr_tt", "NoEXorTr_tt", "NoRecover_tt"))

## swap to faceting for clade or it'll be a mess
p <-ggplot(data = All_RLIpredict, aes(x = year, y = Index, group = interaction(scenario,clade))) + 
  geom_line(aes(colour = scenario), linewidth = 1) + geom_point(aes(colour = scenario), size = 1) + 
  scale_colour_manual(values = c("black", "darkcyan", "pink", "orange","darkgreen", "darkred"), 
                      labels = c("RLI", "Business as Usual", "No Extinctions",  "Cannot become Threatened", "Cannot become Extinct or Threatened", "Worst Case Scenario")) + 
  scale_x_continuous(breaks = seq(1980, 2100, 10)) + ylim(0.8,1) + labs(colour = "RLI", shape = "RLI") + 
  geom_vline(xintercept = 2050, linetype="dotted") +
  labs(y = "Survival Probability (t=100)", x = "Year", colour = "Scenario") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "top", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

p + facet_wrap(~clade)


#####################################
### other conservation scenarios:

## uncertainty??

ConScePredict <- list(Standard_tt, HalfUpThreat, DoubleDownThreat, NoRecover_tt)
ConSce_names <- c("Standard_tt", "HalfUpThreat", "DoubleDownThreat", "NoRecover")

All_RLIpredict <- All_RLI[which(All_RLI$id=="RLI"),c(1,3,4)]
All_RLIpredict$scenario <- "BAU"
names(All_RLIpredict)[3] <- "Index"

#Birds
All_RLIpredict <- formatCalcWeightings("Bird", Bird_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$RLI_weighting, scenario_names = ConSce_names)
#Mammals
All_RLIpredict <- formatCalcWeightings("Mammal", Mammal_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$RLI_weighting, scenario_names = ConSce_names)
# Amphib
All_RLIpredict <- formatCalcWeightings("Amphibian", Amphib_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$RLI_weighting, scenario_names = ConSce_names)
#Cycad
All_RLIpredict <- formatCalcWeightings("Cycad", Cycad_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$RLI_weighting, scenario_names = ConSce_names)

All_RLIpredict[which(All_RLIpredict$scenario == "BAU"),"scenario"] <- "RLI"
All_RLIpredict[which(All_RLIpredict$scenario == "Standard_tt"),"scenario"] <- "BAU"
All_RLIpredict$scenario <- factor(All_RLIpredict$scenario, levels = c("RLI", "BAU", ConSce_names[-1]))

## swap to faceting for clade or it'll be a mess
p <-ggplot(data = All_RLIpredict, aes(x = year, y = Index, group = interaction(scenario,clade))) + 
  geom_line(aes(colour = scenario), linewidth = 1) + geom_point(aes(colour = scenario), size = 1) + 
  scale_colour_manual(values = c("black", "darkcyan", "pink", "orange", "darkred"), 
                      labels = c("RLI", "BAU", ConSce_names[-1])) + 
  scale_x_continuous(breaks = seq(1980, 2100, 10)) + ylim(0.4,1) + labs(colour = "RLI", shape = "RLI") + 
  geom_vline(xintercept = 2050, linetype="dotted") +
  labs(y = "Index Value", x = "Year", colour = "Scenario") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "top", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

p + facet_wrap(~clade)

All_RLIpredict <- All_RLI[which(All_RLI$id=="Bates"),c(1,3,4)]
All_RLIpredict$scenario <- "BAU"
names(All_RLIpredict)[3] <- "Index"

#Birds
All_RLIpredict <- formatCalcWeightings("Bird", Bird_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$Bates_pex, scenario_names = ConSce_names)
#Mammals
All_RLIpredict <- formatCalcWeightings("Mammal", Mammal_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$Bates_pex, scenario_names = ConSce_names)
# Amphib
All_RLIpredict <- formatCalcWeightings("Amphibian", Amphib_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$Bates_pex, scenario_names = ConSce_names)
#Cycad
All_RLIpredict <- formatCalcWeightings("Cycad", Cycad_RLI, scenarios = ConScePredict, RLIweight = Overall_probs$Bates_pex, scenario_names = ConSce_names)

All_RLIpredict[which(All_RLIpredict$scenario == "BAU"),"scenario"] <- "RLI"
All_RLIpredict[which(All_RLIpredict$scenario == "Standard_tt"),"scenario"] <- "BAU"
All_RLIpredict$scenario <- factor(All_RLIpredict$scenario, levels = c("RLI", "BAU", ConSce_names[-1]))

## swap to faceting for clade or it'll be a mess
p <-ggplot(data = All_RLIpredict, aes(x = year, y = Index, group = interaction(scenario,clade))) + 
  geom_line(aes(colour = scenario), linewidth = 1) + geom_point(aes(colour = scenario), size = 1) + 
  scale_colour_manual(values = c("black", "darkcyan", "pink", "orange","darkred"), 
                      labels = c("RLI", "BAU", ConSce_names[-1])) + 
  scale_x_continuous(breaks = seq(1980, 2100, 10)) + ylim(0.8,1) + labs(colour = "RLI", shape = "RLI") + 
  geom_vline(xintercept = 2050, linetype="dotted") +
  labs(y = "Survival Probability (t=100)", x = "Year", colour = "Scenario") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "top", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

p + facet_wrap(~clade)

#############################

## testing different weights

Birds100 <- read.csv("../Data/Birds4way100.csv", header = TRUE)
Birds100$Threat_level <- factor(Birds100$Threat_level, levels = Categories)
birdWeightCats <- unique(Birds100$Data)
Birds100 <- spread(Birds100, Data, Probability)
Birds100low <- Birds100[which(Birds100$Source=="Bottom"),]
Birds100high <- Birds100[which(Birds100$Source=="Top"),]
Birds100 <- Birds100[which(Birds100$Source=="Median"),]
#Birds100 <- pivot_wider(Birds100, id_cols = c("Threat_level"), names_from = c("Data", "Source"), values_from = Probability)
formatbirdcats <- function(Birds100){
  Birds100 <- Birds100[,-1]
  names(Birds100)<- c("category", "RLICRweight", "RLIEXweight", "T7weight", "noT7weight")
  Birds100 <- rbind(Birds100, c("EX", 1,1,1,1))
  return(Birds100)
}

Birds100 <- formatbirdcats(Birds100)
Birds100low <- formatbirdcats(Birds100low)
Birds100high <- formatbirdcats(Birds100high)

Birds100$Batespex <- Overall_probs$Bates_pex
Birds100high$Batespex <- Overall_probs$Top
Birds100low$Batespex <- Overall_probs$Bottom

Calc_weightings_extend <- function(RLI_data, weightings){
  # generate the max values
  Max_value <- length(unique(RLI_data$binomial)) # Bates pEX
  # Add weightings to df
  RLI_data$category <- as.character(RLI_data$category)
  RLI_data <- RLI_data %>% left_join(weightings, by = "category")
  # force numeric
  RLI_data[,c(4:max(ncol(RLI_data)))] <- sapply(RLI_data[,c(4:max(ncol(RLI_data)))],as.numeric)
  # Calculate Index values for each year
  noT7 <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(noT7weight))/Max_value)
  T7 <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(T7weight))/Max_value)
  RLICR <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(RLICRweight))/Max_value)
  RLIEX <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(RLIEXweight))/Max_value)
  Bates <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(Batespex))/Max_value)
  names(noT7) <- c("year", "category")
  names(T7) <- c("year", "category")
  names(RLICR) <- c("year", "category")
  names(RLIEX) <- c("year", "category")
  names(Bates) <- c("year", "category")
  RLI_values <- bind_rows("noT7" = noT7, "T7" = T7, "RLICR" = RLICR, "RLIEX" = RLIEX, "Bates" = Bates, .id = "id")
  return(RLI_values)
}

# Calc_weightings_extend <- function(RLI_data, weightings){
#   # generate the max values
#   Max_value <- length(unique(RLI_data$binomial)) # Bates pEX
#   # Add weightings to df
#   RLI_data$category <- as.character(RLI_data$category)
#   RLI_data <- RLI_data %>% left_join(weightings, by = "category")
#   # force numeric
#   RLI_data[,c(4:max(ncol(RLI_data)))] <- sapply(RLI_data[,c(4:max(ncol(RLI_data)))],as.numeric)
#   RLI_values <- data.frame("id"=NA, "year"=NA, "category"=NA)[0,]
#   # Calculate Index values for each year
#   for(i in 4:max(ncol(RLI_data))){
#     placehold <- RLI_data %>% group_by(year) %>% summarise((Max_value-sum(.[[i]]))/Max_value)
#   }
#   noT7 <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(noT7weight))/Max_value)
#   T7 <- RLI_data %>% group_by(year) %>% summarise((Max_value_RLI - sum(T7weight))/Max_value_RLI)
#   RLICR <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(RLICRweight))/Max_value)
#   RLIEX <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(RLIEXweight))/Max_value)
#   names(PEX) <- c("year", "category")
#   names(RLI) <- c("year", "category")
#   names(CritE) <- c("year", "category")
#   RLI_values <- bind_rows("Bates" = PEX, "RLI" = RLI, "CritE" = CritE, .id = "id")
#   return(RLI_values)
# }

birdCompare <- Calc_weightings_extend(Bird_RLI, weightings = Birds100)
birdCompare$position <- "median"
tempBird <- Calc_weightings_extend(Bird_RLI, weightings = Birds100low)
tempBird$position <- "bottom"
birdCompare <- rbind(birdCompare, tempBird)
tempBird <- Calc_weightings_extend(Bird_RLI, weightings = Birds100high)
tempBird$position <- "top"
birdCompare <- rbind(birdCompare, tempBird)
birdCompare$id <- factor(birdCompare$id, levels = c("RLICR", "RLIEX", "T7", "noT7", "Bates"))
birdCompare<- spread(birdCompare,position, category)

ggplot(data = birdCompare, aes(x = year, y = median, group = id)) + 
  geom_ribbon(aes(ymin=bottom, ymax=top, alpha = 0.3), colour = "grey", fill = "lightgrey", linetype = 2, show.legend = FALSE)+
  geom_line(aes(colour = id), linewidth = 1) + geom_point(aes(colour = id), size = 2) + 
  scale_colour_manual(values = c("darkcyan", "orange","darkred", "green4", "black"), 
                      labels = c("RLI data with CR(PEX) treated as CR", "RLI data with CR(PEX) treated as EX",
                                 "RL data corrected using Table 7", "RL data without corrections", "Overall data with corrections")) + 
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(colour = "Data Source") + labs(y = "Survival Probability (t=100)", x = "Year") +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black")), 
        axis.title.x = element_text(vjust=-1), axis.title.y = element_text(vjust = 3),
        legend.position = "top", legend.text = element_text(size=11), 
        legend.title = element_text(size=12), strip.text = element_text(size=14))+
  guides(colour=guide_legend(nrow=3,byrow=TRUE))

### Collect Slope values for all clade/id combos ###

RLI_slope <- birdCompare %>% group_by(id) %>% summarise(Gradient <- lm(median ~ year)[[1]][2])
names(RLI_slope) <- c("Id", "Gradient")

print(xtable(RLI_slope, digits = 3, display = c("d", "s", "g")), include.rownames=FALSE)

















