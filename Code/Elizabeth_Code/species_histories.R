rm(list=ls())

###############################################################################
#This file uses the RedList API to call up listing histories of all species on the lists downloaded and connverted into csv files in species_categories.R
# These lists are then converted into dataframes and csv files for further wrangling 


#import dplyr
require(dplyr)

# First to import the csv files created in species_categories.R and use dplyr to retain only taxon id and scientific name.  Also rename columns

# extinct
extinct <- read.csv("../Data/EX.csv") 
extinct <- dplyr::select(extinct, result.taxonid, result.scientific_name)
colnames(extinct) <-c("taxon_id", "Name")

# extinct in the wild
extinct_wild <- read.csv("../Data/EW.csv") 
extinct_wild <- dplyr::select(extinct_wild, result.taxonid, result.scientific_name)
colnames(extinct_wild) <-c("taxon_id", "Name")

# critical
critical <- read.csv("../Data/CR.csv")  
critical <- dplyr::select(critical, result.taxonid, result.scientific_name)
colnames(critical) <-c("taxon_id", "Name")

# endangered
endangered <- read.csv("../Data/EN.csv") 
endangered <- dplyr::select(endangered, result.taxonid, result.scientific_name)
colnames(endangered) <-c("taxon_id", "Name")

# vulnerable
vulnerable <- read.csv("../Data/VU.csv") 
vulnerable <- dplyr::select(vulnerable, result.taxonid, result.scientific_name)
colnames(vulnerable) <-c("taxon_id", "Name")

# near threatened
near_threatened <- read.csv("../Data/NT.csv") 
near_threatened <- dplyr::select(near_threatened, result.taxonid, result.scientific_name)
colnames(near_threatened) <-c("taxon_id", "Name")

# least concern
least_concern <- read.csv("../Data/LC.csv") 
least_concern <- dplyr::select(least_concern, result.taxonid, result.scientific_name)
colnames(least_concern) <-c("taxon_id", "Name")

# LR/cd
LRcd <- read.csv("../Data/LRcd.csv") 
LRcd <- dplyr::select(LRcd, result.taxonid, result.scientific_name)
colnames(LRcd) <-c("taxon_id", "Name")

# and also old LR/lc and LR/nt to amalgamate into LC and NT above
# LR/nt
LRnt <- read.csv("../Data/LRnt.csv") 
LRnt <- dplyr::select(LRnt, result.taxonid, result.scientific_name)
colnames(LRnt) <-c("taxon_id", "Name")

# LR/lc
LRlc <- read.csv("../Data/LRlc.csv") 
LRlc <- dplyr::select(LRlc, result.taxonid, result.scientific_name)
colnames(LRlc) <-c("taxon_id", "Name")

# Also include DD following JR email 3 March

DD <- read.csv("../Data/DD.csv") 
DD <- dplyr::select(DD, result.taxonid, result.scientific_name)
colnames(DD) <-c("taxon_id", "Name")


# now use RedList API to call histories of all listings
# import rredlist
library(rredlist)

RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"

# create lists - all hashed out once csv files have been saved with full dataframes

#extinct_APIList <-as.character(extinct$Name) # Run on 11 Feb 18
# sublists or RedList API unhappy and stops it working!
# Turns out the problem (resolved by running as follows) was caused by a / character - so ignores that one.  If its a major issue and many are excluded I will consider a separate list using their ids rather than names

#extinct_APIList1 <-extinct_APIList[1:100]
#extinct_APIList2 <-extinct_APIList[101:200]
#extinct_APIList3 <-extinct_APIList[201:300]
#extinct_APIList4 <-extinct_APIList[301:400]
#extinct_APIList5 <-extinct_APIList[401:500]
#extinct_APIList6a <-extinct_APIList[501:550]
#extinct_APIList6b <-extinct_APIList[551:575]
#extinct_APIList6c <-extinct_APIList[576:582]
#extinct_APIList6d <- extinct_APIList[588:600]
#extinct_APIList6e <- extinct_APIList[583]
#extinct_APIList6f <- extinct_APIList[586:587]
#extinct_APIList7 <-extinct_APIList[601:700]
#extinct_APIList8 <-extinct_APIList[701:800]
#extinct_APIList9 <-extinct_APIList[801:902]

#extinct_wild_APIList <- as.character(extinct_wild$Name) # Run on 11 Feb 18

#critical_APIList <- as.character(critical$Name) # Run 11 Feb 18
# Breaking into 1000+ long chunks to avoid a problem with API calls
# Noting here that #3749 contains a / character so will need to be excluded
# As I am breaking this list into chunks in any event I haven't bothered to specifically exclude from the list

#critical_APIList1 <- critical_APIList[1:1000]
#critical_APIList2 <- critical_APIList[1001:2000]
#critical_APIList3 <- critical_APIList[2001:3748]
#critical_APIList4 <- critical_APIList[3750:5892]


#endangered_APIList <- as.character(endangered$Name) 
# Being broken into parts to exclude #5697 which contains a / character and to put slightly shorter lists through the process
#endangered_APIList1 <- endangered_APIList[1:5696] # Run 14 Feb 2018
#endangered_APIList2 <- endangered_APIList[5698:8842] # Run 14 Feb 2018


#vulnerable_APIList <- as.character(vulnerable$Name)
# Being broken into parts to exclude #1042 and #9695 which contain a / character and to put slightly shorter lists through the process
#vulnerable_APIList1 <- vulnerable_APIList[1:1041] # Run 13 Feb 2018
#vulnerable_APIList2 <- vulnerable_APIList[1043:3000] # Run 13 Feb 2018
#vulnerable_APIList3 <- vulnerable_APIList[3001:5000] # Run 13 Feb 2018
#vulnerable_APIList4 <- vulnerable_APIList[5001:7000] # Run 13 Feb 2018
#vulnerable_APIList5 <- vulnerable_APIList[7001:8000] # Run 14 Feb 2018
#vulnerable_APIList6 <- vulnerable_APIList[8001:9000] # Run 14 Feb 2018
#vulnerable_APIList7 <- vulnerable_APIList[9001:9694] # Run 14 Feb 2018
#vulnerable_APIList8 <- vulnerable_APIList[9696:12418] # Run 14 Feb 2018


#near_threatened_APIList <- as.character(near_threatened$Name) # Run 12 Feb 2018
# Being broken into parts to exclude #3381 which contains a / character and to put slightly shorter lists through the process
#near_threatened_APIList1 <- near_threatened_APIList[1:2000]
#near_threatened_APIList2 <- near_threatened_APIList[2001:3380]
#near_threatened_APIList3 <- near_threatened_APIList[3382:5238]

#LRnt_APIList <- as.character(LRnt$Name) # Run 12 Feb 2018
#LRcd_APIList <- as.character(LRcd$Name) # Run 12 Feb 2018
#least_concern_APIList <- as.character(least_concern$Name)
#LC1 <- least_concern_APIList[1:5000] # run 14 Feb 18
#LC2 <- least_concern_APIList[5001:10000] # run 15 Feb 18
#LC3 <- least_concern_APIList[10001:15000] # run 15 Feb 18
#LC7 <- least_concern_APIList[28803:28809] # run 15 Feb 18
#LC8 <- least_concern_APIList[28811] # run 15 Feb 18

# The following were run on my Mac and then transferred over later:
#LC1Mac <- least_concern_APIList[15001:16000] # Run 17 Feb 18
#LC2Mac <- least_concern_APIList[16001:17000] # Run 17 Feb 18
#LC3Mac <- least_concern_APIList[17001:18000] # Run 17 Feb 18
#LC4Mac <- least_concern_APIList[18001:19000] # Run 17 Feb 18
#LC5Mac <- least_concern_APIList[19001:20000] # Run 17 Feb 18
#LC6Mac <- least_concern_APIList[20001:21000] # Run 17 Feb 18
#LC7Mac <- least_concern_APIList[21001:24000] # Run 17 Feb 18
#LC8Mac <- least_concern_APIList[24001:28801] # Run 17 Feb 18
#LC9Mac <- least_concern_APIList[28813:31000] # Run 17 Feb 18
#LC10Mac <- least_concern_APIList[31001:35000] # Run 18 Feb 18
#LC11Mac <- least_concern_APIList[35001:36000] # Run 18 Feb 18
#LC11aMac <- least_concern_APIList[36001:37500] # Run 18 Feb 18
#LC11bMac <- least_concern_APIList[37501:40000] # Run 18 Feb 18
#LC11cMac <- least_concern_APIList[40001:40500] # Run 19 Feb 18
#LC11dMac <- least_concern_APIList[40501:41000] # Run 19 Feb 18
#LC11eMac <- least_concern_APIList[41001:42000] # Run 19 Feb 18
#LC12Mac <- least_concern_APIList[42001:43971] # Run 19 Feb 18


# Breaking out into sublists as error running entire list (no apparant reason why (have certainly run much longer lists)- busy time for a large run on the server?)
#LRlc_APIList <- as.character(LRlc$Name) # Run 12 Feb 2018
#LRlc_APIList1 <- LRlc_APIList[1:250]
#LRlc_APIList2 <- LRlc_APIList[251:500]
#LRlc_APIList3 <- LRlc_APIList[501:729]

#DD_APIList <- as.character(DD$Name) # 8spp with / character and 1 with "" which add a \ in the as.character list - so odd lists!
#DD_APIList1a <- DD_APIList[1:498]# run 3 March 18
#DD_APIList1b <- DD_APIList[500:4500] # run 3 March 18
#DD_APIList2 <- DD_APIList[4501:9574] # run 4 March 18
#DD_APIList3 <- DD_APIList[9576:9588] # run 3 March 18
#DD_APIList4 <- DD_APIList[9590:9591] # run 3 March 18 
#DD_APIList5 <- DD_APIList[9593:9594] # run 3 March 18
#DD_APIList6 <- DD_APIList[9600:14661] # run 4 March 18


# using lapply rather than a loop here (quicker) also this doesn't fall foul of the "two seconds between calls" which means I am not penalised for making repeated calls of the Red List API

# following (example) few commands hashed out to avoid making repeated calls to the RedList API when sourcing file!


#DD6_histories <- lapply(1:length(DD_APIList6), function(i){
#  species_history <- rl_history(name=DD_APIList6[i], key=RL_token)
#})

# extinct removed but followed the same format - spent a lot of time trying to work out what was stopping it working by breaking listing into parts (a-f) - transpires it was the / character so need to remove these from name listings prior to running code
#extinct_wild_histories <- lapply(1:length(extinct_wild_APIList), function(i){
#  species_history <- rl_history(name=extinct_wild_APIList[i], key=RL_token)
#})

# need to remove those species where result within list is list() - ie there is no historic listing information.  Following function removes them quickly prior to conversion to df - it has otherwise proved impossible despite many attempts to convert to a df
  
listing <- function(any_list){
  
  species_listing <-c()
  
  for (i in 1:length(any_list)){
    
    if((length(any_list[[i]]$result)) > 0) {
      species_listing <- c(species_listing, any_list[i])
    }
    
    }
  
  return(species_listing)
}

# Example command
#LC1_edited <- listing(LC1_histories)
#DD3_edited <- listing(DD3_histories)
#DD4_edited <- listing(DD4_histories)
#DD5_edited <- listing(DD5_histories)
#DD1a_edited <- listing(DD1a_histories)
#DD1b_edited <- listing(DD1b_histories)
##DD1c_edited <- listing(DD1c_histories)
##DD1d_edited <- listing(DD1d_histories)
#DD2_edited <- listing(DD2_histories)
#DD6_edited <- listing(DD6_histories)

#  now take the remaining list of lists and turn turn into a dataframe
df_creation <- function(a_list){
  df <- data.frame()
  for (i in 1:length(a_list)){
    aaa <- as.data.frame(a_list[i])
    df <- rbind(df, aaa)
  }
  return(df)
}

# Example command

#df_LC1 <- df_creation(LC1_edited)
#df_DD3 <- df_creation(DD3_edited)
#df_DD4 <- df_creation(DD4_edited)
#df_DD5 <- df_creation(DD5_edited)
#df_DD1a <- df_creation(DD1a_edited)
#df_DD1b <- df_creation(DD1b_edited)
##df_DD1c <- df_creation(DD1c_edited)
##df_DD1d <- df_creation(DD1d_edited)
#df_DD2 <- df_creation(DD2_edited)
#df_DD6 <- df_creation(DD6_edited)

# Save files (rbind was used where df_creation run in sections to create dfs)
#df_DD_histories <- rbind(df_DD1a, df_DD1b, df_DD2, df_DD3, df_DD4, df_DD5, df_DD6)

#write.csv(df_DD_histories, "../Data/DDHistories.csv", row.names = FALSE)
#write.csv(df_extinct_histories, "../Data/EXHistories.csv", row.names = FALSE)
#write.csv(df_extinct_wild_histories_edited, "../Data/EWHistories.csv", row.names = FALSE)
#write.csv(df_critical_histories, "../Data/CRHistories.csv", row.names=FALSE)
#write.csv(df_LRcd_histories, "../Data/LRcdHistories.csv", row.names = FALSE)
#write.csv(df_LRlc_histories, "../Data/LRlcHistories.csv", row.names = FALSE)
#write.csv(df_LRnt_histories, "../Data/LRntHistories.csv", row.names = FALSE)
#write.csv(df_near_threatened_histories, "../Data/NTHistories.csv", row.names = FALSE)
#write.csv(df_vulnerable_histories, "../Data/VUHistories.csv", row.names=FALSE)
#write.csv(df_endangered_histories, "../Data/ENHistories.csv", row.names=FALSE)
#write.csv(df_LC_historiesPARTIAL, "../Data/LCHistoriesPARTIAL.csv", row.names = FALSE)

# sort out LC as I ended up having to do most of that on my Mac

#LC_HP <- read.csv("../Data/LCHistoriesPARTIAL.csv")
#LC_Mac <- read.csv("../Data/LCHistoriesMac.csv")

#df_LCHistories <- rbind(LC_HP, LC_Mac)
# now write to csv file
#write.csv(df_LCHistories, "../Data/LCHistories.csv", row.names = FALSE)
