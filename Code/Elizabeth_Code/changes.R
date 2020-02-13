rm(list=ls())
require(dplyr)

# pdf files downloaded from the Red List on 8 March 2018 and converted from pdf to csv format using Zamzar

# I'm not going to tidy these files extensively as it isn't necessary. What I do want to do is to look at the numbers of changes
# between categories for each year and note whether they are genuine or non-genuine changes

# so I can write a loop that will add columns with numbers for each status listing, I need to make sure that "before", 
# "after" and "n/g" columns are apropriately labelled in each df.  I am not worried about removing useless rows etc as they won't
# be bothered with in the recoding loop so for all intents and purposes are ignored

# I have to deal with each year individually as they are all in slightly different formats

# read in csv
changes2017 <- read.csv("../Data/2017_3_RL_Stats_Table_7.csv", header=TRUE, skip=18)
# select the colimns I am interested in
ch2017 <- dplyr::select(changes2017, MAMMALS, X.1, X.2, X.3, X.4)
# appropriately name these columns so that one loop will work for all datasets
colnames(ch2017) <- c("name", "before", "after", "N/G", "year")

# Now to do the same for all previous years, noting that columns are not always given the same names initially
changes2016 <- read.csv("../Data/2016_3_RL_Stats_Table_7.csv", header=TRUE, skip=18)
ch2016 <- dplyr::select(changes2016, MAMMALS, X.1, X.2, X.3, X.4)
colnames(ch2016) <- c("name", "before", "after", "N/G", "year")

changes2015 <- read.csv("../Data/2015_4_RL_Stats_Table_7.csv", header=TRUE, skip=18)
ch2015 <- dplyr::select(changes2015, MAMMALS, X.1, X.2, X.3, X.4)
colnames(ch2015) <- c("name", "before", "after", "N/G", "year")

changes2014 <- read.csv("../Data/2014_3_RL_Stats_Table_7.csv", header=TRUE, skip=18)
ch2014 <- dplyr::select(changes2014, MAMMALS, X.2, X.3, X.4, X.5)
colnames(ch2014) <- c("name", "before", "after", "N/G", "year")

changes2013 <- read.csv("../Data/2013_2_RL_Stats_Table7_edited.csv", header=TRUE, skip=17)
ch2013 <- dplyr::select(changes2013, MAMMALS, X.2, X.3, X.4, X.5)
colnames(ch2013) <- c("name", "before", "after", "N/G", "year")

changes2012 <- read.csv("../Data/2012_2_RL_Stats_Table_7.csv", header=TRUE, skip=16)
ch2012 <- dplyr::select(changes2012, MAMMALS, X.2, X.3, X.4, X.5)
colnames(ch2012) <- c("name", "before", "after", "N/G", "year")

changes2011 <- read.csv("../Data/2011_2_RL_Stats_Table7.csv", header=TRUE, skip=16)
ch2011 <- dplyr::select(changes2011, MAMMALS, X.2, X.3, X.4, X.5)
colnames(ch2011) <- c("name", "before", "after", "N/G", "year")

changes2010 <- read.csv("../Data/2010_4RL_Stats_Table_7.csv", header=TRUE, skip=19)
ch2010 <- dplyr::select(changes2010, MAMMALS, X.1, X.2, X.3, X.4)
colnames(ch2010) <- c("name", "before", "after", "N/G", "year")

changes2009 <- read.csv("../Data/2009RL_Stats_Table_7.csv", header=TRUE, skip=16)
changes2009$year <- "2009" # year is split between X.4 and X.5 which causes problems with "NA"s  - adding a new year column here to take account of this
ch2009 <- dplyr::select(changes2009, MAMMALS, X.1, X.2, X.3, year)
colnames(ch2009) <- c("name", "before", "after", "N/G", "year")

changes2008 <- read.csv("../Data/2008RL_Stats_Table_7.csv", header=TRUE, skip=14)
# all changes in 2008 are genuine so I need to add a "G" column and a year column(!)
changes2008$type <- "G"
changes2008$year <- "2008"
ch2008 <- dplyr::select(changes2008, Genuine.Improvements, X.1, X.2,type, year)
colnames(ch2008) <- c("name", "before", "after", "N/G", "year")

changes2007 <- read.csv("../Data/2007RL_Stats_Table_7.csv", header=TRUE, skip=15)
ch2007 <- dplyr::select(changes2007, MAMMALS, X.1, X.2, X.3, X.4)
colnames(ch2007) <- c("name", "before", "after", "N/G", "year")

# Now to add two columns to each dataset so that the before and after categories are given a numerical code

# I have used the same numbers as in my other project files.  This series of dfs also includes a CR(PE) listing.  I have coded this as 5 for now as very few

recode <- function(df){
  
  output <-df[NULL, ]
  
  df$state1 <- 0
  df$state2 <- 0
  
  for (i in (1:nrow(df))){
    if((df$before[i])=="LC"|(df$before[i])=="LR/lc"){
    df$state1[i] <- 1
    }
    if ((df$before[i])=="NT"|(df$before[i])=="LR/nt"|(df$before[i])=="LR/cd"){
      df$state1[i] <- 2
    }
    if((df$before[i])=="VU"){
      df$state1[i] <- 3
    }
    if((df$before[i])=="EN"){
      df$state1[i] <- 4
    }
    if((df$before[i])=="CR"|(df$before[i])=="CR (PE)"|(df$before[i])=="CR(PE)"){
      df$state1[i] <- 5
    }
    if((df$before[i])=="EW"){
      df$state1[i] <- 6
    }
    if((df$before[i])=="EX"){
      df$state1[i] <- 7
    }
    if((df$after[i])=="LC"|(df$after[i])=="LR/lc"){
      df$state2[i] <- 1
    }
    if ((df$after[i])=="NT"|(df$after[i])=="LR/nt"|(df$after[i])=="LR/cd"){
      df$state2[i] <- 2
    }
    if((df$after[i])=="VU"){
      df$state2[i] <- 3
    }
    if((df$after[i])=="EN"){
      df$state2[i] <- 4
    }
    if((df$after[i])=="CR"|(df$after[i])=="CR (PE)"|(df$after[i])=="CR(PE)"){
      df$state2[i] <- 5
    }
    if((df$after[i])=="EW"){
      df$state2[i] <- 6
    }
    if((df$after[i])=="EX"){
      df$state2[i] <- 7
    }
      
    
    output <- rbind(output, df[i,])
  }
  
 return(output)
}

recoded2017 <- recode(ch2017)
recoded2016 <- recode(ch2016)
recoded2015 <- recode(ch2015)
recoded2014 <- recode(ch2014)
recoded2013 <- recode(ch2013)
recoded2012 <- recode(ch2012)
recoded2011 <- recode(ch2011)
recoded2010 <- recode(ch2010)
recoded2009 <- recode(ch2009)
recoded2008 <- recode(ch2008)
recoded2007 <- recode(ch2007)


# Bind into one dataset - then remove rows which show before and after state as zeros as these will just be non-informative "information" rows (as I created these columns as columns of zeros prior to recoding them)
# Also remove those with an "after" state recoeded as "NR". There are 11 spp from 2007 which have synonyms elsewhere so can be removed (DD to NR removed via rr not rr2)


Master <- rbind(recoded2007, recoded2008, recoded2009, recoded2010, recoded2011, recoded2012, recoded2013, recoded2014, recoded2015, recoded2016, recoded2017)

rr <-dplyr::filter(Master, Master$state1==0 & Master$state2==0)
Master <- dplyr::anti_join(Master, rr)
rr2 <- dplyr::filter(Master, Master$after=="NR")
Master <- dplyr::anti_join(Master, rr2)
# There are 20 errors categorised as "E" in 2014.  I am going to treat these as "N" and recode (really too few to be concerned with)
Master$`N/G`[Master$`N/G`=="E"] <- "N"

# Finally, in subsequent scripts I will need to refer to the years, and so far there are many years which are shown as, for example, 2015.1:

sort(unique(Master$year))
# These need to be recoded as follows:

Master$result.year[Master$year==2007] <- 2007
Master$result.year[Master$year==2008] <- 2008
Master$result.year[Master$year==2009] <- 2009
Master$result.year[Master$year==2010.1|Master$year==2010.2|Master$year==2010.3|Master$year==2010.4] <- 2010
Master$result.year[Master$year==2011.1|Master$year==2011.2] <- 2011
Master$result.year[Master$year==2012.1|Master$year==2012.2] <- 2012
Master$result.year[Master$year==2013.1|Master$year==2013.2] <- 2013
Master$result.year[Master$year==2014.1|Master$year==2014.2|Master$year==2014.3] <- 2014
Master$result.year[Master$year=="2015-1"|Master$year=="2015-2"|Master$year=="2015-4"] <- 2015
Master$result.year[Master$year=="2016-1"|Master$year=="2016-2"|Master$year=="2016-3"] <- 2016
Master$result.year[Master$year=="2017-1"|Master$year=="2017-2"|Master$year=="2017-3"] <- 2017

# Prior to saving, check for any duplicates:

duplicates <- Master %>%
  group_by(name) %>%
  filter(duplicated(result.year))


# remove the dulpicates

Master <- distinct(Master)

# Master <- dplyr::anti_join(Master, duplicates) # No this takes out both entries

# Save for use in summary_data.R

write.csv(Master, "../Data/Master_changes.csv", row.names = FALSE)

