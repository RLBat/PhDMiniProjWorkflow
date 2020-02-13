# This R script takes the Master_changes file created at the end of changes.R and adds in the changes which, for some reason, do not appear to have been incorporated within relevant Table 7 data for each year

rm(list=ls())

# first read in the df from wrangling.R

df <- read.csv("../Data/wrangling.csv", header = TRUE) # 111017 obs

# now add a pvsstate column which will enable me to calculate the total numbers of state changes

df$pvsstate <- dplyr::lag(df$state)

# need to make sure where TSFO = 0 we have NA not a number or I am re-coding between spp

df$pvsstate[df$TSFO==0] <- "NA"

#for (i in 1:nrow(df)){
#  if (df[i,]$TSFO == 0) {
#    df[i,]$pvsstate <- "NA"
#  }
#}
# Loop above took ages to run!

# Now I want to filter the df as I am only interested here in post 2007 data where I have Table 7 information and can see what hasn't, for whatever reason, been picked up in these Tables

dfpost07 <- dplyr::filter(df, result.year>=2007) #64418

# Now read in the Master_change file created in changes.R
changes <- read.csv("../Data/Master_changes.csv", header = TRUE) #4518

# remove Master_changes from dfpost07 and see what remains and if there are any unrecorded transitions with changes:
# first remove non-essential rows

changes_remove <- dplyr::select(changes, name, result.year) 

dfpost07_remaining <- dplyr::anti_join(dfpost07, changes_remove) #60627 - so 3791 removed

# now see how many of these are same state transitions:
  
sum(dfpost07_remaining$state==dfpost07_remaining$pvsstate) # 52809
sum(dfpost07_remaining$pvsstate=="NA") #5787

# so accounted for 52809 + 5787 = 58596
# remaining to check = 2033

remaining <- dplyr::filter(dfpost07_remaining, state!=pvsstate) #7818
# don't want NA

remaining <- dplyr::filter(remaining, pvsstate!="NA") #2031
# so there are 2034 transitions which are not on the Table 7 data????

sort(unique(remaining$result.year))

remaining_07 <- dplyr::filter(remaining, result.year==2007) #16
remaining_08 <- dplyr::filter(remaining, result.year==2008) #1602
remaining_09 <- dplyr::filter(remaining, result.year==2009) #23
remaining_10 <- dplyr::filter(remaining, result.year==2010) #120
remaining_11 <- dplyr::filter(remaining, result.year==2011) #35
remaining_12 <- dplyr::filter(remaining, result.year==2012) #53
remaining_13 <- dplyr::filter(remaining, result.year==2013) #8
remaining_14 <- dplyr::filter(remaining, result.year==2014) #41
remaining_15 <- dplyr::filter(remaining, result.year==2015) #25
remaining_16 <- dplyr::filter(remaining, result.year==2016|result.year==2016.5) #55
remaining_17 <- dplyr::filter(remaining, result.year==2017|result.year==2017.5) #53



# most of them are for 2008 and the RedList Table 7 for that year only has genuine changes.
# so let's categorise these as "N"
remaining_08$N.G<-"N"

#write.csv(remaining_08, "../Data/remaining08.csv", row.names = FALSE)

# only 429 remain after that so I will also recode these as "N" changes since it is more likely a genuine change would be picked up in Table7
# some appear to be because pvs listing DD (always N change), sometimes for updated codes etc etc
remaining_07$N.G <-"N"
remaining_09$N.G <-"N"
remaining_10$N.G <-"N"
remaining_11$N.G <-"N"
remaining_12$N.G <-"N"
remaining_13$N.G <-"N"
remaining_14$N.G <-"N"
remaining_15$N.G <-"N"
remaining_16$N.G <-"N"
remaining_17$N.G <-"N"

remaining_not08 <- rbind(remaining_07, remaining_09, remaining_10, remaining_11, remaining_12, remaining_13, remaining_14, remaining_15, remaining_16, remaining_17)

#write.csv(remaining_not08, "../Data/remaining_not08.csv", row.names = FALSE)

# Now save a final Master_changes file for use in Summary_data.R and subsequent files
# first sort out columns and headings so everything can be put into one df

changes <- dplyr::select(changes, name, result.year, state1, state2, N.G)
remaining_08 <- dplyr::select(remaining_08, name, result.year, pvsstate, state, N.G)
colnames(remaining_08) <- c("name", "result.year", "state1", "state2", "N.G")
remaining_not08 <- dplyr::select(remaining_not08, name, result.year, pvsstate, state, N.G)
colnames(remaining_not08) <- c("name", "result.year", "state1", "state2", "N.G")


Master <- rbind(changes, remaining_08, remaining_not08)


write.csv(Master, "../Data/Master_changes_final.csv", row.names = FALSE)
