
rm(list=ls())

require(dplyr)

## This R file recodes the overalldf produced by summary_data.R to take account of comments made in my meeting with James on 4 April and James' subsequent email of the same date 

df <- read.csv("../Data/Overalldf.csv", header = TRUE) #111122 

## This df will now be recoded with the 27 new categories (see James' thoughts 3 March 2018)

df$new27 <-"" # add new column and set all entries as blank

# First - if a change is based on a previous mistake, the last category is given a true status equal to the current status and the believed status as already known

# so for N changes I need to give the new27 column its current state

df$new27[df$change.status=="N" & df$state==0] <- 0 
df$new27[df$change.status=="N" & df$state==1] <- 2
df$new27[df$change.status=="N" & df$state==2] <- 6
df$new27[df$change.status=="N" & df$state==3] <- 10
df$new27[df$change.status=="N" & df$state==4] <- 14
df$new27[df$change.status=="N" & df$state==5] <- 18
df$new27[df$change.status=="N" & df$state==6] <- 23
df$new27[df$change.status=="N" & df$state==7] <- 26

sum(df$new27!="") # 5165 as expected

# I also now want to give the previous mistaken entry (which it must have been as these are N changes) the same code as above (NB need to ensure no coding between spp.)

for (i in 1:nrow(df)){
  
  if (df[i,]$change.status=="N") {
    if(df[i,]$name==df[i-1,]$name){
    df[i-1, ]$new27 <- df[i,]$new27
    }
  }
}

# initially ran the above code twice or entries with two N changes (one after the other) remain incorrectly coded eg, Acropogon bullatus.  On reflection not going to do that.

#for (i in 1:nrow(df)){
#  
#  if (df[i,]$change.status=="N") {
#    if(df[i,]$name==df[i-1,]$name){
#      df[i-1, ]$new27 <- df[i,]$new27
#    }
#  }
#}


# So now I have a df with N entries with new codes, plus the previous listing has also been re-coded.

sum(df$change.status=="N"&df$new27=="") # 0
sum(df$new27!="") # 10068 (c200 spp have more than one N entry)


# Second - If change was based on a true change then the previous category can be given a true status equal to its believed status based on the logic that it has been re-examined and found to be reasonable, as this is a G change

df$new27[df$change.status=="G" & df$state==0] <- 0 # shouldn't be any(!)
sum(df$new27==0 & df$change.status=="G") # 0 (good!)
df$new27[df$change.status=="G" & df$state==1] <- 2
df$new27[df$change.status=="G" & df$state==2] <- 6
df$new27[df$change.status=="G" & df$state==3] <- 10
df$new27[df$change.status=="G" & df$state==4] <- 14
df$new27[df$change.status=="G" & df$state==5] <- 18
df$new27[df$change.status=="G" & df$state==6] <- 23
df$new27[df$change.status=="G" & df$state==7] <- 26

# the prior entry in the case of a genuine change is believed

for (i in 1:nrow(df)){
  
  
  if (df[i,]$change.status=="G") {
  
    if(df[i,]$name==df[i-1,]$name){
      
    if (df[i-1, ]$state==1){
      df[i-1,]$new27 <-2
    }
    if (df[i-1, ]$state==2){
      df[i-1,]$new27 <-6
    }
    if (df[i-1, ]$state==3){
      df[i-1,]$new27 <-10
    }
    if (df[i-1, ]$state==4){
      df[i-1,]$new27 <-14
    }
    if (df[i-1, ]$state==5){
      df[i-1,]$new27 <-18
    }
    if (df[i-1, ]$state==6){
      df[i-1,]$new27 <-23
    }
    if (df[i-1, ]$state==7){
      df[i-1,]$new27 <-26
    }
  }
  }
}

sum(df$new27!="") # 11481

# Thirdly - If there is no change between a pair statuses, then the first of the two can also be given a true status equal to its believed status on the same logic that it has been looked at twice 

# Also need to be sure the current and previous status are the same!


for (i in 1:nrow(df)){
  
  if (df[i,]$TSFO!=0) {
    
      if (df[i, ]$state==0 & df[i-1, ]$state==0){
        df[i-1,]$new27 <-0
      }
    
      if (df[i, ]$state==1 & df[i-1, ]$state==1){
        df[i-1,]$new27 <-2
      }
      if (df[i, ]$state==2 & df[i-1, ]$state==2){
        df[i-1,]$new27 <-6
      }
      if (df[i, ]$state==3 & df[i-1, ]$state==3){
        df[i-1,]$new27 <-10
      }
      if (df[i, ]$state==4 & df[i-1, ]$state==4){
        df[i-1,]$new27 <-14
      }
      if (df[i, ]$state==5 & df[i-1, ]$state==5){
        df[i-1,]$new27 <-18
      }
      if (df[i, ]$state==6 & df[i-1, ]$state==6){
        df[i-1,]$new27 <-23
      }
      if (df[i, ]$state==7 & df[i-1, ]$state==7){
        df[i-1,]$new27 <-26
      }
    }
  }


sum(df$new27=="") # 28458 uncoded entries


# So the only uncoded entries should now be the last value in any species history and changes between categories where the reason for that change isn't known (so pre 2007)

# To check exactly what remains uncoded:

sum(df$new27=="" & df$result.year < 2007) # 6661 
sum(df$new27=="" & df$result.year >= 2007) # 21797
sum(df$change.status=="U" &df$new27=="") # 28458

latest_yr <-df[!duplicated(df$name, fromLast = TRUE), ] #28110
sum(latest_yr$new27=="") # 24127

# so of the 28110 uncoded entries, 24127 relate to the final year

df <- dplyr::select(df, name, result.year, state, TSFO, change.status, new27)

# Save file

write.csv(df, "../Data/partial_recode.csv", row.names = FALSE)



