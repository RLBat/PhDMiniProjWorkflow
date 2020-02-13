rm(list=ls())

require(dplyr)
# This file carries on from second_recodedf.R and finishes recoding the overall master df and readying it for msm modelling 
#############################################################################################
### In this file, the final listings are recoded and included, and these are then subject to randomisation to try to take account of errors in listings
##############################################################################################
##This file has been run  XXX  times, each time outputting a slightly different dataframe

## Nb some small alterations to earlier scripts codes to make it run more efficiently, eg in the case of "back to life" recoding below

# First load the required files 


df <- read.csv("../Data/df_27state.csv", header = TRUE) #111122 obs

# See how many entries still show 999, ie uncoded entries

#sum(df$new27addl==999) # 21797  As noted in the other two files referenced above, 1 of these relate to final year listing for individual species

# First add a new column so I don't overwrite or randomise already considered entries:

df$new <- 55

# now "populate", where appropriate, with given states and recode into new 27 state numbers:
df$new[df$state==0 & df$new27addl==999] <- 0
df$new[df$state==1 & df$new27addl==999] <- 2
df$new[df$state==2 & df$new27addl==999] <- 6 
df$new[df$state==3 & df$new27addl==999] <- 10
df$new[df$state==4 & df$new27addl==999] <- 14
df$new[df$state==5 & df$new27addl==999] <- 18
df$new[df$state==6 & df$new27addl==999] <- 23
df$new[df$state==7 & df$new27addl==999] <- 26

# These codes are now subject to "randomisation" to try to take account of listings that may be in error

# so what % of these should be subject to randomisation on the assumption that they may be mis-recorded?

# All the above are post 2007, and from summary_data.R I know that I have the following listings (file from summary_data.R)


#load("../Data/summary_data.Rdata")
# also load NGmatrix_full from Summary_data.R
#load("../Data/NGmatrix_full.Rdata")


# so what percentage of each category above may be miscalculated? Use the same calculation as in summary_data.R but apply it here to the totals above.  This will give numbers to be randomised
## using ceiling rather than round to ensure that most possible new states included (eg, 21 would be 0 not 1 using round)


Change1 <-213 #DDsbLC
Change3 <-196 #NTsbLC
Change4 <-43 #DDsbNT
Change5 <-80 #LCsbNT
Change7 <-107 #VUsbNT
Change8 <-65 #DDsbVU
Change9 <-63 #NTsbVU
Change11 <-92 #ENsbVU
Change12 <-75 #DDsbEN
Change13 <-133 #VUsbEN
Change15 <-70 #CRsbEN
Change16 <-44 #DDsbCR
Change17 <-61 #ENsbCR
Change19 <-4 #EWsbCR
Change20 <-15 #EXsbCR
Change21 <-1 #DDsbEW
Change22 <-1 #CRsbEW
Change24 <-1 #DDsbEX
Change25 <-6 #CRsbEX

# now use the figures above to randomly recode some of the df$new column

Recode1<-as.integer(row.names(df[ sample( which( df$new==0) , Change1) , ]))
for (i in 1:length(Recode1)){
  df[Recode1[i],]$new <-1}

Recode3 <-as.integer(row.names(df[ sample( which( df$new==6) , Change3) , ]))
for (i in 1:length(Recode3)){
  df[Recode3[i],]$new <-3}

Recode4 <-as.integer(row.names(df[ sample( which( df$new==0) , Change4) , ]))
for (i in 1:length(Recode4)){
  df[Recode4[i],]$new <-4}

Recode5 <-as.integer(row.names(df[ sample( which( df$new==2) , Change5) , ]))
for (i in 1:length(Recode5)){
  df[Recode5[i],]$new <-5}

Recode7 <-as.integer(row.names(df[ sample( which( df$new==10) , Change7) , ]))
for (i in 1:length(Recode7)){
  df[Recode7[i],]$new <-7}

Recode8 <-as.integer(row.names(df[ sample( which( df$new==0) , Change8) , ]))
for (i in 1:length(Recode8)){
  df[Recode8[i],]$new <-8}

Recode9 <-as.integer(row.names(df[ sample( which( df$new==6) , Change9) , ]))
for (i in 1:length(Recode9)){
  df[Recode9[i],]$new <-9}

Recode11 <-as.integer(row.names(df[ sample( which( df$new==14) , Change11) , ]))
for (i in 1:length(Recode11)){
  df[Recode11[i],]$new <-11}

Recode12 <-as.integer(row.names(df[ sample( which( df$new==0) , Change12) , ]))
for (i in 1:length(Recode12)){
  df[Recode12[i],]$new <-12}

Recode13 <-as.integer(row.names(df[ sample( which( df$new==10) , Change13) , ]))
for (i in 1:length(Recode13)){
  df[Recode13[i],]$new <-13}

Recode15 <-as.integer(row.names(df[ sample( which( df$new==18) , Change15) , ]))
for (i in 1:length(Recode15)){
  df[Recode15[i],]$new <-15}

Recode16 <-as.integer(row.names(df[ sample( which( df$new==0) , Change16) , ]))
for (i in 1:length(Recode16)){
  df[Recode16[i],]$new <-16}

Recode17 <-as.integer(row.names(df[ sample( which( df$new==14) , Change17) , ]))
for (i in 1:length(Recode17)){
  df[Recode17[i],]$new <-17}

Recode19 <-as.integer(row.names(df[ sample( which( df$new==23) , Change19) , ]))
for (i in 1:length(Recode19)){
  df[Recode19[i],]$new <-19}

Recode20 <-as.integer(row.names(df[ sample( which( df$new==26) , Change20) , ]))
for (i in 1:length(Recode20)){
  df[Recode20[i],]$new <-20}

Recode21 <-as.integer(row.names(df[ sample( which( df$new==0) ,Change21) , ]))
for (i in 1:length(Recode21)){
  df[Recode21[i],]$new <-21}

Recode22 <-as.integer(row.names(df[ sample( which( df$new==18) , Change22) , ]))
for (i in 1:length(Recode22)){
  df[Recode22[i],]$new <-22}

Recode24 <-as.integer(row.names(df[ sample( which( df$new==0) ,Change24) , ])) 
for (i in 1:length(Recode24)){
  df[Recode24[i],]$new <-24}

Recode25 <-as.integer(row.names(df[ sample( which( df$new==18) , Change25) , ]))
for (i in 1:length(Recode25)){
  df[Recode25[i],]$new <-25}

sort(unique(df$new)) 

# now where df$new==55, copy over all those from new27addl
# loop very slow so d/w as follows:


df$new[df$new==55 & df$new27addl==0] <- 0
df$new[df$new==55 & df$new27addl==1] <- 1
df$new[df$new==55 & df$new27addl==2] <- 2
df$new[df$new==55 & df$new27addl==3] <- 3
df$new[df$new==55 & df$new27addl==4] <- 4
df$new[df$new==55 & df$new27addl==5] <- 5
df$new[df$new==55 & df$new27addl==6] <- 6
df$new[df$new==55 & df$new27addl==7] <- 7
df$new[df$new==55 & df$new27addl==8] <- 8
df$new[df$new==55 & df$new27addl==9] <- 9
df$new[df$new==55 & df$new27addl==10] <- 10
df$new[df$new==55 & df$new27addl==11] <- 11
df$new[df$new==55 & df$new27addl==12] <- 12
df$new[df$new==55 & df$new27addl==13] <- 13
df$new[df$new==55 & df$new27addl==14] <- 14
df$new[df$new==55 & df$new27addl==15] <- 15
df$new[df$new==55 & df$new27addl==16] <- 16
df$new[df$new==55 & df$new27addl==17] <- 17
df$new[df$new==55 & df$new27addl==18] <- 18
df$new[df$new==55 & df$new27addl==19] <- 19
df$new[df$new==55 & df$new27addl==20] <- 20
df$new[df$new==55 & df$new27addl==21] <- 21
df$new[df$new==55 & df$new27addl==22] <- 22
df$new[df$new==55 & df$new27addl==23] <- 23
df$new[df$new==55 & df$new27addl==24] <- 24
df$new[df$new==55 & df$new27addl==25] <- 25
df$new[df$new==55 & df$new27addl==26] <- 26



#for (i in 1:nrow(df)){
#  
#  if (df[i,]$new==55) {
#    df[i, ]$new <- df[i,]$new27addl
#  }
#}

sum(df$new==55) # 0
sum(df$new==999) # 0

require(msm)
#require(dplyr)
# Now to prepare the 27 state df for msm modelling

# Now add a new column for msm to flatten the 27 states back to states 0 to 7 
# The underlying msm model specifies that rather than (for example) jumping from state 1 to 3, state 2 must have been passed through unobserved.  It is not possible to have passed through VUsbNT however, so a final recategorisation is required 


df$recode <-0 # for checking purposes
df$recode[df$new==1|df$new==2|df$new==3] <- 1
df$recode[df$new==4|df$new==5|df$new==6|df$new==7] <- 2
df$recode[df$new==8|df$new==9|df$new==10|df$new==11] <- 3
df$recode[df$new==12|df$new==13|df$new==14|df$new==15] <- 4
df$recode[df$new==16|df$new==17|df$new==18|df$new27==19|df$new==20] <- 5
df$recode[df$new==21|df$new==22|df$new==23] <- 6
df$recode[df$new==24|df$new==25|df$new==26] <- 7

# now exclude readings of 0, ie DD

df <- dplyr::filter(df, recode != 0) # leaves 106827 entries

# Now remove any species where there remins only one listing

df <- df %>% group_by(name) %>% filter(n()>1) #  106077 obs

# make sure the TSFO column is correct after these removals

df <- df %>%
  group_by(name) %>%
  arrange(name) %>%
  mutate(TSFO = result.year - first(result.year))



statetable.msm(recode, name, data=df)

# The msm statetable shows many moves out of extinct into other states.  
# As in initial_recode moves from extinct to other states are dealt with on the same basis as the "N"  ie both the current and previous listing are recoded based on the new category using numbers for the states above (The original “mistaken” of 7 effectively ignored).


# First add a new column to the df and copy across any 7s from msm

df$ex <-0
df$ex[df$recode==7] <-7



# If the 7 in recode is followed by a number which isn't 7 (comes back to life) recode the 7 as the next entry as the previous entry (ie the 7) must have been in error - since the spp must be in existance.  TSFO cannot be 0 as you would otherwise recode between species

for (i in 1:nrow(df)){
  
  if (df[i, ]$ex==7 & df[i+1, ]$recode!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$ex <- df[i+1, ]$recode
  }
  
}

# then the following sorts out where we have had 5, 7, 7, 5 for example

for (i in 1:nrow(df)){
  
  if (df[i, ]$ex==7 & df[i+1, ]$ex!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$ex <- df[i+1, ]$ex
  }
  
} 

# Now copy across recode and ex to a new msm column:

df$msm <- ""
df$msm[df$ex==0 & df$recode==1] <- 1
df$msm[df$ex==0 & df$recode==2] <- 2
df$msm[df$ex==0 & df$recode==3] <- 3
df$msm[df$ex==0 & df$recode==4] <- 4
df$msm[df$ex==0 & df$recode==5] <- 5
df$msm[df$ex==0 & df$recode==6] <- 6
df$msm[df$ex==0 & df$recode==7] <- 7
df$msm[df$ex==1] <- 1
df$msm[df$ex==2] <- 2
df$msm[df$ex==3] <- 3
df$msm[df$ex==4] <- 4
df$msm[df$ex==5] <- 5
df$msm[df$ex==6] <- 6
df$msm[df$ex==7] <- 7


#for (i in 1:nrow(df)){
#  
#  if (df[i,]$msm_ex ==0) {
#    
#    df[i,]$msm_ex <- df[i,]$msm
#    
#  }
#}

statetable.msm(msm, name, data=df)

# There will remain a small number of oddities so 

# first run through
for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$msm <- df[i+1, ]$msm
  }
  
}

statetable.msm(msm, name, data=df)

# second run through
for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$msm <- df[i+1, ]$msm
  }
  
}
# check statetable and run again if necessary
statetable.msm(msm, name, data=df)

# third run through
for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$msm <- df[i+1, ]$msm
  }
  
}
# check statetable and run again if necessary
statetable.msm(msm, name, data=df)

# fourth run through
for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$msm <- df[i+1, ]$msm
  }
  
}
# check statetable and run again if necessary
statetable.msm(msm, name, data=df)

# fifth run through
for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm!=7 & df[i+1,]$TSFO != 0){
    
    df[i,]$msm <- df[i+1, ]$msm
  }
  
}
# check statetable and run again if necessary
statetable.msm(msm, name, data=df)


# Now there are a number of histories which are just listings of 7s.  The following will remove the last entry of these types of listings (and the R script then goes on to remove any species with only one listing remaining).  It also has the benefit in removing some of the "trailing 7s". ie where a spp becomes extinct and then continues to be given extinct listings.

for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm==7 & df[i+1,]$TSFO != 0){
    
    df[i+1, ]$msm <- 8
  }
  
}

dflose <- dplyr::filter(df, msm==8) 

df <- dplyr::anti_join(df, dflose) 

# also need to remove any species which are now left with only one entry

df <- df %>% group_by(name) %>% filter(n()>1) # 105080 obs

# now look at the data using a statetable in msm

statetable.msm(msm, name, data=df)
# 7 to 7 transitions remain

# Run the above piece of code again to remove more of the trailing 7s

for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm==7 & df[i+1,]$TSFO != 0){
    
    df[i+1, ]$msm <- 8
  }
  
}

dflose2 <- dplyr::filter(df, msm==8) 

df <- dplyr::anti_join(df, dflose2) 

df <- df %>% group_by(name) %>% filter(n()>1) #104714

# check statetable again

statetable.msm(msm, name, data=df)
# remove final trailing 7s

for (i in 1:nrow(df)){
  
  if (df[i, ]$msm==7 & df[i+1, ]$msm==7 & df[i+1,]$TSFO != 0){
    
    df[i+1, ]$msm <- 8
  }
  
}

dflose3 <- dplyr::filter(df, msm==8)

df <- dplyr::anti_join(df, dflose3)

df <- df %>% group_by(name) %>% filter(n()>1) #104493

# check statetable again

statetable.msm(msm, name, data=df)

# all fine

# make sure the TSFO column is correct after these removals

df <- df %>%
  group_by(name) %>%
  arrange(name) %>%
  mutate(TSFO = result.year - first(result.year))


# save the file for running the msm model

#write.csv(df, "../Data/msm11.csv", row.names = FALSE)


# Also now want a copy of this file with EW recategorised as CR to simplify the final msm model


df$msm[df$msm==6] <- 5
# need to save as csv file first and then load and save as Rdata or future scripts don'r run properly
write.csv(df, "../Manydf/msm_noEW100.csv", row.names=FALSE)
