rm(list=ls())

# This file carries on from initial_recodedf.R and finishes recoding the overall master df


# First load the required files

df <- read.csv("../Data/partial_recode.csv", header = TRUE) #111122


# so I now want to add another column to df so I can use it to code and randomise where appropriate, the entries currently shown with a new27 state of "".  I don't want to randomise what is currently in the new27 column as this has been "pre-considered"

# I need to give the NAs an entry, say 99, to facilitate this

df$new27[is.na(df$new27)] <-99 
sum(df$new27==99) # 28458 as expected

# add another new column
df$new27addl<- 999

# Firstly, I want to deal with currently uncoded pre2007 listings

# if new27 currently shows 99 I want to copy across the state number, but only where the year is pre2007


sum(df$result.year<2007 & df$new27==99 ) #6661 as expected

#for (i in 1:nrow(df)){
#  
#  if (df[i,]$new27 ==99 & df[i,]$result.year<2007) {
#    df[i, ]$new27addl <- df[i,]$state
#  }
#}

## loop takes ages! So,


df$new27addl[df$state==0 & df$new27==99 & df$result.year<2007] <- 0
df$new27addl[df$state==1 & df$new27==99 & df$result.year<2007] <- 2 
df$new27addl[df$state==2 & df$new27==99 & df$result.year<2007] <- 6
df$new27addl[df$state==3 & df$new27==99 & df$result.year<2007] <- 10
df$new27addl[df$state==4 & df$new27==99 & df$result.year<2007] <- 14
df$new27addl[df$state==5 & df$new27==99 & df$result.year<2007] <- 18
df$new27addl[df$state==6 & df$new27==99 & df$result.year<2007] <- 23
df$new27addl[df$state==7 & df$new27==99 & df$result.year<2007] <- 26

sort(unique(df$new27addl)) # check I have the expected codes
sum(df$new27addl!=999) # 6661 so all copied across

# load from summary_data.R
load("../Data/NGmatrix_full.Rdata")
load("../Data/summary_data.Rdata")

#attempt to estimate what may have been miscategorise pre 2007 based on transitions and using NG changes post 07 to inform these estimates
# using ceiling rather than round to ensure that most possible new states included (eg, 21 would be 0 not 1 using round)

Change1 <-ceiling(NGmatrix_full[2,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbLC
Change3 <-ceiling(NGmatrix_full[2,3]/summary[8,2] * sum(df$new27addl==6)) #NTsbLC
Change4 <-ceiling(NGmatrix_full[3,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbNT
Change5 <-ceiling(NGmatrix_full[3,2]/summary[8,1] * sum(df$new27addl==2)) #LCsbNT
Change7 <-ceiling(NGmatrix_full[3,4]/summary[8,3] * sum(df$new27addl==10)) #VUsbNT
Change8 <-ceiling(NGmatrix_full[4,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbVU
Change9 <-ceiling(NGmatrix_full[4,3]/summary[8,2] * sum(df$new27addl==6))  #NTsbVU
Change11 <-ceiling(NGmatrix_full[4,5]/summary[8,4] * sum(df$new27addl==14)) #ENsbVU
Change12 <-ceiling(NGmatrix_full[5,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbEN
Change13 <-ceiling(NGmatrix_full[5,4]/summary[8,3] * sum(df$new27addl==10)) #VUsbEN
Change15 <-ceiling(NGmatrix_full[5,6]/summary[8,5] * sum(df$new27addl==18)) #CRsbEN
Change16 <-ceiling(NGmatrix_full[6,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbCR
Change17 <-ceiling(NGmatrix_full[6,5]/summary[8,4] * sum(df$new27addl==14)) #ENsbCR
Change19 <-ceiling(NGmatrix_full[6,7]/summary[8,6] * sum(df$new27addl==23)) #EWsbCR
Change20 <-ceiling(NGmatrix_full[6,8]/summary[8,7] * sum(df$new27addl==26)) #EXsbCR
Change21 <-ceiling(NGmatrix_full[7,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbEW 
Change22 <-ceiling(NGmatrix_full[7,6]/summary[8,5] * sum(df$new27addl==18)) #CRsbEW 
Change24 <-ceiling(NGmatrix_full[8,1]/summary[8,8] * sum(df$new27addl==0)) #DDsbEX
Change25 <-ceiling(NGmatrix_full[8,6]/summary[8,5] * sum(df$new27addl==18)) #CRsbEX



Recode1<-as.integer(row.names(df[ sample( which( df$new27addl==0) , Change1) , ])) 
for (i in 1:length(Recode1)){
  df[Recode1[i],]$new27addl <-1}

Recode3 <-as.integer(row.names(df[ sample( which( df$new27addl==6) , Change3) , ]))
for (i in 1:length(Recode3)){
  df[Recode3[i],]$new27addl <-3}

Recode4 <-as.integer(row.names(df[ sample( which( df$new27addl==0) , Change4) , ]))
for (i in 1:length(Recode4)){
  df[Recode4[i],]$new27addl <-4}

Recode5 <-as.integer(row.names(df[ sample( which( df$new27addl==2) , Change5) , ]))
for (i in 1:length(Recode5)){
  df[Recode5[i],]$new27addl <-5}

Recode7 <-as.integer(row.names(df[ sample( which( df$new27addl==10) , Change7) , ]))
for (i in 1:length(Recode7)){
  df[Recode7[i],]$new27addl <-7}

Recode8 <-as.integer(row.names(df[ sample( which( df$new27addl==0) , Change8) , ]))
for (i in 1:length(Recode8)){
  df[Recode8[i],]$new27addl <-8}

Recode9 <-as.integer(row.names(df[ sample( which( df$new27addl==6) , Change9) , ]))
for (i in 1:length(Recode9)){
  df[Recode9[i],]$new27addl <-9}

Recode11 <-as.integer(row.names(df[ sample( which( df$new27addl==14) , Change11) , ]))
for (i in 1:length(Recode11)){
  df[Recode11[i],]$new27addl <-11}

Recode12 <-as.integer(row.names(df[ sample( which( df$new27addl==0) , Change12) , ]))
for (i in 1:length(Recode12)){
  df[Recode12[i],]$new27addl <-12}

Recode13 <-as.integer(row.names(df[ sample( which( df$new27addl==10) , Change13) , ]))
for (i in 1:length(Recode13)){
  df[Recode13[i],]$new27addl <-13}

Recode15 <-as.integer(row.names(df[ sample( which( df$new27addl==18) , Change15) , ]))
for (i in 1:length(Recode15)){
  df[Recode15[i],]$new27addl <-15}

Recode16 <-as.integer(row.names(df[ sample( which( df$new27addl==0) , Change16) , ]))
for (i in 1:length(Recode16)){
  df[Recode16[i],]$new27addl <-16}

Recode17 <-as.integer(row.names(df[ sample( which( df$new27addl==14) , Change17) , ]))
for (i in 1:length(Recode17)){
  df[Recode17[i],]$new27addl <-17}

Recode19 <-as.integer(row.names(df[ sample( which( df$new27addl==23) , Change19) , ]))
for (i in 1:length(Recode19)){
  df[Recode19[i],]$new27addl <-19}

Recode20 <-as.integer(row.names(df[ sample( which( df$new27addl==26) , Change20) , ]))
for (i in 1:length(Recode20)){
  df[Recode20[i],]$new27addl <-20}

Recode21 <-as.integer(row.names(df[ sample( which( df$new27addl==0) ,Change21) , ]))  
for (i in 1:length(Recode21)){
  df[Recode21[i],]$new27addl <-21}

Recode22 <-as.integer(row.names(df[ sample( which( df$new27addl==18) , Change22) , ]))
for (i in 1:length(Recode22)){
  df[Recode22[i],]$new27addl <-22}

Recode24 <-as.integer(row.names(df[ sample( which( df$new27addl==0) ,Change24) , ]))
for (i in 1:length(Recode24)){
  df[Recode24[i],]$new27addl <-24}

Recode25 <-as.integer(row.names(df[ sample( which( df$new27addl==18) , Change25) , ]))
for (i in 1:length(Recode25)){
  df[Recode25[i],]$new27addl <-25}


# check I have all codes
sort(unique(df$new27addl))


# Finally copy over the pre-considered entries (ie those without 99) from new27 into new27addl
# loop way too slow!!

#for (i in 1:nrow(df)){
#  
#  if (df[i,]$new27 !=99) {
#    df[i, ]$new27addl <- df[i,]$new27
#  }
#  
#}


df$new27addl[df$new27==0] <- 0
df$new27addl[df$new27==2] <- 2 
df$new27addl[df$new27==6] <- 6
df$new27addl[df$new27==10] <- 10
df$new27addl[df$new27==14] <- 14
df$new27addl[df$new27==18] <- 18
df$new27addl[df$new27==23] <- 23
df$new27addl[df$new27==26] <- 26


# See how many entries still show 999 and see why

sum(df$new27addl==999) # 21797
sum(df$new27addl==999 & df$result.year<2007) #0
sum(df$new27addl==999 & df$result.year>=2007) #21797


# save the final 27 state df

write.csv(df, "../Data/df_27state.csv", row.names = FALSE)
