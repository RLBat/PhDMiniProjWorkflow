rm(list = ls())

require(dplyr)

# For total numbers of species currently in each category, I can use the files from my Rscript RedListAPI.R as follows:

Red_List_Totals <- cbind(c (length(unique(read.csv("../Data/LC.csv"))$result.scientific_name) + length(unique(read.csv("../Data/LRlc.csv"))$result.scientific_name)), 
                (length(unique(read.csv("../Data/NT.csv"))$result.scientific_name) + length(unique(read.csv("../Data/LRnt.csv"))$result.scientific_name) + length(unique(read.csv("../Data/LRcd.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/VU.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/EN.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/CR.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/EW.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/EX.csv"))$result.scientific_name)),
                (length(unique(read.csv("../Data/DD.csv"))$result.scientific_name)))

colnames(Red_List_Totals) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(Red_List_Totals) <- c("Red List Totals of current categories")
Red_List_Totals 
sum(Red_List_Totals) # total number of listings #93872

# Total species I have useful histories for AT THIS POINT - not the final number in the very last dataset (Estimates are being informed here using these figures):

df <- read.csv("../Data/wrangling.csv", header = TRUE) # from Wrangling.R ## 111017 obs
#df <- read.csv("../Data/msm_noEW.csv", header = TRUE) # from final_recode_msm.R


length(unique(df$name)) # 28110 spp


# look at final entry to see most recent categorisation
latest_yr <-df[!duplicated(df$name, fromLast = TRUE), ]

useful <- cbind(c(sum(latest_yr$state==1)),
                sum(latest_yr$state==2),
                sum(latest_yr$state==3),
                sum(latest_yr$state==4),
                sum(latest_yr$state==5),
                sum(latest_yr$state==6),
                sum(latest_yr$state==7),
                sum(latest_yr$state==0))

colnames(useful) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(useful) <- c("spp with useful histories TO THIS POINT")

# bind Totals and useful

summary <- rbind(Red_List_Totals, useful)
summary

useful07 <- cbind(c(sum(latest_yr$state==1 & latest_yr$result.year>=2007)),
                  sum(latest_yr$state==2 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==3 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==4 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==5 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==6 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==7 & latest_yr$result.year>=2007),
                  sum(latest_yr$state==0 & latest_yr$result.year>=2007))

colnames(useful07) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(useful07) <- c("useful histories (latest listing since 2007)")

# bind to summary

summary <- rbind(summary, useful07)
summary


# read in changes df 

changes <- read.csv("../Data/Master_changes_final.csv", header = TRUE) #6549 obs

# See how many species are in changes but do not have a useful history listing in df

diffs <-  setdiff(unique(changes$name), unique(df$name))
#diffs2 <- dplyr::setdiff(changes$name, df$name)

# Exclude these from the changes file, to retain only spp with useful histories

changes_lose <- dplyr::filter(changes, name %in% diffs)
changes <- dplyr::anti_join(changes, changes_lose) #5927 obs

#changes <- dplyr::semi_join(changes, df) #3798 obs THROWS UP WRONG ANSWER

#check all exclusions have been made
diffs2 <- setdiff(changes$name, df$name) #0 spp so all ok

length(unique(changes$name)) #5556 spp with changes and useful histories

# some of the species must have been wrongly categorised more than once - so to check:

duplicates <- changes %>%
  group_by(name) %>%
  filter(duplicated(name)) # 371 names have more than one entry

## Total changes therefore 5556 + 371 = 5927

# summarise the miscategorised states - doesn't matter at this point if for genuine or non-genuine reasons:

# wrongly categorised and totals (from and to statuses)
# I am not interested where the before and after state is the same though and this is taken account of in the code below


changedfrom <- cbind(c(sum(changes$state1==1 & changes$state2!=1)), 
                   (sum(changes$state1==2 & changes$state2!=2)), 
                   (sum(changes$state1==3 & changes$state2!=3)), 
                   (sum(changes$state1==4 & changes$state2!=4)), 
                   (sum(changes$state1==5 & changes$state2!=5)), 
                   (sum(changes$state1==6 & changes$state2!=6)), 
                   (sum(changes$state1==7 & changes$state2!=7)), 
                   (sum(changes$state1==0 & changes$state2!=0)))

colnames(changedfrom) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(changedfrom) <- c("transitions from (since 2007)")

summary <- rbind(summary, changedfrom)

changedto <- cbind(c(sum(changes$state1!=1 & changes$state2==1)), 
                   (sum(changes$state1!=2 & changes$state2==2)), 
                   (sum(changes$state1!=3 & changes$state2==3)), 
                   (sum(changes$state1!=4 & changes$state2==4)), 
                   (sum(changes$state1!=5 & changes$state2==5)), 
                   (sum(changes$state1!=6 & changes$state2==6)), 
                   (sum(changes$state1!=7 & changes$state2==7)), 
                   (sum(changes$state1!=0 & changes$state2==0)))

colnames(changedto) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(changedto) <- c("transitions to (since 2007)")

summary <- rbind(summary, changedto)
summary

# rows 4 and 5 of the summary should be equal:

sum(summary[4,]) #5877
sum(summary[5,]) #5877

# from the above there were 5927 changes

# add same state transitions

nochange <- cbind(c(sum(changes$state1==1 & changes$state2==1)), 
                  (sum(changes$state1==2 & changes$state2==2)), 
                  (sum(changes$state1==3 & changes$state2==3)), 
                  (sum(changes$state1==4 & changes$state2==4)), 
                  (sum(changes$state1==5 & changes$state2==5)), 
                  (sum(changes$state1==6 & changes$state2==6)), 
                  (sum(changes$state1==7 & changes$state2==7)), 
                  (sum(changes$state1==0 & changes$state2==0)))

colnames(nochange) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(nochange) <- c("transitions with no change (since 07)")

summary <- rbind(summary, nochange)
summary


# Total transitions:
sum(summary[4,])+sum(summary[6,]) #5927 


# Add total numbers in each category
Total_numbers <- cbind(c(sum(df$state==1)),
                  (sum(df$state==2)), 
                  (sum(df$state==3)), 
                  (sum(df$state==4)), 
                  (sum(df$state==5)), 
                  (sum(df$state==6)), 
                  (sum(df$state==7)), 
                  (sum(df$state==0)))

colnames(Total_numbers) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(Total_numbers) <- c("Total listings in each category")

summary <- rbind(summary, Total_numbers)
summary

# and since 2007

Total_numbers07 <- cbind(c(sum(df$state==1 & df$result.year>=2007)),
                       (sum(df$state==2 & df$result.year>=2007)), 
                       (sum(df$state==3 & df$result.year>=2007)), 
                       (sum(df$state==4 & df$result.year>=2007)), 
                       (sum(df$state==5 & df$result.year>=2007)), 
                       (sum(df$state==6 & df$result.year>=2007)), 
                       (sum(df$state==7 & df$result.year>=2007)), 
                       (sum(df$state==0 & df$result.year>=2007)))

colnames(Total_numbers07) <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")
row.names(Total_numbers07) <- c("Total listings in each category (since 2007)")

summary <- rbind(summary, Total_numbers07)
summary

sum(summary[7,]) #111017
sum(summary[8,]) #64418

##################################################################################################
# Now to sumarise all changes between states from 2007, where I have useful histories

LC_DD <-sum(changes$state1==1 & changes$state2==0)
LC_LC <-sum(changes$state1==1 & changes$state2==1)
LC_NT <-sum(changes$state1==1 & changes$state2==2)
LC_VU <-sum(changes$state1==1 & changes$state2==3)
LC_EN <-sum(changes$state1==1 & changes$state2==4)
LC_CR <-sum(changes$state1==1 & changes$state2==5)
LC_EW <-sum(changes$state1==1 & changes$state2==6)
LC_EX <-sum(changes$state1==1 & changes$state2==7)

NT_DD <-sum(changes$state1==2 & changes$state2==0)
NT_LC <-sum(changes$state1==2 & changes$state2==1)
NT_NT <-sum(changes$state1==2 & changes$state2==2)
NT_VU <-sum(changes$state1==2 & changes$state2==3)
NT_EN <-sum(changes$state1==2 & changes$state2==4)
NT_CR <-sum(changes$state1==2 & changes$state2==5)
NT_EW <-sum(changes$state1==2 & changes$state2==6)
NT_EX <-sum(changes$state1==2 & changes$state2==7)

VU_DD <-sum(changes$state1==3 & changes$state2==0)
VU_LC <-sum(changes$state1==3 & changes$state2==1)
VU_NT <-sum(changes$state1==3 & changes$state2==2)
VU_VU <-sum(changes$state1==3 & changes$state2==3)
VU_EN <-sum(changes$state1==3 & changes$state2==4)
VU_CR <-sum(changes$state1==3 & changes$state2==5)
VU_EW <-sum(changes$state1==3 & changes$state2==6)
VU_EX <-sum(changes$state1==3 & changes$state2==7)

EN_DD <-sum(changes$state1==4 & changes$state2==0)
EN_LC <-sum(changes$state1==4 & changes$state2==1)
EN_NT <-sum(changes$state1==4 & changes$state2==2)
EN_VU <-sum(changes$state1==4 & changes$state2==3)
EN_EN <-sum(changes$state1==4 & changes$state2==4)
EN_CR <-sum(changes$state1==4 & changes$state2==5)
EN_EW <-sum(changes$state1==4 & changes$state2==6)
EN_EX <-sum(changes$state1==4 & changes$state2==7)

CR_DD <-sum(changes$state1==5 & changes$state2==0)
CR_LC <-sum(changes$state1==5 & changes$state2==1)
CR_NT <-sum(changes$state1==5 & changes$state2==2)
CR_VU <-sum(changes$state1==5 & changes$state2==3)
CR_EN <-sum(changes$state1==5 & changes$state2==4)
CR_CR <-sum(changes$state1==5 & changes$state2==5)
CR_EW <-sum(changes$state1==5 & changes$state2==6)
CR_EX <-sum(changes$state1==5 & changes$state2==7)

EW_DD <-sum(changes$state1==6 & changes$state2==0)
EW_LC <-sum(changes$state1==6 & changes$state2==1)
EW_NT <-sum(changes$state1==6 & changes$state2==2)
EW_VU <-sum(changes$state1==6 & changes$state2==3)
EW_EN <-sum(changes$state1==6 & changes$state2==4)
EW_CR <-sum(changes$state1==6 & changes$state2==5)
EW_EW <-sum(changes$state1==6 & changes$state2==6)
EW_EX <-sum(changes$state1==6 & changes$state2==7)

EX_DD <-sum(changes$state1==7 & changes$state2==0)
EX_LC <-sum(changes$state1==7 & changes$state2==1)
EX_NT <-sum(changes$state1==7 & changes$state2==2)
EX_VU <-sum(changes$state1==7 & changes$state2==3)
EX_EN <-sum(changes$state1==7 & changes$state2==4)
EX_CR <-sum(changes$state1==7 & changes$state2==5)
EX_EW <-sum(changes$state1==7 & changes$state2==6)
EX_EX <-sum(changes$state1==7 & changes$state2==7)

DD_DD <-sum(changes$state1==0 & changes$state2==0)
DD_LC <-sum(changes$state1==0 & changes$state2==1)
DD_NT <-sum(changes$state1==0 & changes$state2==2)
DD_VU <-sum(changes$state1==0 & changes$state2==3)
DD_EN <-sum(changes$state1==0 & changes$state2==4)
DD_CR <-sum(changes$state1==0 & changes$state2==5)
DD_EW <-sum(changes$state1==0 & changes$state2==6)
DD_EX <-sum(changes$state1==0 & changes$state2==7)


# show all transitions between states:

matrix_full <- rbind(c(DD_DD, LC_DD, NT_DD, VU_DD, EN_DD, CR_DD, EW_DD, EX_DD),
                     c(DD_LC, LC_LC, NT_LC, VU_LC, EN_LC, CR_LC, EW_LC, EX_LC),
                     c(DD_NT, LC_NT, NT_NT, VU_NT, EN_NT, CR_NT, EW_NT, EX_NT),
                     c(DD_VU, LC_VU, NT_VU, VU_VU, EN_VU, CR_VU, EW_VU, EX_VU),
                     c(DD_EN, LC_EN, NT_EN, VU_EN, EN_EN, CR_EN, EW_EN ,EX_EN),
                     c(DD_CR, LC_CR, NT_CR, VU_CR, EN_CR, CR_CR, EW_CR, EX_CR),
                     c(DD_EW, LC_EW, NT_EW, VU_EW, EN_EW, CR_EW, EW_EW, EX_EW),
                     c(DD_EX, LC_EX, NT_EX, VU_EX, EN_EX, CR_EX, EW_EX, EX_EX))
colnames(matrix_full) <- c("From DD", "From LC", "From NT", "From VU", "From EN", "From CR", "From EW", "From EX")
row.names(matrix_full) <-c("Into DD", "Into LC", "Into NT", "Into VU", "Into EN", "Into CR", "Into EW", "Into EX")

sum(matrix_full) #5927 so all obs accounted for
matrix_full

summary
# save summary
save(summary, file = "../Data/summary_data.Rdata")
# save full matrix
save(matrix_full, file = "../Data/matrix_full.Rdata")


#################################################

# Now to sumarise all NG changes between states from 2007, where I have useful histories

NGLC_DD <-sum(changes$state1==1 & changes$state2==0 & changes$N.G=="N")
NGLC_LC <-sum(changes$state1==1 & changes$state2==1 & changes$N.G=="N")
NGLC_NT <-sum(changes$state1==1 & changes$state2==2 & changes$N.G=="N")
NGLC_VU <-sum(changes$state1==1 & changes$state2==3 & changes$N.G=="N")
NGLC_EN <-sum(changes$state1==1 & changes$state2==4 & changes$N.G=="N")
NGLC_CR <-sum(changes$state1==1 & changes$state2==5 & changes$N.G=="N")
NGLC_EW <-sum(changes$state1==1 & changes$state2==6 & changes$N.G=="N")
NGLC_EX <-sum(changes$state1==1 & changes$state2==7 & changes$N.G=="N")

NGNT_DD <-sum(changes$state1==2 & changes$state2==0 & changes$N.G=="N")
NGNT_LC <-sum(changes$state1==2 & changes$state2==1 & changes$N.G=="N")
NGNT_NT <-sum(changes$state1==2 & changes$state2==2 & changes$N.G=="N")
NGNT_VU <-sum(changes$state1==2 & changes$state2==3 & changes$N.G=="N")
NGNT_EN <-sum(changes$state1==2 & changes$state2==4 & changes$N.G=="N")
NGNT_CR <-sum(changes$state1==2 & changes$state2==5 & changes$N.G=="N")
NGNT_EW <-sum(changes$state1==2 & changes$state2==6 & changes$N.G=="N")
NGNT_EX <-sum(changes$state1==2 & changes$state2==7 & changes$N.G=="N")
NGVU_DD <-sum(changes$state1==3 & changes$state2==0 & changes$N.G=="N")
NGVU_LC <-sum(changes$state1==3 & changes$state2==1 & changes$N.G=="N")
NGVU_NT <-sum(changes$state1==3 & changes$state2==2 & changes$N.G=="N")
NGVU_VU <-sum(changes$state1==3 & changes$state2==3 & changes$N.G=="N")
NGVU_EN <-sum(changes$state1==3 & changes$state2==4 & changes$N.G=="N")
NGVU_CR <-sum(changes$state1==3 & changes$state2==5 & changes$N.G=="N")
NGVU_EW <-sum(changes$state1==3 & changes$state2==6 & changes$N.G=="N")
NGVU_EX <-sum(changes$state1==3 & changes$state2==7 & changes$N.G=="N")

NGEN_DD <-sum(changes$state1==4 & changes$state2==0 & changes$N.G=="N")
NGEN_LC <-sum(changes$state1==4 & changes$state2==1 & changes$N.G=="N")
NGEN_NT <-sum(changes$state1==4 & changes$state2==2 & changes$N.G=="N")
NGEN_VU <-sum(changes$state1==4 & changes$state2==3 & changes$N.G=="N")
NGEN_EN <-sum(changes$state1==4 & changes$state2==4 & changes$N.G=="N")
NGEN_CR <-sum(changes$state1==4 & changes$state2==5 & changes$N.G=="N")
NGEN_EW <-sum(changes$state1==4 & changes$state2==6 & changes$N.G=="N")
NGEN_EX <-sum(changes$state1==4 & changes$state2==7 & changes$N.G=="N")

NGCR_DD <-sum(changes$state1==5 & changes$state2==0 & changes$N.G=="N")
NGCR_LC <-sum(changes$state1==5 & changes$state2==1 & changes$N.G=="N")
NGCR_NT <-sum(changes$state1==5 & changes$state2==2 & changes$N.G=="N")
NGCR_VU <-sum(changes$state1==5 & changes$state2==3 & changes$N.G=="N")
NGCR_EN <-sum(changes$state1==5 & changes$state2==4 & changes$N.G=="N")
NGCR_CR <-sum(changes$state1==5 & changes$state2==5 & changes$N.G=="N")
NGCR_EW <-sum(changes$state1==5 & changes$state2==6 & changes$N.G=="N")
NGCR_EX <-sum(changes$state1==5 & changes$state2==7 & changes$N.G=="N")

NGEW_DD <-sum(changes$state1==6 & changes$state2==0 & changes$N.G=="N")
NGEW_LC <-sum(changes$state1==6 & changes$state2==1 & changes$N.G=="N")
NGEW_NT <-sum(changes$state1==6 & changes$state2==2 & changes$N.G=="N")
NGEW_VU <-sum(changes$state1==6 & changes$state2==3 & changes$N.G=="N")
NGEW_EN <-sum(changes$state1==6 & changes$state2==4 & changes$N.G=="N")
NGEW_CR <-sum(changes$state1==6 & changes$state2==5 & changes$N.G=="N")
NGEW_EW <-sum(changes$state1==6 & changes$state2==6 & changes$N.G=="N")
NGEW_EX <-sum(changes$state1==6 & changes$state2==7 & changes$N.G=="N")

NGEX_DD <-sum(changes$state1==7 & changes$state2==0 & changes$N.G=="N")
NGEX_LC <-sum(changes$state1==7 & changes$state2==1 & changes$N.G=="N")
NGEX_NT <-sum(changes$state1==7 & changes$state2==2 & changes$N.G=="N")
NGEX_VU <-sum(changes$state1==7 & changes$state2==3 & changes$N.G=="N")
NGEX_EN <-sum(changes$state1==7 & changes$state2==4 & changes$N.G=="N")
NGEX_CR <-sum(changes$state1==7 & changes$state2==5 & changes$N.G=="N")
NGEX_EW <-sum(changes$state1==7 & changes$state2==6 & changes$N.G=="N")
NGEX_EX <-sum(changes$state1==7 & changes$state2==7 & changes$N.G=="N")

NGDD_DD <-sum(changes$state1==0 & changes$state2==0 & changes$N.G=="N")
NGDD_LC <-sum(changes$state1==0 & changes$state2==1 & changes$N.G=="N")
NGDD_NT <-sum(changes$state1==0 & changes$state2==2 & changes$N.G=="N")
NGDD_VU <-sum(changes$state1==0 & changes$state2==3 & changes$N.G=="N")
NGDD_EN <-sum(changes$state1==0 & changes$state2==4 & changes$N.G=="N")
NGDD_CR <-sum(changes$state1==0 & changes$state2==5 & changes$N.G=="N")
NGDD_EW <-sum(changes$state1==0 & changes$state2==6 & changes$N.G=="N")
NGDD_EX <-sum(changes$state1==0 & changes$state2==7 & changes$N.G=="N")


# show all transitions between states:

NGmatrix_full <- rbind(c(NGDD_DD, NGLC_DD, NGNT_DD, NGVU_DD, NGEN_DD, NGCR_DD, NGEW_DD, NGEX_DD),
                      c(NGDD_LC, NGLC_LC, NGNT_LC, NGVU_LC, NGEN_LC, NGCR_LC, NGEW_LC, NGEX_LC),
                      c(NGDD_NT, NGLC_NT, NGNT_NT, NGVU_NT, NGEN_NT, NGCR_NT, NGEW_NT, NGEX_NT),
                      c(NGDD_VU, NGLC_VU, NGNT_VU, NGVU_VU, NGEN_VU, NGCR_VU, NGEW_VU, NGEX_VU),
                      c(NGDD_EN, NGLC_EN, NGNT_EN, NGVU_EN, NGEN_EN, NGCR_EN, NGEW_EN ,NGEX_EN),
                      c(NGDD_CR, NGLC_CR, NGNT_CR, NGVU_CR, NGEN_CR, NGCR_CR, NGEW_CR, NGEX_CR),
                      c(NGDD_EW, NGLC_EW, NGNT_EW, NGVU_EW, NGEN_EW, NGCR_EW, NGEW_EW, NGEX_EW),
                      c(NGDD_EX, NGLC_EX, NGNT_EX, NGVU_EX, NGEN_EX, NGCR_EX, NGEW_EX, NGEX_EX))
colnames(NGmatrix_full) <- c("From DD", "From LC", "From NT", "From VU", "From EN", "From CR", "From EW", "From EX")
row.names(NGmatrix_full) <-c("Into DD", "Into LC", "Into NT", "Into VU", "Into EN", "Into CR", "Into EW", "Into EX")

sum(changes$N.G=="N") # 5165
sum(NGmatrix_full) #5165 So all obs accounted for
NGmatrix_full

# save NGmatrix_full

save(NGmatrix_full, file = "../Data/NGmatrix_full.Rdata")

# Summarise 2007 onwards by year:
sort(unique(df$result.year))

# Totals
df07 <- sum(df$result.year==2007)
df08 <- sum(df$result.year==2008)
df09 <- sum(df$result.year==2009)
df10 <- sum(df$result.year==2010)
df11 <- sum(df$result.year==2011)
df12 <- sum(df$result.year==2012)
df13 <- sum(df$result.year==2013)
df14 <- sum(df$result.year==2014)
df15 <- sum(df$result.year==2015)
df16 <- sum(df$result.year==2016) + sum(df$result.year==2016.5)
df17 <- sum(df$result.year==2017) + sum(df$result.year==2017.5)

sort(unique(changes$result.year))

# Genuine
G07 <- sum(changes$N.G=="G" & changes$result.year==2007)
G08 <- sum(changes$N.G=="G" & changes$result.year==2008)
G09 <- sum(changes$N.G=="G" & changes$result.year==2009)
G10 <- sum(changes$N.G=="G" & changes$result.year==2010)
G11 <- sum(changes$N.G=="G" & changes$result.year==2011)
G12 <- sum(changes$N.G=="G" & changes$result.year==2012)
G13 <- sum(changes$N.G=="G" & changes$result.year==2013)
G14 <- sum(changes$N.G=="G" & changes$result.year==2014) 
G15 <- sum(changes$N.G=="G" & changes$result.year==2015)
G16 <- sum(changes$N.G=="G" & changes$result.year==2016)+sum(changes$N.G=="G" & changes$result.year==2016.5) # using | here didn't give the correct answer
G17 <- sum(changes$N.G=="G" & changes$result.year==2017)+sum(changes$N.G=="G" & changes$result.year==2017.5)

# Non-Genuine
NG07 <- sum(changes$N.G=="N" & changes$result.year==2007)
NG08 <- sum(changes$N.G=="N" & changes$result.year==2008)
NG09 <- sum(changes$N.G=="N" & changes$result.year==2009)
NG10 <- sum(changes$N.G=="N" & changes$result.year==2010)
NG11 <- sum(changes$N.G=="N" & changes$result.year==2011)
NG12 <- sum(changes$N.G=="N" & changes$result.year==2012)
NG13 <- sum(changes$N.G=="N" & changes$result.year==2013)
NG14 <- sum(changes$N.G=="N" & changes$result.year==2014)
NG15 <- sum(changes$N.G=="N" & changes$result.year==2015)
NG16 <- sum(changes$N.G=="N" & changes$result.year==2016)+sum(changes$N.G=="N" & changes$result.year==2016.5)
NG17 <- sum(changes$N.G=="N" & changes$result.year==2017)+sum(changes$N.G=="N" & changes$result.year==2017.5)



# Post 07 summary
## Note: Each row in this summary will add up to the total number of post 2007 obs (not spp) as some spp will be included in more than one year over the 11 years covered


Post07 <- rbind(c(df07, df08, df09, df10, df11, df12, df13, df14, df15, df16, df17),
                c( G07,  G08,  G09,  G10,  G11,  G12,  G13,  G14,  G15,  G16,  G17),
                c(NG07, NG08, NG09, NG10, NG11, NG12, NG13, NG14, NG15, NG16, NG17),
                c(df07-G07-NG07,df08-G08-NG08, df09-G09-NG09, df10-G10-NG10, df11-G11-NG11, df12-G12-NG12, df13-G13-NG13, df14-G14-NG14, df15-G15-NG15, df16-G16-NG16, df17-G17-NG17 ))
colnames(Post07) <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
row.names(Post07) <- c("transitions", "transitions w/G changes", "transitions w/NG changes", "transitions w/no change")
Post07
save(Post07, file = "../Data/Post07.Rdata")

###################

## Finally, I really need to merge df with changes
## See all the notes at the end of this file about listings not coming back "complete" from the RedList and effectively losing 79 N changes and 25 G changes
## To counter this I am using a full_join command below (not right or left join)

dffull <- dplyr::full_join(changes, df) # join df with changes
# This results in 111122 obs
# order by name and in ascending order

dffull <- dffull[order(dffull$name, dffull$result.year),]

# where I have added in missing changes observations, the state is shown in state2 but not in state, so I need to copy these across (without a loop)

dffull$state[dffull$state2==0] <-0
dffull$state[dffull$state2==1] <-1
dffull$state[dffull$state2==2] <-2
dffull$state[dffull$state2==3] <-3
dffull$state[dffull$state2==4] <-4
dffull$state[dffull$state2==5] <-5
dffull$state[dffull$state2==6] <-6
dffull$state[dffull$state2==7] <-7

dffull <- dplyr::select(dffull, name, result.year, state, TSFO, N.G) # only retain what's useful
colnames(dffull) <- c("name", "result.year", "state", "TSFO","change.status")

# Now rework TSFO (time since first observation) column and calculate the number of years from the fist observation for each subsequent observation (not the time between observations), to incorporate new columns added because of missing changes

dffull <- dffull %>%
  group_by(name) %>%
  arrange(name) %>%
  mutate(TSFO = result.year - first(result.year))


# to avoid problems with row numbers save and re-load here

write.csv(dffull, "../Data/dffull.csv", row.names = FALSE)
dffull <- read.csv("../Data/dffull.csv", header=TRUE) #111122

# Now check for duplicates

duplicates <- dffull %>%
  group_by(name) %>%
  filter(duplicated(result.year)) # 0 


levels(dffull$change.status)
#add new factor level "U"
dffull$change.status = factor(dffull$change.status, levels=c(levels(dffull$change.status), "U"))

#convert all NA's to "U"
dffull$change.status[is.na(dffull$change.status)] = "U"

# Save

write.csv(dffull, "../Data/Overalldf.csv", row.names = FALSE)


sum(dffull$change.status=="N") # 5165 - expected sum(Post07[3,]) = 5165
sum(dffull$change.status=="G") # 762 - expected sum(Post07[2,]) = 762
sum(dffull$change.status=="U") # 105195 - The balance


####Working through earlier issues:
# so I have a total of non_U changes here of 5822, whereas I expected 5926
# I need to work out why 104 have not been correctly categorised after merging changes and df

# It seems as if around 104 RedList Histories have come back in incomplete form

# Now to find species which appear to be incomplete (and appear in the changes file but have incomplete listings incorporated within dffull (and df))

# Here using merge{base} rather than a dplyr function so that I can easily see what isn't in my main df

#Included_changes <- merge(changes, dffull) # 5824
# Check for and exclude duplicates as I have 5822 referenced above

#duplicates2 <- Included_changes %>%
#  group_by(name) %>%
#  filter(duplicated(result.year))
# There are therefore (as expected two duplicates)

#which(grepl(as.character(duplicates2$name[1]), Included_changes$name))
#Included_changes[5258:5259,] # remove row number 5259
#which(grepl(as.character(duplicates2$name[2]), Included_changes$name))
#Included_changes[5260:5261,] # remove row number 5261

# remove the dulpicates
#rr2 <- c(5259, 5261)
#Included_changes <- Included_changes[!(row.names(Included_changes) %in% rr2), ] # now have the expected 5822 obs


# Find out the difference between changes and Included_changes.  First make sure that the column headings are the same

#Included_changes <- dplyr::select(Included_changes, name, result.year, state)
#colnames(Included_changes) <- c("name", "result.year", "state")
#changes <- dplyr::select(changes, name, result.year, state2)
#colnames(changes) <- c("name", "result.year", "state")
#dffull_edited <- dplyr::select(dffull, name, result.year, state)

#find_diffs <- setdiff(changes, dffull_edited) # 106 obs, was expecting 104 so check for duplicates of names
#length(unique(find_diffs$name)) # 104


# double check the histories of these 104 spp

#library(rredlist)

#RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"

# re-request histories
#names_list <- as.character(unique(find_diffs$name))

#recheck_histories <- lapply(1:length(names_list), function(i){
#  species_history <- rl_history(name=names_list[i], key=RL_token)
#})
#save(recheck_histories, file = "../Data/recheck_histories.Rdata")

# convert to dfs and see what is going on here

# convert to a df 
#df_creation <- function(a_list){
#  df <- data.frame()
#  for (i in 1:length(a_list)){
#    aaa <- as.data.frame(a_list[i])
#    df <- rbind(df, aaa)
#  }
#  return(df)
#}

#recheck_historiesdf <- df_creation(recheck_histories) # 289 obs of 104 spp
# filter as only 1994+ entries will have been considered
#recheck_historiesdf <- dplyr::filter(recheck_historiesdf, result.year>=1994) #236 obs
# only retain name and result year

#recheck_historiesdf <- dplyr::select(recheck_historiesdf, name, result.year)

# now to see if any of find_diffs appear in these histories, or if the history listings have come back incomplete

#find_diffs_edited <- dplyr::select(find_diffs, name, result.year)
#find_diffs_edited$result.year <-as.character(find_diffs_edited$result.year)

#missing_entries <- setdiff(find_diffs_edited, recheck_historiesdf)
# so 102 entries have come back from the RedList incomplete


## To counter this I need to "add them back" in correct format to dffull.  The entries are there because I have joined the two dfs together (df and changes), but I have lost the N or G part of the change

