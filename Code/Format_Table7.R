## Author: Rachel Bates (rlb18@ic.ac.uk)
## Date: 13/02/2020

require(dplyr)

## Script to allow the Table 7 data to be usable
## First I downloaded the data from the IUCN website 
##### https://www.iucnredlist.org/resources/summary-statistics only available as a pdf
## Used a free online tool to convert them to csv

col_names <- c("Scientific_Name", "Common_Name", "Previous_Category", "New_Category", "Reason_For_Change", "Year")

## Read in the table 7 docs

### Could do via lapply or similar? ###
changes2007 <- read.csv("../Data/2007Table7.csv", header=FALSE)
changes2008 <- read.csv("../Data/2008Table7.csv", header=FALSE)
changes2009 <- read.csv("../Data/2009Table7.csv", header=FALSE)
changes2010 <- read.csv("../Data/2010Table7.csv", header=FALSE)
changes2011 <- read.csv("../Data/2011Table7.csv", header=FALSE)
changes2012 <- read.csv("../Data/2012Table7.csv", header=FALSE)
changes2013 <- read.csv("../Data/2013Table7.csv", header=FALSE)
changes2014 <- read.csv("../Data/2014Table7.csv", header=FALSE)
changes2015 <- read.csv("../Data/2015Table7.csv", header=FALSE)
changes2016 <- read.csv("../Data/2016Table7.csv", header=FALSE)
changes2017 <- read.csv("../Data/2017Table7.csv", header=FALSE)
changes2018 <- read.csv("../Data/2018Table7.csv", header=FALSE)
changes2019 <- read.csv("../Data/2019Table7.csv", header=FALSE)


## Individual changes to get the formatting in line with each other
changes2008["Reason_For_Change"] <- "G"
changes2008["Year"] <- "2008"

changes2009 <- changes2009[,1:5]
changes2009["Year"] <- "2009"

changes2011 <- changes2011[,2:7]
changes2012 <- changes2012[,2:7]
changes2013 <- changes2013[,2:7]
changes2014 <- changes2014[,2:7]

## Put all of the dfs in a list
all_tables = mget(ls(pattern = "changes[2007:2019]"))

## Make all the colnames the same
for (i in 1:length(all_tables)){
  print(i)
  names(all_tables[[i]]) <- col_names
}

## Bind together into one df
Table7 <- bind_rows(all_tables)
Table7[Table7==""] <- NA
Table7 <- Table7[complete.cases(Table7[,c(1,3:6)]),]

## Check for and remove erroneous rows
table(Table7$Previous_Category)
Table7 <- dplyr::filter(Table7, Previous_Category != "(2007)" & Previous_Category != "(2008)")

#############################################################

### 2007 ###
# 
# changes2007 <- read.csv("../Data/2007RL_Stats_Table_7.csv", header=FALSE)
# changes2007 <- changes2007[c(17:22,24:61,63:72),]
# names(changes2007) <- col_names
# 
# ### 2008 ###
# 
# changes2008 <- read.csv("../Data/2008RL_Stats_Table_7.csv", header=FALSE)
# changes2008 <- changes2008[c(16:52,54:55,60:113,118:171,176:209,212:213,215:227,232:242,245:250,253,255:261,264),]
# changes2008["Reason_For_Change"] <- "G"
# changes2008["Year"] <- "2008"
# names(changes2008) <- col_names
# 
# ### 2009 ###
# 
# changes2009 <- read.csv("../Data/2009RL_Stats_Table_7.csv", header=FALSE)
# changes2009 <- changes2009[c(18,20:69,77:88,90:126,128:141,148:164,166:199,201:212),1:5]
# changes2009["Year"] <- "2009"
# names(changes2009) <- col_names
# 
# ### 2010 ###
# 
# changes2010 <- read.csv("../Data/2010_4RL_Stats_Table_7.csv", header=FALSE)
# changes2010 <- changes2010[c(21:48,50:66,73:94,96:117,119:131,133:134,141:202,209:257,259:270,277:323,325:333,335:337,345:409,416:443,445:478,485:504,506,508:519,521:546,553:593,595:610),]
# names(changes2010) <- col_names
# 
# ### 2011 ###
# 
# changes2011 <- read.csv("../Data/2011_2_RL_Stats_Table7.csv", header=FALSE)
# changes2011 <- changes2011[c(18:28,30:69,78:111,113:115,117:146,148:154,162:166,168:241,248:328,335:354,356:381,383:397),2:7]
# names(changes2011) <- col_names
# 
# ### 2012 ###
# 
# changes2012 <- read.csv("../Data/2012_2_RL_Stats_Table_7.csv", header=FALSE)
# changes2012 <- changes2012[c(18:19,21:62,70:138,145:213,220:223,225:250,252:258,260:284,291,293:358,365:401,403:432,439:471,473:481,483:485),2:7]
# names(changes2012) <- col_names
# 
# ### 2013 ###
# 
# changes2013 <- read.csv("../Data/2013_2_RL_Stats_Table7_edited.csv", header=FALSE)
# changes2013 <- changes2013[c(19:23,25:64,73:77,79:110,112:140,148:153,155:212,214,222:226,228:231,233,235:289,297:365,373:404,406:440,448:502),2:7]
# names(changes2013) <- col_names
# 
# ### 2014 ###
# 
# changes2014 <- read.csv("../Data/2014_3_RL_Stats_Table_7.csv", header=FALSE)
# changes2014 <- changes2014[,2:7]
# changes2014[changes2014==""] <- NA
# changes2014 <- changes2014[complete.cases(changes2014[,c(1,3:6)]),]
# names(changes2014) <- col_names
# 
# ### 2015 ###
# 
# changes2015 <- read.csv("../Data/2015_4_RL_Stats_Table_7.csv", header=FALSE)
# changes2015[changes2015==""] <- NA
# changes201 <- changes2014[complete.cases(changes2014[,c(1,3:6)]),]


















