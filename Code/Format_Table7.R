## Author: Rachel Bates (rlb18@ic.ac.uk)
## Date: 13/02/2020

## Script to allow the Table 7 data to be usable
## First I downloaded the data from the IUCN website 
##### https://www.iucnredlist.org/resources/summary-statistics only available as a pdf
## Used a free online tool to convert them to csv

col_names <- c("Scientific_Name", "Common_Name", "Previous_Category", "New_Category", "Reason_For_Change", "Year")

### 2007 ###

changes2007 <- read.csv("../Data/2007RL_Stats_Table_7.csv", header=FALSE)
changes2007 <- changes2007[c(17:22,24:61,63:72),]
names(changes2007) <- col_names

### 2008 ###

changes2008 <- read.csv("../Data/2008RL_Stats_Table_7.csv", header=FALSE)
changes2008 <- changes2008[c(16:52,54:55,60:113,118:171,176:209,212:213,215:227,232:242,245:250,253,255:261,264),]
changes2008["Reason_For_Change"] <- "G"
changes2008["Year"] <- "2008"
names(changes2008) <- col_names

### 2009 ###

changes2009 <- read.csv("../Data/2009RL_Stats_Table_7.csv", header=FALSE)
changes2009 <- changes2009[c(18,20:69,77:88,90:126,128:141,148:164,166:199,201:212),1:5]
changes2009["Year"] <- "2009"
names(changes2009) <- col_names

### 2010 ###

changes2010 <- read.csv("../Data/2010_4RL_Stats_Table_7.csv", header=FALSE)
changes2010 <- changes2010[c(21:48,50:66,73:94,96:117,119:131,133:134,141:202,209:257,259:270,277:323,325:333,335:337,345:409,416:443,445:478,485:504,506,508:519,521:546,553:593,595:610),]
names(changes2010) <- col_names

### 2011 ###

changes2011 <- read.csv("../Data/2011_2_RL_Stats_Table7.csv", header=FALSE)
changes2011 <- changes2011[c(18:28,30:69,78:111,113:115,117:146,148:154,162:166,168:241,248:328,335:354,356:381,383:397),2:7]
names(changes2011) <- col_names

### 2012 ###

changes2012 <- read.csv("../Data/2012_2_RL_Stats_Table_7.csv", header=FALSE)
changes2012 <- changes2012[c(18:19,21:62,70:138,145:213,220:223,225:250,252:258,260:284,291,293:358,365:401,403:432,439:471,473:481,483:485),2:7]
names(changes2012) <- col_names

### 2013 ###















