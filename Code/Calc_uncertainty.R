
##### Calculating uncertainty measures #########
##############################################

## Packages

#################

## Data

##Gather file names
filenames <- list.files(path="../Data/",
                        pattern="2020_full_random+.*csv")

# Read files into a list
All <- lapply(filenames,function(i){
  i <- paste("../Data/",i,sep="")
  read.csv(i, header=TRUE)
})
filenames <- gsub("-",".",filenames)
names(All) <- gsub(".csv","",filenames)

##Rename the files in the list
names(All) <- substr(filenames,1,nchar(filenames)-4)

#################

## Functions


#################

## Code




