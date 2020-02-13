rm(list=ls())

# list of all average_new Rdata files

all.files <-c()
for (i in 1:100){
  
  all.files[i] <- paste("../Manydf/average_new", i, ".Rdata", sep="")
}

matrices<- lapply(all.files, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

save(matrices, file="../Data/av100sensitivity_new.Rdata")
save(matrices, file= "../Data/av100sens_new_convergedonly.Rdata")# only saving the ones which converged
#all.files <-c()
#for (i in 1:100){
#  
#  all.files[i] <- paste("../Manydf/average", i, ".Rdata", sep="")
#}



#matrices<- lapply(all.files, function(x) {
#  load(file = x)
#  get(ls()[ls()!= "filename"])
#})

#save(matrices, file="../Data/av100sensitivity.Rdata")

# now average out
matrix.array <- array(unlist(matrices), dim = c(6,6,100))
sd <-apply(matrix.array, c(1,2), sd) # c(1,2) means rows and columns
ci <-apply(matrix.array, c(1,2), function(x)quantile(x, c(0.025, 0.975)))

# function to add up the total number of matrices and then find the average
# using q.list for length here

array_total <- function(matrix.array, matrices){
  sum <-0
  for (i in 1:length(matrices)){
    sum <- sum + matrix.array[, , i]}
  return(sum)
}

# total of all matrices

array_total(matrix.array, matrices)

# average out over 100 runs:

average <-array_total(matrix.array, matrices)/length(matrices)





