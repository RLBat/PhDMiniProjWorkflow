## This R file is used to run a multi-state Markov model on the panel data without the EW category

# clear workspace
rm(list=ls())

# require msm package
require(msm)
require(dplyr)

# read in data from csv files 

df <- read.csv("../Data/msm_noEW.csv", header = TRUE) # 104490 obs 

# in these cases, for purposes of msm modelling, recode 7s as 6s (model needs consecutive numbers)

df$msm[df$msm==7] <-6

#summarise changes between pairs of consecutive states in a state table

statetable.msm(msm, name, data=df)

# define Q to show msm what the allowed transitions of the model are, containing zeros where the entries of Q are 0.
# The underlying model specifies that rather than (for example) jumping from state 1 to 3, state 2 must have been passed through


Q <- rbind(c(0, 0.25, 0, 0, 0, 0.25),
           c(0.166, 0, 0.166, 0, 0, 0.166),
           c(0, 0.166, 0, 0.166, 0, 0.166),
           c(0, 0, 0.166, 0, 0.166, 0.166),
           c(0, 0, 0, 0.25, 0, 0.25),
           c(0, 0, 0, 0, 0, 0))
row.names(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q) <- c("LC", "NT", "VU", "EN", "CR", "EX")


# Now to fit the model which is the process of finding values of the unknown transition intensities which maximise the likelihood

# specify initial values - crude estimate

Q.crude <- crudeinits.msm(msm ~ TSFO, name, data=df, qmatrix = Q)

save(Q.crude, file="../Data/Qcrude_msm.Rdata")
load("../Data/Qcrude_msm.Rdata")
# fit a simple bi-directional model - "death" is not assumed to be recorded exactly

#changes.msm <- msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.crude, death = 6, control=list(fnscale=60000,maxit=500, trace=1, REPORT=1))

changes.msm <- msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.crude, control=list(fnscale=60000,maxit=500, trace=1, REPORT=1))

# save file for use later

save(changes.msm, file = "../Data/changes_msm_nocensor.Rdata")
load("../Data/changes_msm_nocensor.Rdata")

# transition matrices
#p20 <-pmatrix.msm(changes.msm, t=20)
#p50 <- pmatrix.msm(changes.msm, t=50)
p100 <- pmatrix.msm(changes.msm, t=100)
save(p100, file = "../Data/p100OVERALL.Rdata")
#p250 <- pmatrix.msm(changes.msm, t=250)
#p20ci <-pmatrix.msm(changes.msm, t=20, ci="normal")
#p50ci <- pmatrix.msm(changes.msm, t=50, ci="normal")
p100ci <-pmatrix.msm(changes.msm, t=100, ci="normal")
save(p100ci, file="../Data/p100OVERALLnormalci.Rdata")
#p250ci <- pmatrix.msm(changes.msm, t=250, ci="normal")
#p500ci <- pmatrix.msm(changes.msm, t=500, ci="normal")
#p750ci <- pmatrix.msm(changes.msm, t=750, ci="normal")
#p1000ci <- pmatrix.msm(changes.msm, t=1000, ci="normal")

p100boot <- pmatrix.msm(changes.msm, t=100, ci="boot", B=100)
save(p100boot, file = "../Data/p100OVERALLbootci.Rdata")
# following hashed out as they take a very long time to run
# they have been saved and can be re-loaded

#p5_boot <- pmatrix.msm(changes.msm, t=5, ci="bootstrap", B=100)
#save(p5_boot, "../Data/p5_boot.Rdata")
#p20_boot <- pmatrix.msm(changes.msm, t=20, ci="bootstrap", B=100)
#save(p20_boot, "../Data/p20_boot.Rdata")
#p50_boot <- pmatrix.msm(changes.msm, t=50, ci="bootstrap", B=100)
#save(p50_boot, "../Data/p50_boot.Rdata")
#p100_boot <- pmatrix.msm(changes.msm, t=100, ci="bootstrap", B=100)
#save(p100_boot, "../Data/p100_boot.Rdata")
#p250_boot <- pmatrix.msm(changes.msm, t=250, ci="bootstrap", B=100)
#save(p250_boot, "../Data/p250_boot.Rdata")

load("../Data/p5_boot.Rdata")
load("../Data/p20_boot.Rdata")
load("../Data/p50_boot.Rdata")
load("../Data/p100_boot.Rdata")
load("../Data/p250_boot.Rdata")


## Note that msm_diffQ.R also runs the msm model with many different matrices based on the esitmates in Mooers et al(2008) and other matrices


## First, mean sojourn times, ie estimated mean sojourn time in each transient state (average period in a single stay)

sojourn.msm(changes.msm)

## Second, the probability that each state is next

pnext.msm(changes.msm)

## total length of stay

totlos.msm(changes.msm)

## Survival plot
plot(changes.msm, range = c(0, 500), legend.pos = c(0.1, 0.5))
title(main = "Expected probability of survival \nfrom each initial state")
#### Model assessment####

## observed and expected prevalence

prevalence.msm(changes.msm)

plot.prevalence.msm(changes.msm, mintime = 0, maxtime = 20, xlab = "Time in years", col.obs = "black", col.exp = "red")


## Pearson-type goodness-of-fit test

pearson.msm(changes.msm)


#######

# threatened /non-threatened model
df$msm[df$msm==2] <-1
df$msm[df$msm==3] <-2
df$msm[df$msm==4] <-3
df$msm[df$msm==5] <-4
df$msm[df$msm==6] <-5

Q2 <- rbind(c(0, 0.25, 0, 0, 0.25),
           c(0.166, 0, 0.166, 0, 0.166),
           c(0, 0.166, 0, 0.166, 0.166),
           c(0, 0, 0.25, 0, 0.25),
           c(0, 0, 0, 0, 0))
row.names(Q2) <- c("not", "VU", "EN", "CR", "EX")
colnames(Q2) <- c("not", "VU", "EN", "CR", "EX")

Q2.crude <- crudeinits.msm(msm ~ TSFO, name, data=df, qmatrix = Q2)

changes2.msm <- msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q2.crude, control=list(fnscale=20000, maxit = 500, trace=1, REPORT=1))

pmatrix.msm(changes2.msm, t=100)

######### Bootstrapping##########
##100 and 250 year transition matrices

#q.list <- boot.msm(changes.msm, stat = function(x) list (pmatrix.msm(x, t=100), pmatrix.msm(x, t=250)), B=500)

#save(q.list, file = "../Data/bootstrapping.Rdata")
load("../Data/bootstrapping.Rdata")

## unlist and sum all the 100 year transition matrices to find the average

sum_t100 <-0
for (i in 1:length(q.list)){
  sum_t100 <- sum_t100 + array(unlist(q.list[[i]][1]), dim = c(6, 6, 1))
}

# now average

average_t100 <- sum_t100/length(q.list)
average_t100

## now unlist and sum all the 250 year transition matrices to find the average

sum_t250 <-0
for (i in 1:length(q.list)){
  sum_t250 <- sum_t250 + array(unlist(q.list[[i]][2]), dim = c(6, 6, 1))
}

# now average

average_t250 <- sum_t250/length(q.list)
average_t250


## playing around with plotting
av_t100 <- data.frame(average_t100)
colnames(av_t100) <- c("LC", "NT", "VU", "EN", "CR", "EX")
row.names(av_t100) <- c("LC", "NT", "VU", "EN", "CR", "EX")
#av_t100 <- av_t100[1:5,]
#av_t100 <- av_t100[,6]


plot(p250[,6][1:5], type="l", col="red", xaxt="n", xlab = "Red List category", ylab = "probability of being in each state")
title(main = "transition probabilities")
axis(1, at=1:5, labels=c("LC", "NT", "VU", "EN", "CR"))
par(new=TRUE)
lines(p100[,6][1:5], type = "l", col="blue")
lines(p50[,6][1:5], type = "l", col ="green")
lines(p20[,6][1:5], type = "l", col ="brown")




require(ggplot2)
require(reshape2)
meltR = melt(av_t100)
ggplot(meltR, aes(x=EX, y = value, group = variable, colour = variable)) +   geom_line()

#plot(average_t100[,,1][,6][1:5], type="l", col="red", xaxt="n", xlab = "Red List category", ylab = "probability of being in each state")
#title(main = "100 year transition probabilities")
#axis(1, at=1:5, labels=c("LC", "NT", "VU", "EN", "CR"))
#par(new=TRUE)
#lines(average_t100[,,1][,5][1:5], type = "l", col="blue")
#lines(average_t100[,,1][,4][1:5], type = "l", col ="green")
#lines(average_t100[,,1][,3][1:5], type = "l", col ="brown")
#lines(average_t100[,,1][,2][1:5], type = "l", col ="black")
#lines(average_t100[,,1][,1][1:5], type = "l", col ="yellow")
#average_t100

#plot(average_t100[,,1][5,], type="l", col="red")
#par(new=TRUE)
#lines(average_t100[,,1][4,], type = "l", col="blue")
#lines(average_t100[,,1][3,], type = "l", col ="green")
#lines(average_t100[,,1][2,], type = "l", col ="brown")
#lines(average_t100[,,1][1,], type = "l", col ="black")


#plot(average_t250[,,1][,6][1:5], type="l", col="red")
#par(new=TRUE)
#lines(average_t250[,,1][,5][1:5], type = "l", col="blue")
#lines(average_t250[,,1][,4][1:5], type = "l", col ="green")
#lines(average_t250[,,1][,3][1:5], type = "l", col ="brown")
#lines(average_t250[,,1][,2][1:5], type = "l", col ="black")
#lines(average_t250[,,1][,1][1:5], type = "l", col ="yellow")

require (markovchain)


p100_new <-as(p100, "markovchain")

pdf("../Results/100yr.pdf")
plot(p100_new, 
     main="Underlying 100 year transition graph",
     edge.arrow.size=.5, 
     edge.color="red",
     edge.curved=0.2,
     vertex.label.color="black", 
     vertex.size=20, 
     mark.groups=c(5,6),  
     mark.col="#C5E5E7", 
     layout=l)
dev.off()

p250_new <-as(p250, "markovchain")

pdf("../Results/250yr.pdf")
plot(p250_new, 
     main="Underlying 250 year transition graph",
     edge.arrow.size=.5, 
     edge.color="red",
     edge.curved=0.2,
     vertex.label.color="black", 
     vertex.size=20, 
     mark.groups=c(5,6),  
     mark.col="#C5E5E7", 
     layout=l)
dev.off()

require(igraph)

p100_new_igraph <- as(p100_new, "igraph")

plot(p100_new_igraph, weighted=TRUE)
p100_new_igraph <- simplify(p100_new_igraph,remove.multiple = F, remove.loops = T)

plot(p100_new_igraph, ain="100 year transition matrix",edge.arrow.size=.5, edge.color="red",
     vertex.color="blue", vertex.label.color="black", edge.curved=0.3, edge.label.ces=0.3, vertex.size=20, mark.groups=c(5,6),  mark.col="#C5E5E7", layout=l)
l <- layout_in_circle(p100_new_igraph)
E(p100_new_igraph)
#plot(p100_new_igraph, layout=l)


pdf("../Results/survival.pdf")
plot(changes.msm, range = c(0, 500), legend.pos = c(0, 0.25))
title(main = "Expected probability of survival \nfrom each initial state")
dev.off()




sim.df <- data.frame(subject=rep(1:100, rep(13,100)), time=rep(seq(0, 24, 2), 100))
test <-simmulti.msm(sim.df, Q.crude, death = 6, start = c(1:5))
