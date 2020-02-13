## This R file is used to run a multi-state Markov model on panel data and use different values for Q

# clear workspace
rm(list=ls())

# require msm package
require(msm)
require(dplyr)

#options(digits = 4)
# read in data from csv files 

df <- read.csv("../Data/msm_noEW.csv", header = TRUE) #104490

# in these cases, for purposes of msm modelling, recode 7s as 6s (model needs consecutive numbers)

df$msm[df$msm==7] <-6


#summarise changes between pairs of consecutive states in a state table

#changes_table <-statetable.msm(msm, name, data=df)

#changes_table

# We have already defined Q and Q.crude in msm.R and here are using figures in Mooers 2008 paper to inform values for extinction probabilities in initial transitions matrices and guesses base on transitions for other entries 


Q.Isaac <- rbind(c(0, 0.01, 0, 0, 0, 0.025),
           c(0.1, 0, 0.05, 0, 0, 0.05),
           c(0, 0.08, 0, 0.1, 0, 0.1),
           c(0, 0, 0.1, 0, 0.1, 0.2),
           c(0, 0, 0, 0.08, 0, 0.4),
           c(0, 0, 0, 0, 0, 0))
row.names(Q.Isaac) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.Isaac) <- c("LC", "NT", "VU", "EN", "CR", "EX")


Q.IUCN100 <- rbind(c(0, 0.01, 0, 0, 0, 0.0001),
                 c(0.1, 0, 0.05, 0, 0, 0.01),
                 c(0, 0.08, 0, 0.1, 0, 0.1),
                 c(0, 0, 0.1, 0, 0.1, 0.667),
                 c(0, 0, 0, 0.08, 0, 0.999),
                 c(0, 0, 0, 0, 0, 0))
row.names(Q.IUCN100) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.IUCN100) <- c("LC", "NT", "VU", "EN", "CR", "EX")


Q.IUCN50 <- rbind(c(0, 0.01, 0, 0, 0, 0.00005),
                   c(0.1, 0, 0.05, 0, 0, 0.004),
                   c(0, 0.08, 0, 0.1, 0, 0.05),
                   c(0, 0, 0.1, 0, 0.1, 0.42),
                   c(0, 0, 0, 0.08, 0, 0.97),
                   c(0, 0, 0, 0, 0, 0))
row.names(Q.IUCN50) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.IUCN50) <- c("LC", "NT", "VU", "EN", "CR", "EX")


Q.IUCN500 <- rbind(c(0, 0.01, 0, 0, 0, 0.0005),
                  c(0.1, 0, 0.05, 0, 0, 0.02),
                  c(0, 0.08, 0, 0.1, 0, 0.39),
                  c(0, 0, 0.1, 0, 0.1, 0.996),
                  c(0, 0, 0, 0.08, 0, 1),
                  c(0, 0, 0, 0, 0, 0))
row.names(Q.IUCN500) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.IUCN500) <- c("LC", "NT", "VU", "EN", "CR", "EX")


Q.Pessimistic <- rbind(c(0, 0.01, 0, 0, 0, 0.2),
                   c(0.1, 0, 0.05, 0, 0, 0.4),
                   c(0, 0.08, 0, 0.1, 0, 0.8),
                   c(0, 0, 0.1, 0, 0.1, 0.9),
                   c(0, 0, 0, 0.08, 0, 0.99),
                   c(0, 0, 0, 0, 0, 0))
row.names(Q.Pessimistic) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.Pessimistic) <- c("LC", "NT", "VU", "EN", "CR", "EX")


# fit simple bi-directional models

changes_Isaac.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.Isaac, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_Isaac.msm, file="../Data/QIsaacmodel.Rdata")

changes_Isaac.msm


changes_IUCN100.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.IUCN100, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_IUCN100.msm, file="../Data/QIUCN100.Rdata")

changes_IUCN100.msm

changes_IUCN50.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.IUCN50, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_IUCN50.msm, file="../Data/QIUCN50.Rdata")

changes_IUCN50.msm

changes_IUCN500.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.IUCN500, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_IUCN500.msm, file="../Data/QIUCN500.Rdata")

changes_IUCN500.msm

changes_Pessimistic.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.Pessimistic, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_Pessimistic.msm, file = "../Data/QPessimistic.Rdata")

changes_Pessimistic.msm


## Also for completeness using Q based on sjourn rates- LC 210yrs, NT, VU, EN = 12 years, CR = 23 years

Q.sojourn <- rbind(c(0, 0.005, 0, 0, 0, 0.001),
           c(0.03, 0, 0.03, 0, 0, 0.03),
           c(0, 0.03, 0, 0.03, 0, 0.03),
           c(0, 0, 0.03, 0, 0.02, 0.03),
           c(0, 0, 0, 0.2, 0, 0.2),
           c(0, 0, 0, 0, 0, 0))
row.names(Q.sojourn) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q.sojourn) <- c("LC", "NT", "VU", "EN", "CR", "EX")

changes_sojourn.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q.sojourn, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_sojourn.msm, file = "../Data/Qsojourn.Rdata")

changes_sojourn.msm


Q_crexonly <- rbind(c(0, 0.25, 0, 0, 0, 0),
           c(0.166, 0, 0.166, 0, 0, 0),
           c(0, 0.166, 0, 0.166, 0, 0),
           c(0, 0, 0.166, 0, 0.166, 0),
           c(0, 0, 0, 0.25, 0, 0.25),
           c(0, 0, 0, 0, 0, 0))
row.names(Q_crexonly) <- c("LC", "NT", "VU", "EN", "CR", "EX")
colnames(Q_crexonly) <- c("LC", "NT", "VU", "EN", "CR", "EX")


changes_crexonly.msm <-msm(msm ~ TSFO, subject = name, data = df, qmatrix = Q_crexonly, control=list(fnscale=57000,maxit=500, trace=1, REPORT=1))
save(changes_crexonly.msm, file="../Data/Qcrexonly.Rdata")

changes_crexonly.msm


load("../Data/changes_msm_nocensor.Rdata")

t100_1 <- pmatrix.msm(changes_Isaac.msm, t=100)
t100_2 <- pmatrix.msm(changes_IUCN50.msm, t=100)
t100_3 <- pmatrix.msm(changes_IUCN100.msm, t=100)
t100_4 <- pmatrix.msm(changes_IUCN500.msm, t=100)
t100_5 <- pmatrix.msm(changes_Pessimistic.msm, t=100)
t100_6 <- pmatrix.msm(changes_sojourn.msm, t=100)
t100_7 <- pmatrix.msm(changes_crexonly.msm, t=100)

DiffQlist <- list(t100_1, t100_2, t100_3, t100_4, t100_5, t100_6, t100_7)

save(DiffQlist, file = "../Data/DiffQmatrices.Rdata")
