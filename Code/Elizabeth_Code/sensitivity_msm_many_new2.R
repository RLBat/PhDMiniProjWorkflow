## This R file is used to test the sensitivity of the multi-state Markov model on the panel data without the EW category

## This file updates the original so that I am not averaging the results of each run, but am saving a list of 100 matrices each time

# clear workspace
rm(list=ls())

# require msm package
require(msm)
require(dplyr)

# run


for (i in 1:100){
  
  load(paste("../Manydf/msm_noEW", i, ".Rdata", sep = ""))
  
  # in these cases, for purposes of msm modelling, recode 7s as 6s (model needs consecutive numbers)
  
  
  df$msm[df$msm==7] <-6
  
  
  # I want to randomly sample by species not by row in the table (as just taking individual rows lead to problems in the state table, eg moves in and out of extinct, and also many species with only one entry which need excluding etc)
  
  # now randomly sample the names list and take, say 25%, a number of times
  
  
  #names <-lapply(1:100, function(i){
  #  df_names <- c(sample(unique(df$name), 0.25*length(unique(df$name###))))})
  
  # depite many alternatives the following has proved the fastest way to run this script (!)
  
  df1_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df1 <- df[df$name %in% df1_names, ]
  df2_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df2 <- df[df$name %in% df2_names, ]
  df3_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df3 <- df[df$name %in% df3_names, ]
  df4_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df4 <- df[df$name %in% df4_names, ]
  df5_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df5 <- df[df$name %in% df5_names, ]
  df6_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df6 <- df[df$name %in% df6_names, ]
  df7_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df7 <- df[df$name %in% df7_names, ]
  df8_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df8 <- df[df$name %in% df8_names, ]
  df9_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df9 <- df[df$name %in% df9_names, ]
  df10_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df10 <- df[df$name %in% df10_names, ]
  
  df11_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df11 <- df[df$name %in% df11_names, ]
  df12_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df12 <- df[df$name %in% df12_names, ]
  df13_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df13 <- df[df$name %in% df13_names, ]
  df14_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df14 <- df[df$name %in% df14_names, ]
  df15_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df15 <- df[df$name %in% df15_names, ]
  df16_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df16 <- df[df$name %in% df16_names, ]
  df17_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df17 <- df[df$name %in% df17_names, ]
  df18_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df18 <- df[df$name %in% df18_names, ]
  df19_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df19 <- df[df$name %in% df19_names, ]
  df20_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df20 <- df[df$name %in% df20_names, ]
  
  df21_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df21 <- df[df$name %in% df21_names, ]
  df22_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df22 <- df[df$name %in% df22_names, ]
  df23_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df23 <- df[df$name %in% df23_names, ]
  df24_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df24 <- df[df$name %in% df24_names, ]
  df25_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df25 <- df[df$name %in% df25_names, ]
  df26_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df26 <- df[df$name %in% df26_names, ]
  df27_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df27 <- df[df$name %in% df27_names, ]
  df28_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df28 <- df[df$name %in% df28_names, ]
  df29_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df29 <- df[df$name %in% df29_names, ]
  df30_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df30 <- df[df$name %in% df30_names, ]
  
  df31_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df31 <- df[df$name %in% df31_names, ]
  df32_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df32 <- df[df$name %in% df32_names, ]
  df33_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df33 <- df[df$name %in% df33_names, ]
  df34_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df34 <- df[df$name %in% df34_names, ]
  df35_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df35 <- df[df$name %in% df35_names, ]
  df36_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df36 <- df[df$name %in% df36_names, ]
  df37_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df37 <- df[df$name %in% df37_names, ]
  df38_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df38 <- df[df$name %in% df38_names, ]
  df39_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df39 <- df[df$name %in% df39_names, ]
  df40_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df40 <- df[df$name %in% df40_names, ]
  
  df41_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df41 <- df[df$name %in% df41_names, ]
  df42_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df42 <- df[df$name %in% df42_names, ]
  df43_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df43 <- df[df$name %in% df43_names, ]
  df44_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df44 <- df[df$name %in% df44_names, ]
  df45_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df45 <- df[df$name %in% df45_names, ]
  df46_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df46 <- df[df$name %in% df46_names, ]
  df47_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df47 <- df[df$name %in% df47_names, ]
  df48_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df48 <- df[df$name %in% df48_names, ]
  df49_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df49 <- df[df$name %in% df49_names, ]
  df50_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df50 <- df[df$name %in% df50_names, ]
  
  
  df51_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df51 <- df[df$name %in% df51_names, ]
  df52_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df52 <- df[df$name %in% df52_names, ]
  df53_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df53 <- df[df$name %in% df53_names, ]
  df54_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df54 <- df[df$name %in% df54_names, ]
  df55_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df55 <- df[df$name %in% df55_names, ]
  df56_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df56 <- df[df$name %in% df56_names, ]
  df57_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df57 <- df[df$name %in% df57_names, ]
  df58_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df58 <- df[df$name %in% df58_names, ]
  df59_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df59 <- df[df$name %in% df59_names, ]
  df60_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df60 <- df[df$name %in% df60_names, ]
  
  df61_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df61 <- df[df$name %in% df61_names, ]
  df62_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df62 <- df[df$name %in% df62_names, ]
  df63_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df63 <- df[df$name %in% df63_names, ]
  df64_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df64 <- df[df$name %in% df64_names, ]
  df65_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df65 <- df[df$name %in% df65_names, ]
  df66_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df66 <- df[df$name %in% df66_names, ]
  df67_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df67 <- df[df$name %in% df67_names, ]
  df68_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df68 <- df[df$name %in% df68_names, ]
  df69_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df69 <- df[df$name %in% df69_names, ]
  df70_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df70 <- df[df$name %in% df70_names, ]
  
  
  df71_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df71 <- df[df$name %in% df71_names, ]
  df72_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df72 <- df[df$name %in% df72_names, ]
  df73_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df73 <- df[df$name %in% df73_names, ]
  df74_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df74 <- df[df$name %in% df74_names, ]
  df75_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df75 <- df[df$name %in% df75_names, ]
  df76_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df76 <- df[df$name %in% df76_names, ]
  df77_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df77 <- df[df$name %in% df77_names, ]
  df78_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df78 <- df[df$name %in% df78_names, ]
  df79_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df79 <- df[df$name %in% df79_names, ]
  df80_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df80 <- df[df$name %in% df80_names, ]
  
  df81_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df81 <- df[df$name %in% df81_names, ]
  df82_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df82 <- df[df$name %in% df82_names, ]
  df83_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df83 <- df[df$name %in% df83_names, ]
  df84_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df84 <- df[df$name %in% df84_names, ]
  df85_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df85 <- df[df$name %in% df85_names, ]
  df86_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df86 <- df[df$name %in% df86_names, ]
  df87_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df87 <- df[df$name %in% df87_names, ]
  df88_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df88 <- df[df$name %in% df88_names, ]
  df89_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df89 <- df[df$name %in% df89_names, ]
  df90_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df90 <- df[df$name %in% df90_names, ]
  
  
  df91_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df91 <- df[df$name %in% df91_names, ]
  df92_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df92 <- df[df$name %in% df92_names, ]
  df93_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df93 <- df[df$name %in% df93_names, ]
  df94_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df94 <- df[df$name %in% df94_names, ]
  df95_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df95 <- df[df$name %in% df95_names, ]
  df96_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df96 <- df[df$name %in% df96_names, ]
  df97_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df97 <- df[df$name %in% df97_names, ]
  df98_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df98 <- df[df$name %in% df98_names, ]
  df99_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df99 <- df[df$name %in% df99_names, ]
  df100_names <- sample(unique(df$name), 0.25*length(unique(df$name)))
  df100 <- df[df$name %in% df100_names, ]
  
  # load Q.crude from overall model
  
  load("../Data/Qcrude_msm.Rdata")
  
  
  # Now to fit the model which is the process of finding values of the unknown transition intensities which maximise the likelihood
  
  # fit a simple bi-directional model
  
  changes1.msm <- msm(msm ~ TSFO, subject = name, data = df1, qmatrix = Q.crude, control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes2.msm <- msm(msm ~ TSFO, subject = name, data = df2, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes3.msm <- msm(msm ~ TSFO, subject = name, data = df3, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes4.msm <- msm(msm ~ TSFO, subject = name, data = df4, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes5.msm <- msm(msm ~ TSFO, subject = name, data = df5, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes6.msm <- msm(msm ~ TSFO, subject = name, data = df6, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes7.msm <- msm(msm ~ TSFO, subject = name, data = df7, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes8.msm <- msm(msm ~ TSFO, subject = name, data = df8, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes9.msm <- msm(msm ~ TSFO, subject = name, data = df9, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes10.msm <- msm(msm ~ TSFO, subject = name, data = df10, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes11.msm <- msm(msm ~ TSFO, subject = name, data = df11, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes12.msm <- msm(msm ~ TSFO, subject = name, data = df12, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes13.msm <- msm(msm ~ TSFO, subject = name, data = df13, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes14.msm <- msm(msm ~ TSFO, subject = name, data = df14, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes15.msm <- msm(msm ~ TSFO, subject = name, data = df15, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes16.msm <- msm(msm ~ TSFO, subject = name, data = df16, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes17.msm <- msm(msm ~ TSFO, subject = name, data = df17, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes18.msm <- msm(msm ~ TSFO, subject = name, data = df18, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes19.msm <- msm(msm ~ TSFO, subject = name, data = df19, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes20.msm <- msm(msm ~ TSFO, subject = name, data = df20, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes21.msm <- msm(msm ~ TSFO, subject = name, data = df21, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes22.msm <- msm(msm ~ TSFO, subject = name, data = df22, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes23.msm <- msm(msm ~ TSFO, subject = name, data = df23, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes24.msm <- msm(msm ~ TSFO, subject = name, data = df24, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes25.msm <- msm(msm ~ TSFO, subject = name, data = df25, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes26.msm <- msm(msm ~ TSFO, subject = name, data = df26, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes27.msm <- msm(msm ~ TSFO, subject = name, data = df27, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes28.msm <- msm(msm ~ TSFO, subject = name, data = df28, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes29.msm <- msm(msm ~ TSFO, subject = name, data = df29, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes30.msm <- msm(msm ~ TSFO, subject = name, data = df30, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes31.msm <- msm(msm ~ TSFO, subject = name, data = df31, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes32.msm <- msm(msm ~ TSFO, subject = name, data = df32, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes33.msm <- msm(msm ~ TSFO, subject = name, data = df33, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes34.msm <- msm(msm ~ TSFO, subject = name, data = df34, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes35.msm <- msm(msm ~ TSFO, subject = name, data = df35, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes36.msm <- msm(msm ~ TSFO, subject = name, data = df36, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes37.msm <- msm(msm ~ TSFO, subject = name, data = df37, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes38.msm <- msm(msm ~ TSFO, subject = name, data = df38, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes39.msm <- msm(msm ~ TSFO, subject = name, data = df39, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes40.msm <- msm(msm ~ TSFO, subject = name, data = df40, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes41.msm <- msm(msm ~ TSFO, subject = name, data = df41, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes42.msm <- msm(msm ~ TSFO, subject = name, data = df42, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes43.msm <- msm(msm ~ TSFO, subject = name, data = df43, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes44.msm <- msm(msm ~ TSFO, subject = name, data = df44, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes45.msm <- msm(msm ~ TSFO, subject = name, data = df45, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes46.msm <- msm(msm ~ TSFO, subject = name, data = df46, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes47.msm <- msm(msm ~ TSFO, subject = name, data = df47, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes48.msm <- msm(msm ~ TSFO, subject = name, data = df48, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes49.msm <- msm(msm ~ TSFO, subject = name, data = df49, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes50.msm <- msm(msm ~ TSFO, subject = name, data = df50, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes51.msm <- msm(msm ~ TSFO, subject = name, data = df51, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes52.msm <- msm(msm ~ TSFO, subject = name, data = df52, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes53.msm <- msm(msm ~ TSFO, subject = name, data = df53, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes54.msm <- msm(msm ~ TSFO, subject = name, data = df54, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes55.msm <- msm(msm ~ TSFO, subject = name, data = df55, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes56.msm <- msm(msm ~ TSFO, subject = name, data = df56, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes57.msm <- msm(msm ~ TSFO, subject = name, data = df57, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes58.msm <- msm(msm ~ TSFO, subject = name, data = df58, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes59.msm <- msm(msm ~ TSFO, subject = name, data = df59, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes60.msm <- msm(msm ~ TSFO, subject = name, data = df60, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes61.msm <- msm(msm ~ TSFO, subject = name, data = df61, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes62.msm <- msm(msm ~ TSFO, subject = name, data = df62, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes63.msm <- msm(msm ~ TSFO, subject = name, data = df63, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes64.msm <- msm(msm ~ TSFO, subject = name, data = df64, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes65.msm <- msm(msm ~ TSFO, subject = name, data = df65, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes66.msm <- msm(msm ~ TSFO, subject = name, data = df66, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes67.msm <- msm(msm ~ TSFO, subject = name, data = df67, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes68.msm <- msm(msm ~ TSFO, subject = name, data = df68, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes69.msm <- msm(msm ~ TSFO, subject = name, data = df69, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes70.msm <- msm(msm ~ TSFO, subject = name, data = df70, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes71.msm <- msm(msm ~ TSFO, subject = name, data = df71, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes72.msm <- msm(msm ~ TSFO, subject = name, data = df72, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes73.msm <- msm(msm ~ TSFO, subject = name, data = df73, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes74.msm <- msm(msm ~ TSFO, subject = name, data = df74, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes75.msm <- msm(msm ~ TSFO, subject = name, data = df75, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes76.msm <- msm(msm ~ TSFO, subject = name, data = df76, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes77.msm <- msm(msm ~ TSFO, subject = name, data = df77, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes78.msm <- msm(msm ~ TSFO, subject = name, data = df78, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes79.msm <- msm(msm ~ TSFO, subject = name, data = df79, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes80.msm <- msm(msm ~ TSFO, subject = name, data = df80, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes81.msm <- msm(msm ~ TSFO, subject = name, data = df81, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes82.msm <- msm(msm ~ TSFO, subject = name, data = df82, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes83.msm <- msm(msm ~ TSFO, subject = name, data = df83, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes84.msm <- msm(msm ~ TSFO, subject = name, data = df84, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes85.msm <- msm(msm ~ TSFO, subject = name, data = df85, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes86.msm <- msm(msm ~ TSFO, subject = name, data = df86, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes87.msm <- msm(msm ~ TSFO, subject = name, data = df87, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes88.msm <- msm(msm ~ TSFO, subject = name, data = df88, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes89.msm <- msm(msm ~ TSFO, subject = name, data = df89, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes90.msm <- msm(msm ~ TSFO, subject = name, data = df90, qmatrix = Q.crude, control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes91.msm <- msm(msm ~ TSFO, subject = name, data = df91, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes92.msm <- msm(msm ~ TSFO, subject = name, data = df92, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes93.msm <- msm(msm ~ TSFO, subject = name, data = df93, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes94.msm <- msm(msm ~ TSFO, subject = name, data = df94, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes95.msm <- msm(msm ~ TSFO, subject = name, data = df95, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes96.msm <- msm(msm ~ TSFO, subject = name, data = df96, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes97.msm <- msm(msm ~ TSFO, subject = name, data = df97, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes98.msm <- msm(msm ~ TSFO, subject = name, data = df98, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes99.msm <- msm(msm ~ TSFO, subject = name, data = df99, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  changes100.msm <- msm(msm ~ TSFO, subject = name, data = df100, qmatrix = Q.crude,  control=list(fnscale=15000,maxit=500, trace=1, REPORT=1))
  
  
  a1 <-pmatrix.msm(changes1.msm, t=100)
  b1 <-pmatrix.msm(changes2.msm, t=100)
  c1 <-pmatrix.msm(changes3.msm, t=100)
  d1 <-pmatrix.msm(changes4.msm, t=100)
  e1 <-pmatrix.msm(changes5.msm, t=100)
  f1 <-pmatrix.msm(changes6.msm, t=100)
  g1 <-pmatrix.msm(changes7.msm, t=100)
  h1 <-pmatrix.msm(changes8.msm, t=100)
  i1 <-pmatrix.msm(changes9.msm, t=100)
  j1 <-pmatrix.msm(changes10.msm, t=100)
  
  a2 <-pmatrix.msm(changes11.msm, t=100)
  b2 <-pmatrix.msm(changes12.msm, t=100)
  c2 <-pmatrix.msm(changes13.msm, t=100)
  d2 <-pmatrix.msm(changes14.msm, t=100)
  e2 <-pmatrix.msm(changes15.msm, t=100)
  f2 <-pmatrix.msm(changes16.msm, t=100)
  g2 <-pmatrix.msm(changes17.msm, t=100)
  h2 <-pmatrix.msm(changes18.msm, t=100)
  i2 <-pmatrix.msm(changes19.msm, t=100)
  j2 <-pmatrix.msm(changes20.msm, t=100)
  
  a3 <-pmatrix.msm(changes21.msm, t=100)
  b3 <-pmatrix.msm(changes22.msm, t=100)
  c3 <-pmatrix.msm(changes23.msm, t=100)
  d3 <-pmatrix.msm(changes24.msm, t=100)
  e3 <-pmatrix.msm(changes25.msm, t=100)
  f3 <-pmatrix.msm(changes26.msm, t=100)
  g3 <-pmatrix.msm(changes27.msm, t=100)
  h3 <-pmatrix.msm(changes28.msm, t=100)
  i3 <-pmatrix.msm(changes29.msm, t=100)
  j3 <-pmatrix.msm(changes30.msm, t=100)
  
  a4 <-pmatrix.msm(changes31.msm, t=100)
  b4 <-pmatrix.msm(changes32.msm, t=100)
  c4 <-pmatrix.msm(changes33.msm, t=100)
  d4 <-pmatrix.msm(changes34.msm, t=100)
  e4 <-pmatrix.msm(changes35.msm, t=100)
  f4 <-pmatrix.msm(changes36.msm, t=100)
  g4 <-pmatrix.msm(changes37.msm, t=100)
  h4 <-pmatrix.msm(changes38.msm, t=100)
  i4 <-pmatrix.msm(changes39.msm, t=100)
  j4 <-pmatrix.msm(changes40.msm, t=100)
  
  a5 <-pmatrix.msm(changes41.msm, t=100)
  b5 <-pmatrix.msm(changes42.msm, t=100)
  c5 <-pmatrix.msm(changes43.msm, t=100)
  d5 <-pmatrix.msm(changes44.msm, t=100)
  e5 <-pmatrix.msm(changes45.msm, t=100)
  f5 <-pmatrix.msm(changes46.msm, t=100)
  g5 <-pmatrix.msm(changes47.msm, t=100)
  h5 <-pmatrix.msm(changes48.msm, t=100)
  i5 <-pmatrix.msm(changes49.msm, t=100)
  j5 <-pmatrix.msm(changes50.msm, t=100)
  
  a6 <-pmatrix.msm(changes51.msm, t=100)
  b6 <-pmatrix.msm(changes52.msm, t=100)
  c6 <-pmatrix.msm(changes53.msm, t=100)
  d6 <-pmatrix.msm(changes54.msm, t=100)
  e6 <-pmatrix.msm(changes55.msm, t=100)
  f6 <-pmatrix.msm(changes56.msm, t=100)
  g6 <-pmatrix.msm(changes57.msm, t=100)
  h6 <-pmatrix.msm(changes58.msm, t=100)
  i6 <-pmatrix.msm(changes59.msm, t=100)
  j6 <-pmatrix.msm(changes60.msm, t=100)
  
  a7 <-pmatrix.msm(changes61.msm, t=100)
  b7 <-pmatrix.msm(changes62.msm, t=100)
  c7 <-pmatrix.msm(changes63.msm, t=100)
  d7 <-pmatrix.msm(changes64.msm, t=100)
  e7 <-pmatrix.msm(changes65.msm, t=100)
  f7 <-pmatrix.msm(changes66.msm, t=100)
  g7 <-pmatrix.msm(changes67.msm, t=100)
  h7 <-pmatrix.msm(changes68.msm, t=100)
  i7 <-pmatrix.msm(changes69.msm, t=100)
  j7 <-pmatrix.msm(changes70.msm, t=100)
  
  a8 <-pmatrix.msm(changes71.msm, t=100)
  b8 <-pmatrix.msm(changes72.msm, t=100)
  c8 <-pmatrix.msm(changes73.msm, t=100)
  d8 <-pmatrix.msm(changes74.msm, t=100)
  e8 <-pmatrix.msm(changes75.msm, t=100)
  f8 <-pmatrix.msm(changes76.msm, t=100)
  g8 <-pmatrix.msm(changes77.msm, t=100)
  h8 <-pmatrix.msm(changes78.msm, t=100)
  i8 <-pmatrix.msm(changes79.msm, t=100)
  j8 <-pmatrix.msm(changes80.msm, t=100)
  
  a9 <-pmatrix.msm(changes81.msm, t=100)
  b9 <-pmatrix.msm(changes82.msm, t=100)
  c9 <-pmatrix.msm(changes83.msm, t=100)
  d9 <-pmatrix.msm(changes84.msm, t=100)
  e9 <-pmatrix.msm(changes85.msm, t=100)
  f9 <-pmatrix.msm(changes86.msm, t=100)
  g9 <-pmatrix.msm(changes87.msm, t=100)
  h9 <-pmatrix.msm(changes88.msm, t=100)
  i9 <-pmatrix.msm(changes89.msm, t=100)
  j9 <-pmatrix.msm(changes90.msm, t=100)
  
  a10 <-pmatrix.msm(changes91.msm, t=100)
  b10 <-pmatrix.msm(changes92.msm, t=100)
  c10 <-pmatrix.msm(changes93.msm, t=100)
  d10 <-pmatrix.msm(changes94.msm, t=100)
  e10 <-pmatrix.msm(changes95.msm, t=100)
  f10 <-pmatrix.msm(changes96.msm, t=100)
  g10 <-pmatrix.msm(changes97.msm, t=100)
  h10 <-pmatrix.msm(changes98.msm, t=100)
  i10 <-pmatrix.msm(changes99.msm, t=100)
  j10 <-pmatrix.msm(changes100.msm, t=100)
  
  # convert to array and then calculate sd and 95% CIs
  
  matrices <- list(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1,
                   a2, b2, c2, d2, e2, f2, g2, h2, i2, j2,
                   a3, b3, c3, d3, e3, f3, g3, h3, i3, j3,
                   a4, b4, c4, d4, e4, f4, g4, h4, i4, j4,
                   a5, b5, c5, d5, e5, f5, g5, h5, i5, j5,
                   a6, b6, c6, d6, e6, f6, g6, h6, i6, j6,
                   a8, b8, c8, d8, e8, f8, g8, h8, i8, j8,
                   a7, b7, c7, d7, e7, f7, g7, h7, i7, j7,
                   a9, b9, c9, d9, e9, f9, g9, h9, i9, j9,
                   a10, b10, c10, d10, e10, f10, g10, h10, i10, j10)
  
  save(matrices, file = paste0("../Manydf/sensitivity_matrices_", i,".Rdata"))
  

}
