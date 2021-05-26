## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)

`%!in%` = Negate(`%in%`)

############################


Bootstrap_msm <- function(msm_model, repeats = 100){
  # The bootstrapping, this takes a while (~15 mins)
  Boot_models <- boot.msm(msm_model, stat = NULL, B=repeats, cores = (detectCores()-1))
}

