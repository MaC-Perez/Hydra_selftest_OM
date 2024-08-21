source("OM_MUST/pMSE/get_hydra_data_GB_5bin_1978_inpN_noM1.R")
source("OM_MUST/pMSE/get_hydra.R")
source("functions/hydra/read.report.R")
library(tidyr)
library(here)
library(R2admb)
library(dplyr)


#newdata=list(bs_temp=c(),F_full=c(),rec_devs=c())

#hydra_data<- get_hydra(MSEyr)  


newdata <- list(
  bs_temp = c(15,30), 
  F_full = c(),
  rec_devs = c())

result <- get_hydra(newseed=404, newdata=newdata)
