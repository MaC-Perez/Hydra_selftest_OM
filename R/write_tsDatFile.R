write_tsDatFile <- function(dataList,listOfParameters) {
  sim<-nsim
  outPath <- paste0(listOfParameters$outDir,"/",listOfParameters$outputFilename)
  outputFileName <-  paste0(outPath,sim,"-ts.dat")
  # write explanation of how this file was formed
  cat("# This file was created using the write_tsDatFile function and used all the simulated data from sim_dta.rds found in repo:Hydra-self-testing",
      file=outputFileName,fill=listOfParameters$fillLength)

  # observed (survey) biomass
  cat("# Survey index data ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Number of observations, init_int Nsurvey_obs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nsurvey_obs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_matrix obs_survey_biomass(1,Nsurvey_obs,1,5)",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# survey year spp value cv",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Nsurvey_obs){
    cat(c(" ",as.matrix(dataList$observedBiomass[idata,])),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("# end survey index observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # observed survey length composition
  cat("# Survey size comp data ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Number of observations, init_int Nsurvey_size_obs ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Nsurvey_size_obs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_matrix obs_survey_size(1,Nsurvey_size_obs,1,ncol) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Survey  Year  Species Type (0=whole, 1=retained, 2=discard) InpN, proportion by length bin ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Nsurvey_size_obs){
    cat(c(" ",as.matrix(dataList$observedSurvSize[idata,])),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("# end survey length observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # observed fishery catch
  cat("# Fishery catch data ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Number of observations, init_int Ncatch_obs",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Ncatch_obs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_matrix obs_catch_biomass(1,Ncatch_obs,1,6) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Fleet	Area	Year	Species	Catch(biomass t)	CV ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Ncatch_obs){
    cat(c(" ",as.matrix(dataList$observedCatch[idata,])),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("# end catch observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # observed fishery length composition
  cat("# Fishery size comp data ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Number of observations, init_int Ncatch_size_obs ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Ncatch_size_obs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_matrix obs_catch_size(1,Ncatch_size_obs,1,ncol) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Fleet Area  Year  Species Type (0=whole, 1=retained, 2=discard) InpN, proportion by size bin ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Ncatch_size_obs){
    cat(c(" ",as.matrix(dataList$observedCatchSize[idata,])),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("# end fishery length observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

  # observed survey diet composition
  cat("# Survey diet proportion observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Number of observations, init_int Ndietprop_obs ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat(c(" ",dataList$Ndietprop_obs),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# init_matrix obs_dietprop(1,Ndietprop_obs,1,ncol) ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("# Survey  Year  Predator  Predator_size InpN  wt_prey_1 wt_prey_2 wt_prey_3 wt_prey_4 wt_prey_5 wt_prey_6 wt_prey_7 wt_prey_8 wt_prey_9 wt_prey_10 wt_prey_11 other ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  for (idata in 1:dataList$Ndietprop_obs){
    cat(c(" ",as.matrix(dataList$observedSurvDiet[idata,])),file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  }
  cat("# end survey diet proportion observations ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)
  cat("#  ",file=outputFileName,fill=listOfParameters$fillLength,append=TRUE)

}
