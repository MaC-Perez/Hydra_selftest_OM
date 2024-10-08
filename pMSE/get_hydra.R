get_hydra <- function(newseed=404,newdata=list(bs_temp=c(),F_full=c(),rec_devs=c())){
  #send hydra data to groundfish MSE?
  #Packages you need:
  #tidyr
  #here
  #R2admb
  # compile_admb("functions/hydra/hydra_sim")
  
  #This is the number of years the MSE has gone on, will inform Nyears
  MSEyr = length(newdata$bs_temp)
  
  #This needs dummy variables to start the while loop
  #the while loop is needed because sometimes there is indexing issues
  #due to the hydra model not running successfully
  Species <- c(1)
  Biomass <- c(1,2)
  while(length(Species)!=length(Biomass))
  {
  
  hydra_data <- get_hydra_data(MSEyr)
  
  # Turn on debugging, if needed:  
  hydra_data$debug <- 0
  # hydra_data$debug <- 1
  
  #Update hydra data based on this iteration of the MSE
  # Primary dat file items:
  hydra_data$bs_temp <- c(hydra_data$bs_temp,newdata$bs_temp)
  
  fleetdistribute <- list()
  for(i in 1:hydra_data$Nfleets) fleetdistribute[[i]] <- unique(dplyr::filter(hydra_data$obs_catch_biomass, fleet==i)$species)
  
  
  # ADD DUMMY DATA TO HYDRA INPUT FILE:
  if(MSEyr>0)
  {
    for(i in (hydra_data$Nyrs-MSEyr+1):hydra_data$Nyrs)
    {
      for(k in 1:hydra_data$Nspecies)
      {
        for(j in 1:hydra_data$Nsurveys)
        {
          hydra_data$obs_survey_biomass <-rbind(hydra_data$obs_survey_biomass,c(j,i,k,100000,0.5))
          
          # Because inpN has an influence on how the simulated age compositions, get the survey and species specific average inpN in dummy data:
          newinpN <- round(mean(dplyr::filter(hydra_data$obs_survey_size,species==k,survey==j,year<=(hydra_data$Nyrs-MSEyr))[,'inpN']))
          hydra_data$obs_survey_size <- rbind(hydra_data$obs_survey_size,c(j,i,k,0,newinpN,0.2,0.2,0.2,0.2,0.2))
        }
        
      }
      for(j in 1:hydra_data$Nfleets)
      {
        for(k in 1:length(as.numeric(fleetdistribute[[j]])))
        {
          for(l in 1:hydra_data$Nareas)
          {
            hydra_data$obs_catch_biomass <- rbind(hydra_data$obs_catch_biomass,c(j,l,i,fleetdistribute[[j]][k],30000,0.05))
            
            newinpN <- round(mean(dplyr::filter(hydra_data$obs_catch_size,species==fleetdistribute[[j]][k],fleet==j,year<=(hydra_data$Nyrs-MSEyr))[,'inpN']))
            hydra_data$obs_catch_size <- rbind(hydra_data$obs_catch_size,c(j,l,i,fleetdistribute[[j]][k],0,newinpN,0.2,0.2,0.2,0.2,0.2))
          }
        } 
      }
    }
    hydra_data$Nsurvey_obs = nrow(hydra_data$obs_survey_biomass)
    hydra_data$Nsurvey_size_obs = nrow(hydra_data$obs_survey_size)
    hydra_data$Ncatch_obs = nrow(hydra_data$obs_catch_biomass)
    hydra_data$Ncatch_size_obs = nrow(hydra_data$obs_catch_size)
  }
  
  # Add additional years of F data:
  if(!is.null(newdata$F_full))
  {
    F_full <- matrix(newdata$F_full,nrow=MSEyr,ncol=hydra_data$Nfleets, byrow = TRUE)
    F_devs <- F_full*0
    for(i in 1:MSEyr) F_devs[i,] <- log(F_full[i,]+0.001)-hydra_data$avg_F
    hydra_data$F_devs <- as.vector(rbind(t(matrix(hydra_data$F_devs,ncol=(hydra_data$Nyrs-MSEyr), byrow = TRUE)),F_devs))
  }
  
  if(!is.null(newdata$rec_devs))
  {
    rec_devs <- matrix(newdata$rec_devs,nrow=MSEyr,ncol=hydra_data$Nspecies, byrow = TRUE)
    hydra_data$recruitment_devs <- as.vector(t(cbind(matrix(hydra_data$recruitment_devs,nrow=(hydra_data$Nspecies),byrow = TRUE),t(rec_devs))))
  }
  
  if(!is.null(newdata$ln_fishery_q))
  {
   hydra_data$ln_fishery_q <- c(hydra_data$ln_fishery_q, newdata$ln_fishery_q)
  }
  
  #############################################################################
  #Start of Emily's code, it runs hydra and pulls data files:
  #Source the baseline hydra sim data:
  
  
  #Write a new hydra sim file, changing anything before this line
  fileConn<-file("functions/hydra/hydra_sim_data.dat")
  writeLines(as.character(c(hydra_data$debug,hydra_data$Nyrs, hydra_data$Nspecies,hydra_data$Nsizebins,
                            hydra_data$Nareas,hydra_data$Nfleets,hydra_data$Nsurveys,hydra_data$wtconv,
                            hydra_data$datfilename,hydra_data$binwidth,hydra_data$lenwt_a,hydra_data$lenwt_b,
                            hydra_data$Nrecruitment_cov,hydra_data$Nmaturity_cov,hydra_data$Ngrowth_cov,hydra_data$recruitment_cov,
                            hydra_data$maturity_cov,hydra_data$growth_cov,hydra_data$obs_effort,hydra_data$mean_stomwt,
                            hydra_data$bs_temp,hydra_data$yr1Nphase,hydra_data$recphase,hydra_data$avg_rec_phase,
                            hydra_data$recsigmaphase,hydra_data$avg_F_phase,hydra_data$dev_rec_phase,hydra_data$dev_F_phase,
                            hydra_data$fqphase,hydra_data$fsphase,hydra_data$sqphase,hydra_data$ssphase,
                            hydra_data$ssig_phase,hydra_data$sig_phase,hydra_data$m1_phase,hydra_data$oF1_phase,
                            hydra_data$oFdev_phase,hydra_data$vuln_phase,hydra_data$recGamma_alpha,hydra_data$recGamma_shape,
                            hydra_data$recGamma_beta,hydra_data$recDS_alpha,hydra_data$recDS_shape,hydra_data$recDS_beta,
                            hydra_data$recGamSSB_alpha, hydra_data$recGamSSB_shape,hydra_data$recGamSSB_beta,hydra_data$recRicker_alpha,
                            hydra_data$recRicker_shape,hydra_data$recRicker_beta,hydra_data$recBH_alpha,hydra_data$recBH_shape,
                            hydra_data$recBH_beta,hydra_data$recShepherd_alpha,hydra_data$recShepherd_shape,hydra_data$recShepherd_beta,
                            hydra_data$recHockey_alpha,hydra_data$recHockey_shape,hydra_data$recHockey_beta,hydra_data$recSegmented_alpha,
                            hydra_data$recSegmented_shape,hydra_data$recSegmented_beta,hydra_data$rectype,hydra_data$stochrec,
                            hydra_data$sexratio,hydra_data$recruitment_covwt,hydra_data$fecund_d,hydra_data$fecund_h,
                            hydra_data$fecund_theta,hydra_data$maturity_nu,hydra_data$maturity_omega,hydra_data$maturity_covwt,
                            hydra_data$growth_psi,hydra_data$growth_kappa,hydra_data$growth_covwt,hydra_data$vonB_LinF,
                            hydra_data$vonB_k,hydra_data$growthtype,hydra_data$phimax,hydra_data$intake_alpha,
                            hydra_data$intake_beta,hydra_data$isprey,hydra_data$preferred_wtratio,hydra_data$sd_sizepref,
                            hydra_data$B0,hydra_data$Nguilds,hydra_data$guildMembers,hydra_data$fleetMembers,
                            hydra_data$AssessmentPeriod,hydra_data$flagRamp,hydra_data$minExploitation,hydra_data$maxExploitation,
                            hydra_data$minMaxExploitation,hydra_data$minMaxThreshold,hydra_data$Nthresholds,hydra_data$threshold_preportion, 
                            hydra_data$exploitation_levels, hydra_data$threshold_species, hydra_data$AssessmentOn, hydra_data$speciesDetection, 
                            hydra_data$LFI_size, hydra_data$scaleInitialN, hydra_data$effortScaled, hydra_data$discard_Coef, 
                            hydra_data$discardSurvival_Coef, hydra_data$predOrPrey, hydra_data$bandwidth_metric, hydra_data$baseline_threshold, 
                            hydra_data$indicator_fishery_q, hydra_data$AR_parameters, hydra_data$flagMSE, hydra_data$residentTime, 
                            hydra_data$areaMortality, 
                            hydra_data$m1_change_yr,
                            hydra_data$m1_multiplier,
                            hydra_data$of_change_yr,
                            hydra_data$of_multiplier,
                            hydra_data$eof)),fileConn)
  close(fileConn)
  
  fileConn<-file("functions/hydra/hydra_sim_data-ts.dat")
  writeLines(as.character(c(hydra_data$Nsurvey_obs, as.list(t(as.matrix(hydra_data$obs_survey_biomass))), hydra_data$Nsurvey_size_obs,
                            as.list(t(as.matrix(hydra_data$obs_survey_size))), hydra_data$Ncatch_obs, as.list(t(as.matrix(hydra_data$obs_catch_biomass))),
                            hydra_data$Ncatch_size_obs, as.list(t(as.matrix(hydra_data$obs_catch_size))), hydra_data$Ndietprop_obs,
                            as.list(t(as.matrix(hydra_data$obs_dietprop))))),fileConn)
  close(fileConn)
  
  fileConn<-file("functions/hydra/hydra_sim_data.pin")
  writeLines(as.character(c(hydra_data$ln_yr1N, hydra_data$recruitment_alpha, hydra_data$recruitment_shape, 
                            hydra_data$recruitment_beta, hydra_data$ln_avg_recruitment, hydra_data$recruitment_devs, 
                            hydra_data$ln_recsigma, hydra_data$avg_F, hydra_data$F_devs,
                            hydra_data$logit_vuln,
                            hydra_data$fishsel_pars, hydra_data$ln_fishery_q, hydra_data$ln_survey_q, 
                            hydra_data$survey_selpars, hydra_data$ln_M1ann, hydra_data$ln_otherFood_base, 
                            hydra_data$otherFood_dev)),fileConn)
  close(fileConn)
  
  # Random number to run the MSE
  randomnum <- newseed
  # randomnum <- round(randu(1,100),1)
  
  # Set the working directory to where the hydra executable is
  originalwd <- getwd()
  setwd(paste0(originalwd,"/functions/hydra"))
  
  # Run the hydra code
  if(platform == 'Windows')
   runmod <- paste("hydra_sim.exe -sim ",randomnum, "-ind hydra_sim_data.dat -ainp hydra_sim_data.pin -nohess -maxfn 1")
  if(platform != 'Windows')
   runmod <- paste("./hydra_sim -sim ",randomnum, "-ind hydra_sim_data.dat -ainp hydra_sim_data.pin -nohess -maxfn 1")
  out <- system(runmod, intern=T)
  # out <- system(runmod, intern=F)
  
  # Reset the working directory to original
  setwd(originalwd)
  
  #Read the report object generated by admb into R
  hydra_sim_rep <- reptoRlist("functions/hydra/hydra_sim.rep")
  # hydra_sim_rep_df <- reptoRlist("functions/hydra/pmse_predvals.out")
  
  Species=rep(1:hydra_data$Nspecies,each=hydra_data$Nyrs)
  Biomass=apply(hydra_sim_rep$EstBsize,1,sum)
  }

  biomass <- data.frame(Species=rep(1:hydra_data$Nspecies,each=hydra_data$Nyrs),Year=rep(1:hydra_data$Nyrs,hydra_data$Nspecies),Biomass=apply(hydra_sim_rep$EstBsize,1,sum))
  biomass <- cbind(biomass,hydra_sim_rep$EstBsize)
  colnames(biomass)[4:8]<- c("size1","size2","size3","size4","size5")
  abundance <- data.frame(Species=rep(1:hydra_data$Nspecies,each=hydra_data$Nyrs),Year=rep(1:hydra_data$Nyrs,hydra_data$Nspecies),Abundance=apply(hydra_sim_rep$EstNsize,1,sum))
  #Convert general report object into things to be fed back
  survey.df <- hydra_sim_rep$survey
  colnames(survey.df) <- c("survey","year","species","biomass","cv","predbiomass","residual","NLL")
  catch.df <- hydra_sim_rep$catch
  colnames(catch.df) <- c("fleet","area","year","species","catch","cv","predcatch","residual","NLL")
  # survey.df <- as.data.frame(hydra_sim_rep$survey_full)
  # colnames(survey.df) <- c("survey","year","species","area", "predbiomass") #,"residual","NLL")
  # catch.df <- as.data.frame(hydra_sim_rep$fleet_full)
  # colnames(catch.df) <- c("fleet","year","species","area","predcatch") #,"cv","predcatch","residual","NLL")  
  survey.df[,"cv"] <- rep(0.3,nrow(survey.df))
  catch.df[,"cv"] <- rep(0.05,nrow(catch.df))
  
  # Calculate the amount of forage biomass
  foragebiomass <- data.frame(Year= c(1:hydra_data$Nyrs),Biomass=rep(0,hydra_data$Nyrs))
  for(i in 1:nrow(biomass))
  {
    tempspecies <- biomass$Species[i]
    if(tempspecies==2||tempspecies==3)
    {
      foragebiomass$Biomass[biomass$Year[i]] <- foragebiomass$Biomass[biomass$Year[i]]+biomass$Biomass[i]
    }
    if(tempspecies!=2&&tempspecies!=3)
    {
      for(j in c(1:5))
      {
        binmax <- hydra_data$binwidth[(tempspecies-1)*5+1]*j
        binmin <- hydra_data$binwidth[(tempspecies-1)*5+1]*(j-1)+1
        if(binmax<=input$forageminimum) foragebiomass$Biomass[biomass$Year[i]] <- foragebiomass$Biomass[biomass$Year[i]]+biomass[i,(j+3)]
        if(binmax>input$forageminimum && binmin<=input$forageminimum) foragebiomass$Biomass[biomass$Year[i]]<- foragebiomass$Biomass[biomass$Year[i]]+(biomass[i,(j+3)]*(input$forageminimum-binmin)/hydra_data$binwidth[(tempspecies-1)*5+1])
      }
    }
  }
  
  #rearrange to look like data that we need
  species <- data.frame(name=hydra_data$speciesList, species= c(1:10))
  K <-as.numeric(hydra_data$vonB_k) 
  Linf <- as.numeric(hydra_data$vonB_LinF) 
  t0 <- 0 # is there something else I should be using here? t0 probably fine
  vonbert <- data.frame(species,K, Linf,t0)
  
  # size bins
  # binwidth <- hydraDataList_msk$binwidth%>%
  #   cbind(species)
  binwidth <- data.frame(sizebin1=matrix(hydra_data$binwidth,nrow=10,ncol=5,byrow=T)[,1],
                         sizebin2=matrix(hydra_data$binwidth,nrow=10,ncol=5,byrow=T)[,2],
                         sizebin3=matrix(hydra_data$binwidth,nrow=10,ncol=5,byrow=T)[,3],
                         sizebin4=matrix(hydra_data$binwidth,nrow=10,ncol=5,byrow=T)[,4],
                         sizebin5=matrix(hydra_data$binwidth,nrow=10,ncol=5,byrow=T)[,5],
                         name=hydra_data$speciesList,
                         species=hydra_data$guildMembers)
  row.names(binwidth) <- hydra_data$speciesList
  
  sizebin0 <- 0
  sizebin1 <- binwidth$sizebin1
  sizebin2 <- sizebin1 + binwidth$sizebin2 
  sizebin3 <- sizebin2 + binwidth$sizebin3
  sizebin4 <- sizebin3 + binwidth$sizebin4
  sizebin5 <- sizebin4 + binwidth$sizebin5
  sizebin <- as.data.frame(cbind(sizebin0,sizebin1, sizebin2, sizebin3, sizebin4, sizebin5, species))%>%
    full_join(vonbert)
  
  sizebins <- as.data.frame(cbind(sizebin0,sizebin1, sizebin2, sizebin3, sizebin4, sizebin5, species))%>%
    gather(bin, endbin, sizebin0:sizebin5)%>%
    group_by(species)%>%
    mutate(startbin= dplyr::lag(endbin))%>%
    dplyr::filter(!bin %in% c('sizebin0'))
  
  # calculate the number of fish in each bin- Problem this is not an integer
  # observedSurvSize<- hydraDataList_msk$observedSurvSize
  # observedSurvSize<- obs_survey_size
  # for convenience, just overwrite the obs_survey_size with predicted values, since it is already formatted nicely in a df
  predSurvSize <- hydra_data$obs_survey_size
  predSurvSize[,6:10] <- matrix(hydra_sim_rep$pred_survey_size,ncol=5,nrow=hydra_data$Nsurvey_size_obs,byrow=T)
  SurvData <- gather(predSurvSize, bin, prop, sizebin1:sizebin5)%>%
    mutate(N= ceiling(inpN*prop))%>% #fix by rounding?
    full_join(sizebins, by=c("species", "bin"))%>%
    full_join(vonbert)
  
  # observedCatchSize <- hydraDataList_msk$observedCatchSize
  # observedCatchSize <- obs_catch_size
  # for convenience, just overwrite the obs_catch_size with predicted values, since it is already formatted nicely in a df
  predCatchSize <- hydra_data$obs_catch_size
  predCatchSize[,7:11] <- matrix(hydra_sim_rep$pred_catch_size,ncol=5,nrow=nrow(predCatchSize),byrow=T)
  catchData <- gather(predCatchSize, bin, prop, sizebin1:sizebin5)%>%
    mutate(N= ceiling(inpN*prop))%>% #fix by rounding?
    full_join(sizebins, by=c("species", "bin"))%>%
    full_join(vonbert)
  
  Surv_newbin<-SurvData%>%
    rowwise()%>%
    mutate(end = min(endbin, Linf-1), start= min(startbin, Linf-1)) #fix by choosing whichever is smallest?
  Catch_newbin <- catchData%>%
    rowwise()%>%
    mutate(end = min(endbin, Linf-1), start= min(startbin, Linf-1)) 
  
  #repeat rows based on N- each individual has its own row
  SurvRep <- as.data.frame(lapply(Surv_newbin, rep, Surv_newbin$N))
  CatchRep <- as.data.frame(lapply(Catch_newbin, rep, Catch_newbin$N))
  
  # some start bins are larger than Linf which will cause errors for calculating length
  # fixed this by forcing start to also be smaller than Linf
  # SurvCheck <- SurvRep%>%
  #   dplyr::filter(startbin>end)
  # CatchCheck <- CatchRep%>%
  #   dplyr::filter(startbin>end)
  # 
  # Survey <- anti_join(SurvRep,SurvCheck) # leave out problem lengths
  # Catch <- anti_join(CatchRep,CatchCheck)   
  EstNsize <- as.data.frame(hydra_sim_rep$EstNsize)
  year <- rep(1:hydra_data$Nyrs,hydra_data$Nspecies)
  species <- rep(1:hydra_data$Nspecies,each=hydra_data$Nyrs)
  EstNsize <- cbind(year,species,EstNsize)
  
  # Extract only the fishing mortality rate for fleet/species combinations specified
  Fyr <- list()
  colnames(hydra_sim_rep$Fyr)[1:2] <- c("species","fleet")
  for(i in 1:length(fleetdistribute))
  {
    Fyr[[i]] <- dplyr::filter(as.data.frame(hydra_sim_rep$Fyr),species %in% as.vector(fleetdistribute[[i]]),fleet==i)
  }
  
  Fyr.df <- data.frame(matrix(ncol=ncol(hydra_sim_rep$Fyr),nrow=0))
  colnames(Fyr.df) <- colnames(as.data.frame(hydra_sim_rep$Fyr))
  for(i in 1:length(fleetdistribute))
  {
    Fyr.df <- rbind(Fyr.df,Fyr[[i]])
  }

  # Here, combine the catches across the three fleets into a single catch vector
  
  hydra_sim_data <- list(predSurSize=SurvRep,
                         predCatchSize=CatchRep,
                         predBiomass=survey.df, #[,-c(4,7,8)],
                         predCatch=catch.df, #[,-c(5,8,9)],
                         abundance=abundance,
                         biomass=biomass,
                         foragebiomass=foragebiomass,
                         recruitment=hydra_sim_rep$EstRec,
                         Nspecies=hydra_data$Nspecies,
                         ln_recsigma=hydra_data$ln_recsigma,
                         Fyr=Fyr.df)
  return(hydra_sim_data)
} 
