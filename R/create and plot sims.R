
source("R/read.report.R")
source("R/gettables.R")
library(tidyverse)
#library(magrittr)
#library(FSA)
library(hydradata)


### Read observed and estimated values, Hydra data list from Sarahs 4 species scenario
hydraDataList <- readRDS("inputs/hydra_sim_GBself_5bin.rds")
#rep file model (check latest version) 
repfile <- "inputs/initial_run/hydra_sim.rep"
output<-reptoRlist(repfile)

#repfile<-read.table("inputs/initial_run/pmse_predvals.out", header = FALSE, skip=2, nrow=336)
#colnames(repfile)<-c("survey", "year", "spp", "area", "pred_survey")

#### READ CATCH AND SURVEY OBSERVED BIOMASS ####
obs_surveyB <- hydraDataList$observedBiomass
obs_catchB <- hydraDataList$observedCatch

biorows <- dim(obs_surveyB)[1]
catchrows <- dim(obs_catchB)[1]

#create a table with estimated and observed values
indexfits <- gettables(repfile, biorows, catchrows)
# select survey biomass
biomass<-indexfits[[1]] %>%
  mutate(species = hydraDataList$speciesList[species])
# select catch
catch<-indexfits[[2]]%>%
  mutate(species = hydraDataList$speciesList[species])


##### SIMULATE CATCH DATA ######
# create simulation object
sim_data <- NULL
isim <- 1

for (isim in 1:100) { # 5 just to try 
  
  # replace index with simulated data
  
  
  hydraDataList$observedCatch <- obs_catchB %>%
    mutate(catch = rnorm(nrow(.), indexfits[[2]]$pred_catch,indexfits[[2]]$cv))
  
  sim_data[[isim]] <- hydraDataList
  
  
  ##### SIMULATE SURVEY BIOMASS DATA ######
  hydraDataList$observedBiomass <- obs_surveyB %>%
    mutate(biomass = (rnorm(nrow(.), indexfits[[1]]$pred_bio, indexfits[[1]]$cv)))
  # store simulated object
  sim_data[[isim]] <- hydraDataList
  
  
  #### SIMULATE CATCH SIZE COMPOSITION DATA ####
  
  obscatch_size <- hydraDataList$observedCatchSize %>% tibble()
  obscatch_size<-obscatch_size %>% pivot_longer(cols=7:ncol(.), names_to = "lenbin") %>%
    mutate(lenbin = as.integer(str_remove(lenbin, "sizebin")),
           label = rep("catch",nrow(.)))#,
  #species = hydraDataList$speciesList[species])# %>% filter(value != -999)
  #obscatch_size<- obscatch_size %>% filter(value != -999)
  obscatch_size$value[which(obscatch_size$value == -999)] = 0.000001
  obscatch_size <- select(obscatch_size, -label)
  
  pred_catchsize<-output$pred_catch_size
  nll_catch<-output$nll_catch_size
  
  #hydraDataList$observedCatchSize <- obscatch_size %>% 
  #  mutate(value = (rmultinom(1, obscatch_size$inpN, pred_catchsize)))
  # store simulated object
  #sim_data[[isim]] <- hydraDataList
  
  temporal1 = numeric()
  especie = numeric(); especie = sort(unique(obscatch_size$species)) # especies
  for(e in 1:length(especie)){
    pos1 = numeric(); pos1 = which(obscatch_size$species == especie[e])
    year = numeric(); year = sort(unique(obscatch_size$year[pos1]))
    for(y in 1:length(year)){
      pos2 = numeric(); pos2 = which(obscatch_size$year[pos1]== year[y])
      
      temp = numeric(); temp = rmultinom(1, unique(obscatch_size$inpN[pos1][pos2]), pred_catchsize[pos1][pos2])
      temporal1 = c(temporal1, temp)
    }
  }
  
  obscatch_size$value = temporal1
  hydraDataList$observedCatchSize<-obscatch_size
  
  hydraDataList$observedCatchSize["value"]<-hydraDataList$observedCatchSize["value"]/hydraDataList$observedCatchSize["inpN"]
  
  hydraDataList$observedCatchSize<-hydraDataList$observedCatchSize %>% pivot_wider(names_from = "lenbin") %>%
    rename(sizebin1 = `1`, sizebin2 = `2`, sizebin3 = `3`, sizebin4 = `4`, sizebin5 = `5`)
  
  
  #### SIMULATE SURVEY SIZE COMPOSITION DATA ####
  
  obssurv_size <- hydraDataList$observedSurvSize %>% tibble()
  obssurv_size <- obssurv_size %>% pivot_longer(cols=6:ncol(.), names_to = "lenbin") %>% #filter(value != -999)%>%
    
    mutate(lenbin = as.integer(str_remove(lenbin, "sizebin")),
           label = rep("survey",nrow(.)))#,
  #species = hydraDataList$speciesList[species])
  #surv_size<- surv_size %>% filter(value != -999)
  obssurv_size$value[which(obssurv_size$value == -999)] = 0.000001
  obssurv_size <- select(obssurv_size, -label)
  
  pred_survsize<-output$pred_survey_size
  nll_survey<-output$nll_survey_size
  
  #hydraDataList$observedSurvSize <- obssurv_size %>% 
  #  mutate(value = (rmultinom(1, obssurv_size$inpN, pred_survsize)))
  # store simulated object
  #sim_data[[isim]] <- hydraDataList
  
  temporal1 = numeric()
  number = numeric(); number = sort(unique(obssurv_size$survey))
  for (n in 1:length(number)) {
    pos0 = numeric(); pos0 = which(obssurv_size$survey == number[n])
    
    especie = numeric(); especie = sort(unique(obssurv_size$species[pos0])) # especies
    for(e in 1:length(especie)){
      pos1 = numeric(); pos1 = which(obssurv_size$species[pos0] == especie[e])
      
      year = numeric(); year = sort(unique(obssurv_size$year[pos0][pos1]))
      for(y in 1:length(year)){
        pos2 = numeric(); pos2 = which(obssurv_size$year[pos0][pos1]== year[y])
        
        temp = numeric(); temp = rmultinom(1, unique(obssurv_size$inpN[pos0][pos1][pos2]), pred_survsize[pos0][pos1][pos2])
        temporal1 = c(temporal1, temp)
      }
    }
  }
  
  obssurv_size$value = temporal1
  hydraDataList$observedSurvSize<-obssurv_size
  
  hydraDataList$observedSurvSize["value"]<-hydraDataList$observedSurvSize["value"]/hydraDataList$observedSurvSize["inpN"]
  
  hydraDataList$observedSurvSize<-hydraDataList$observedSurvSize %>% pivot_wider(names_from = "lenbin") %>%
    rename(sizebin1 = `1`, sizebin2 = `2`, sizebin3 = `3`, sizebin4 = `4`, sizebin5 = `5`)
  
  #to check if I am getting the correct values
  #write.csv(surv_size, file = "surv_size.csv", row.names = T)
  
  
  #### SIMULATE DIET COMPOSITION DATA ####
  
  obsdiet_comp <- hydraDataList$observedSurvDiet %>% tibble()
  obsdiet_comp<-obsdiet_comp %>% pivot_longer(cols=6:ncol(.), names_to = "prey") %>%
    mutate(#lenbin = as.integer(str_remove(lenbin, "V")),
      #species = hydraDataList$speciesList[species],
      label = rep("diet",nrow(.)))
  obsdiet_comp$value[which(obsdiet_comp$value == -999)] = 0.000001
  obsdiet_comp <- select(obsdiet_comp, -label)
  
  pred_diet<-output$pred_dietprop
  if (length(pred_diet)!=nrow(obsdiet_comp)) obsdiet_comp <- obsdiet_comp %>% filter(value != 0)
  nll_diet<-output$nll_dietprop
  
  hydraDataList$observedSurvDiet
  
  temporal1 = numeric()
  number = numeric(); number = sort(unique(obsdiet_comp$survey))
  for (n in 1:length(number)) {
    pos0 = numeric(); pos0 = which(obsdiet_comp$survey == number[n])
    
    species = numeric(); species = sort(unique(obsdiet_comp$species[pos0])) # especies
    for(e in 1:length(species)){
      pos1 = numeric(); pos1 = which(obsdiet_comp$species[pos0] == species[e])
      
      year = numeric(); year = sort(unique(obsdiet_comp$year[pos0][pos1]))
      for(y in 1:length(year)){
        pos2 = numeric(); pos2 = which(obsdiet_comp$year[pos0][pos1]== year[y])
        
        lenbin = numeric(); lenbin = sort(unique(obsdiet_comp$sizebin[pos0][pos1][pos2]))
        for(l in 1:length(lenbin)){
          pos3 = numeric(); pos3 = which(obsdiet_comp$sizebin[pos0][pos1][pos2] == lenbin[l])
          
          temp = numeric(); temp = rmultinom(1, unique(obsdiet_comp$inpN[pos0][pos1][pos2][pos3]), obsdiet_comp$value[pos0][pos1][pos2][pos3])
          temporal1 = c(temporal1, temp)
        }
      }
    }
  }
  
  # replace data with simulated data
  
  obsdiet_comp$value = temporal1
  hydraDataList$observedSurvDiet<-obsdiet_comp
  hydraDataList$observedSurvDiet["value"]<-hydraDataList$observedSurvDiet["value"]/hydraDataList$observedSurvDiet["inpN"]
  
  hydraDataList$observedSurvDiet<-hydraDataList$observedSurvDiet %>% pivot_wider(names_from = "prey")
  
  #to check if I am getting the correct values
  #write.csv(diet_comp, file = "diet_comp.csv", row.names = T)
  
  # use write_tsDatFile function to save the new ts file with simulated data 
  #n sim equal to scenario identifier from 1 to 30 simulations 
  
}

write_rds(sim_data, "sim_data.rds")

#### DIAGNOSTICS ####

source("R/read.report.R")
source("R/gettables.R")
library(ggforce)
library(tidyverse)

hydraDataList <- readRDS("inputs/hydra_sim_GBself_5bin.rds")
#sim_data


hydraDataList2 <- readRDS("sim_data.rds")

sim_obs_catch<-purrr::map_dfr(hydraDataList2,"observedCatch",.id = "isim")
sim_obs_catch<- sim_obs_catch %>%
  mutate(species = hydraDataList$speciesList[species])

fleet1plot<-sim_obs_catch %>% filter(fishery==1)%>%
  ggplot() +
  aes(x = year, y = log(catch), col = isim) +
  geom_line() +
  facet_wrap(~species, scales = "free",dir="v") +
  #geom_line() +
  # geom_point(aes(x=year, y=log(obs_value)), col = "red")+
  #  geom_errorbar(aes(ymin = (log(obs_value)-1.96*cv), ymax = (log(obs_value)+1.96*cv)), col="gray")+
  theme_minimal() +
  labs(x = "Year",
       y = "Catch (t)",
       title = "Time series of estimated LN(catch)")

print(fleet1plot)

fleet2plot<-sim_obs_catch %>% filter(fishery==2)%>%
  ggplot() +
  aes(x = year, y = (catch), col = isim) +
  geom_line() +
  facet_wrap(~species, scales = "free",dir="v") +
  # geom_point(aes(x=year, y=log(obs_value)), col = "red")+
  #  geom_errorbar(aes(ymin = (log(obs_value)-1.96*cv), ymax = (log(obs_value)+1.96*cv)), col="gray")+
  theme_minimal() +
  labs(x = "Year",
       y = "Catch (t)",
       title = "Time series of estimated LN(catch)")

print(fleet2plot)


sim_obs_bio<-purrr::map_dfr(hydraDataList2,"observedBiomass",.id = "isim")
sim_obs_bio<- sim_obs_bio %>%
  mutate(species = hydraDataList$speciesList[species])

surv1plot<-sim_obs_bio %>% filter(survey==1)%>%
  ggplot() +
  aes(x = year, y = log(biomass), col = isim) +
  geom_line() +
  facet_wrap(~species, scales = "free") +
  #geom_point(aes(x=year, y=log(obs_value)), col = "red")+
  #geom_errorbar(aes(ymin = (log(obs_value)-1.96*cv), ymax = (log(obs_value)+1.96*cv)), col="gray")+
  theme_minimal() +
  labs(x = "Year",
       y = "Biomass (t)",
       title = "Time series of estimated LN(biomass)")

print(surv1plot)


surv2plot<-sim_obs_bio %>% filter(survey==2)%>%
  ggplot() +
  aes(x = year, y = log(biomass), col = isim) +
  geom_line() +
  facet_wrap(~species, scales = "free") +
  # geom_point(aes(x=year, y=log(obs_value)), col = "red")+
  #geom_errorbar(aes(ymin = (log(obs_value)-1.96*cv), ymax = (log(obs_value)+1.96*cv)), col="gray")+
  theme_minimal() +
  labs(x = "Year",
       y = "Biomass (t)",
       title = "Time series of estimated LN(biomass)")

print(surv2plot)










