#PLOTS FITS FROM OM MODEL (SARAHS DATASETS GEORGES BANK 4 SPECIES 5 LENBINS)

source("R/read.report.R")
source("R/gettables.R")
library(tidyverse)
library(hydradata)
library(compResidual)

#READ SARAHS DATA SETS
hydraDataList <- readRDS("inputs/hydra_sim_GBself_5bin.rds")

#READ FITS FROM SARAHS DATA SETS
#repfile <- "inputs/initial_run/test/hydra_sim.rep"
repfile <- "inputs/initial_run/hydra_sim.rep"

output<-reptoRlist(repfile)

#### READ CATCH AND SURVEY OBSERVED BIOMASS ####
obs_surveyB <- hydraDataList$observedBiomass
obs_catchB <- hydraDataList$observedCatch

biorows <- dim(obs_surveyB)[1]
catchrows <- dim(obs_catchB)[1]

#create a table with estimated and observed values
indexfits <- gettables(repfile, biorows, catchrows)

survey_obspred <- indexfits[1][[1]] %>%
  mutate(obs = biomass + 1e-07,
         pred = pred_bio + 1e-07,
         log_obs = log(obs),
         log_pred = log(pred),
         log_lo = log_obs - 1.96*cv,
         log_hi = log_obs + 1.96*cv,
         obs_lo = exp(log_lo),
         obs_hi = exp(log_hi),
         species = hydraDataList$speciesList[species])

for (surv in unique(survey_obspred$survey)) {
  cat("##### Survey #", surv, " \n")
  p1 <- survey_obspred %>% 
    filter(survey == surv) %>% 
    ggplot() +
    aes(x= year, y = log_obs, group = species, col=factor(survey)) +
    geom_errorbar(aes(ymin = log_lo, ymax = log_hi)) +
    geom_point() +
    geom_line(aes(x=year, y=log_pred), col = "blue") +
    facet_wrap(~species, scales = "free_y") +
    theme_bw() +
    guides(species = "None")
  print(p1)
  cat(" \n\n")
}

catch_obspred <- indexfits[2][[1]] %>% 
  mutate(obs = catch + 1e-07,
         pred = pred_catch + 1e-07,
         log_obs = log(obs),
         log_pred = log(pred),
         log_lo = log_obs - 1.96*cv,
         log_hi = log_obs + 1.96*cv,
         obs_lo = exp(log_lo),
         obs_hi = exp(log_hi),
         species = hydraDataList$speciesList[species])

for (fleet in unique(catch_obspred$fishery)) {
  cat("##### Fleet #", fleet, " \n")
  p3 <- catch_obspred %>% 
    filter(fishery == fleet,
           catch > 0) %>% 
    ggplot() +
    aes(x= year, y = log_obs, group = species, col= factor(fishery)) +
    geom_errorbar(aes(ymin = log_lo, ymax = log_hi)) +
    geom_point() +
    geom_line(aes(x=year, y=log_pred), col = "blue") +
    facet_wrap(~species, scales = "free_y") +
    theme_bw() +
    guides(species = "None")
  print(p3)
  cat(" \n\n")
}

obs_survey <- hydraDataList$observedSurvSize %>% tibble()
obs_survey <- obs_survey %>% pivot_longer(cols=6:ncol(.), names_to = "lenbin") %>% #filter(value != -999)%>%
  
  mutate(lenbin = as.integer(str_remove(lenbin, "sizebin")),
         label = rep("survey",nrow(.)),
         species = hydraDataList$speciesList[species])
obs_survey$value[which(obs_survey$value == -999)] = 0.00001

pred_surv<-output$pred_survey_size
obs_survey$pred_surv<-pred_surv
nll_survey<-output$nll_survey_size
obs_survey$nll_surv<-nll_survey
obs_survey$pearson<-((obs_survey$value-obs_survey$pred_surv)/sqrt(obs_survey$pred_surv))

colnames(obs_survey) <- c('number','year','species','type','InpN','lenbin','obs_value','label',
                          'pred_value','nll','pearson')

obs_catch <- hydraDataList$observedCatchSize %>% tibble()
obs_catch<-obs_catch %>% pivot_longer(cols=7:ncol(.), names_to = "lenbin") %>%
  mutate(lenbin = as.integer(str_remove(lenbin, "sizebin")),
         label = rep("catch",nrow(.)),
         species = hydraDataList$speciesList[species])# %>% filter(value != -999)
obs_catch$value[which(obs_catch$value == -999)] = 0.00001

pred_catch<-output$pred_catch_size
obs_catch$pred_catch<-pred_catch
nll_catch<-output$nll_catch_size
obs_catch$nll_catch<-nll_catch
obs_catch$pearson<-((obs_catch$value-obs_catch$pred_catch)/sqrt(obs_catch$pred_catch))

colnames(obs_catch) <- c('number','area','year','species','type','InpN','lenbin','obs_value','label',
                         'pred_value','nll','pearson')

diet_catch <- bind_rows(obs_catch, obs_survey)

obs_diet <- hydraDataList$observedSurvDiet %>% tibble()
obs_diet<-obs_diet %>% pivot_longer(cols=6:ncol(.), names_to = "prey") %>%
  mutate(#lenbin = as.integer(str_remove(lenbin, "V")),
    species = hydraDataList$speciesList[species],
    label = rep("diet",nrow(.))) %>%
  I()


pred_diet<-output$pred_dietprop

if (length(pred_diet)!=nrow(obs_diet)) obs_diet <- obs_diet %>% filter(value != 0)

obs_diet$pred_diet<-pred_diet
nll_diet<-output$nll_dietprop
obs_diet$nll_diet<-nll_diet
obs_diet$pearson<-((obs_diet$value-obs_diet$pred_diet)/sqrt(obs_diet$pred_diet))
colnames(obs_diet) <- c('number','year','species','lenbin','InpN','prey','obs_value','label', 'pred_value','nll','pearson')

mydata <- diet_catch %>%
  mutate(#prey = replace_na(prey, -99),
    area = replace_na(area, 1),
    type = replace_na(type, 0))
#data <- mydata
mydata$residual<-ifelse(mydata$pearson<0,"negative","positive")
mydata$res_abs<-abs(mydata$pearson)
mydata<-as.data.frame(mydata)

temp.catch = mydata[which(mydata$label == "catch"),]
temp.surv = mydata[which(mydata$label == "survey"),]
#temp.diet = data[which(data$label == "diet"),]

mydata <- obs_diet #data <- read.csv("outputs/survdiet.csv", header = T)
mydata$residual<-ifelse(mydata$pearson<0,"negative","positive")
mydata$res_abs<-abs(mydata$pearson)
temp.diet = mydata[which(mydata$label == "diet"),]

long_surv <- temp.surv %>%
  pivot_longer(cols = c("pred_value","obs_value"),
               names_to = c("kind","junk"),names_sep = "_") %>%
  select(-junk)

sp<-1
plot_surv <- list()
especies<-unique(long_surv$species)
for (sp in especies) {
  
  temp_size<-long_surv %>% filter(species == sp & number==1) %>%
    group_by(year) %>%
    summarize(mu_ss=mean(InpN))
  
  plot_surv[[sp]] <- long_surv %>% filter (species==sp& number==1) %>%
    ggplot() +
    aes(x=lenbin, y = value) +
    geom_line(aes(col = kind)) +
    geom_point(data = long_surv %>% filter (species==sp, number==1, kind == "obs"),
               aes(x=lenbin, y=value)) +
    facet_wrap(~year, dir="v") +
    geom_text(data=temp_size, aes(x = 4.5, y = 0.5, label = mu_ss), size=3) +
    theme(legend.position = "bottom") +
    labs(col="") +
    guides(col = guide_legend(nrow = 1))
  
  #ggsave(paste0(plotdir,"complot_surv_",sp,".jpeg"), plot_surv, width = 10, height = 7, dpi = 300)#, width=3000, height=2500, res=250)
  
}

##### Size composition of survey 1 by species

for (sp in especies){
  tmp <- plot_surv[[sp]]
  cat("######", sp, " \n")
  print(tmp)
  cat(" \n\n")
}

#### plot 1 length composition plots by species (survey)

sp<-1
plot_surv <- list()
especies<-unique(long_surv$species)
for (sp in especies) {
  
  temp_size<-long_surv %>% filter(species == sp & number==2) %>%
    group_by(year) %>%
    summarize(mu_ss=mean(InpN))
  
  if(dim(temp_size)[1] > 0) {
    
    plot_surv[[sp]] <- long_surv %>% filter (species==sp & number==2) %>%
      ggplot() +
      aes(x=lenbin, y = value) +
      geom_line(aes(col = kind)) +
      geom_point(data = long_surv %>% filter (species==sp, number==2, kind == "obs"),
                 aes(x=lenbin, y=value)) +
      facet_wrap(~year, dir="v", drop = FALSE) +
      geom_text(data=temp_size, aes(x = 4.5, y = 0.5, label = mu_ss), size=3) +
      theme(legend.position = "bottom") +
      labs(col="") +
      guides(col = guide_legend(nrow = 1))
    
  }
  
  if(dim(temp_size)[1] == 0) plot_surv[[sp]] <- NULL
  
  #ggsave(paste0(plotdir,"complot_surv_",sp,".jpeg"), plot_surv, width = 10, height = 7, dpi = 300)#, width=3000, height=2500, res=250)
  
}

for (sp in especies){
  tmp <- plot_surv[[sp]]
  cat("######", sp, " \n")
  print(tmp)
  cat(" \n\n")
}

##### Survey size composition pearson residuals (survey 1)

temp.surv.all <- temp.surv

temp.surv <- temp.surv %>% dplyr::filter(number ==1)

ggplot(temp.surv, aes(x=year, y=lenbin, size = res_abs, color=factor(residual))) +
  geom_point(alpha=0.7) + theme(title = element_text(angle = 0, hjust = 0.5, size=15, colour="black")) +
  facet_wrap(.~species) + labs(x="year", y="length bin", title="Pearson residuals")
#dev.off()

temp.surv <- temp.surv.all

##### Survey size composition pearson residuals (survey 2)

temp.surv.all <- temp.surv

temp.surv <- temp.surv %>% dplyr::filter(number ==2)

ggplot(temp.surv, aes(x=year, y=lenbin, size = res_abs, color=factor(residual))) +
  geom_point(alpha=0.7) + theme(title = element_text(angle = 0, hjust = 0.5, size=15, colour="black")) +
  facet_wrap(.~species) + labs(x="year", y="length bin", title="Pearson residuals")
#dev.off()

temp.surv <- temp.surv.all

##### Survey OSA residuals

plot_osa <- list()
especies<-unique(temp.surv$species)
for (sp in especies) {
  
  survdat <- temp.surv %>%
    filter(number == 1, species == sp)
  obs <- survdat %>% 
    select(year, lenbin, obs_value) %>% 
    mutate(obs_value = obs_value + 0.00001) %>% 
    pivot_wider(names_from = "year",
                values_from = "obs_value") %>% 
    column_to_rownames(var = "lenbin") %>% 
    as.matrix()
  pred <- survdat %>% 
    select(year, lenbin, pred_value) %>% 
    pivot_wider(names_from = "year",
                values_from = "pred_value") %>% 
    column_to_rownames(var = "lenbin") %>% 
    as.matrix()
  res<-resMulti(obs,pred)
  plot_osa[[sp]] <- res
}

for (sp in especies) {
  tmp <- plot_osa[[sp]]
  cat("######", sp, " \n")
  plot(tmp)
  cat(" \n\n")
}

plot_osa <- list()
especies<-unique(long_surv$species)
for (sp in especies) {
  
  survdat <- temp.surv %>%
    filter(number == 2, species == sp)
  obs <- survdat %>% 
    select(year, lenbin, obs_value) %>% 
    mutate(obs_value = obs_value + 0.00001) %>% 
    pivot_wider(names_from = "year",
                values_from = "obs_value") %>% 
    column_to_rownames(var = "lenbin") %>% 
    as.matrix()
  pred <- survdat %>% 
    select(year, lenbin, pred_value) %>% 
    pivot_wider(names_from = "year",
                values_from = "pred_value") %>% 
    column_to_rownames(var = "lenbin") %>% 
    as.matrix()
  if(dim(pred)[1] == 0){plot_osa[[sp]] <- NULL}
  else{
    res<-resMulti(obs,pred)
    plot_osa[[sp]] <- res
  }
  
  
}

##### Survey 2 OSA residuals
for (sp in especies) {
  tmp <- plot_osa[[sp]]
  cat("######", sp, " \n")
  ifelse(!is.null(tmp), plot(tmp), print(tmp))
  cat(" \n\n")
}

##################################################

##### Size composition of catch by species {.tabset}

long_catch <- temp.catch %>%
  pivot_longer(cols = c("pred_value","obs_value"),
               names_to = c("kind","junk"),names_sep = "_") %>%
  select(-junk)
#sp<-1
#plotdir <- "outputs/figures/comp_plots/by_species/"

especies<-unique(long_catch$species)
plot_catch <- NULL
for (sp in especies) {
  
  temp_size<-long_catch%>% filter(species == sp) %>%
    group_by(year) %>%
    summarize(mu_ss=mean(InpN))
  
  plot_catch[[sp]] <- long_catch %>% filter(species==sp) %>%
    ggplot() +
    aes(x=lenbin, y = value) +
    geom_line(aes(col = kind)) +
    geom_point(data = long_catch %>% filter (species==sp, kind == "obs"),
               aes(x=lenbin, y=value)) +
    facet_wrap(~year, dir="v") +
    geom_text(data=temp_size, aes(x = 4.8, y = 0.5, label = mu_ss), size=3) +
    theme(legend.position = "bottom") +
    labs(col="") +
    guides(col = guide_legend(nrow = 1))
  
  ##ggsave(paste0(plotdir,"complot_catch_",sp,".jpeg"), plot_catch, width = 10, height = 7, dpi = 300)#, width=3000, height=2500, res=250)
  
}

for (sp in especies){
  tmp <- plot_catch[[sp]]
  cat("######", sp, " \n")
  print(tmp)
  cat(" \n\n")
}

##### Catch size composition pearson residuals
ggplot(temp.catch, aes(x=year, y=lenbin, size = res_abs, color=factor(residual))) +
  geom_point(alpha=0.7) + theme(title = element_text(angle = 0, hjust = 0.5, size=15, colour="black")) +
  facet_wrap(.~species) + labs(x="year", y="length bin", title="Pearson residuals")


##### Fishery Size Comp OSA residuals
  
plot_osa <- list()
  tempcat1 <- temp.catch %>% 
    filter(number == 1)
  especies<-unique(tempcat1$species)
  for (sp in especies) {
    
    survdat <- tempcat1 %>%
      filter(species == sp)
    obs <- survdat %>% 
      select(year, lenbin, obs_value) %>% 
      mutate(obs_value = obs_value + 0.00001) %>% 
      pivot_wider(names_from = "year",
                  values_from = "obs_value") %>% 
      column_to_rownames(var = "lenbin") %>% 
      as.matrix()
    pred <- survdat %>% 
      select(year, lenbin, pred_value) %>% 
      pivot_wider(names_from = "year",
                  values_from = "pred_value") %>% 
      column_to_rownames(var = "lenbin") %>% 
      as.matrix()
    res<-resMulti(obs,pred)
    plot_osa[[sp]] <- res
  }

  ##### Fleet 1 Size composition OSA residuals {.tabset}

    for (sp in especies) {
    tmp <- plot_osa[[sp]]
    cat("######", sp, " \n")
    plot(tmp)
    cat(" \n\n")
    }
  
##########
  
  plot_osa <- list()
  tempcat1 <- temp.catch %>% 
    filter(number == 2)
  especies<-unique(tempcat1$species)
  for (sp in especies) {
    survdat <- tempcat1 %>%
      filter(species == sp)
    obs <- survdat %>% 
      select(year, lenbin, obs_value) %>% 
      mutate(obs_value = obs_value + 0.00001) %>% 
      pivot_wider(names_from = "year",
                  values_from = "obs_value") %>% 
      column_to_rownames(var = "lenbin") %>% 
      as.matrix()
    pred <- survdat %>% 
      select(year, lenbin, pred_value) %>% 
      pivot_wider(names_from = "year",
                  values_from = "pred_value") %>% 
      column_to_rownames(var = "lenbin") %>% 
      as.matrix()
    res<-resMulti(obs,pred)
    plot_osa[[sp]] <- res
  }

  
  ##### Fleet 2 Size composition OSA residuals {.tabset}
  
  for (sp in especies) {
    tmp <- plot_osa[[sp]]
    cat("######", sp, " \n")
    plot(tmp)
    cat(" \n\n")
  }
  
  
  