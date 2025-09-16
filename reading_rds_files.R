### libraries ----
library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(slider)

# Definir la ruta donde están los archivos .rds
dataListRoute<- "OM_MUST/data"

# not working
#gdrive_shared <- "https://drive.google.com/drive/folders/12oWuIuguyKFsy1ozgzqyqSGY7MKoWz8T"
#usethis::create_download_url(gdrive_shared) %>%
#  url() %>%
#  readRDS() %>%
#  tibble::as_tibble()

# Crear una lista con los nombres de los archivos .rds
files <- list.files(path = dataListRoute, pattern = "\\.rds$", full.names = TRUE)

# read rds files
# now reading just 001 baseline scenario but could read the 36 scenarios
filestoread <- files[1]#[1:36] 

# make a list with the scenarios
dataList <- lapply(filestoread, readRDS)

# save rds file in one object 
#write_rds(dataList, "rds_files.rds")
#write_rds(dataList, "rds001_file.rds")

#read saved object with 36 scenarios
#rdsDataList <- readRDS("rds_files.rds")


### TO READ JUST BASELINE SCENARIO 001 START HERE ---- 

baseline<-readRDS("OM_MUST/data/001.rds")

# read variables I need 
#rdsDataList: a top-level list of length 1
#Inside, rdsDataList[[1]] is a list of 11 components
#Each of the main components you’ll use (biomass, catch, Fyrspecies, Fyrfleets, etc.) is itself a list of 50 — i.e., 50 runs/trajectories.
#Each run element (e.g., rdsDataList[[1]]$biomass[[i]]) is a data frame with columns like Species, Year, Biomass, size1..size5.

#Each component (e.g. biomass) is a list of 50 elements → these are the 50 stochastic runs/scenarios of the pMSE.
#Each run element is usually a data frame:
#Some are large (e.g. 720 rows × 8 columns → species × years × bins).
#Others are smaller (e.g. 72 rows × 2 columns → maybe per-year totals, fleet-specific values, or advice tables).
#The differences in shape depend on what the component represents (species-level by size, fleet-level by year, etc.).

###CREATING VARIABLES ---- 

## bind the 50 runs and tag run_id 

bind_runs <- function(x) {
  map2_dfr(x, seq_along(x), \(df, i) {
    df <- as.data.frame(df)
    df$run_id <- i
    df
  })
}

## 1) BIOMASS (species-specific) 
stopifnot("biomass" %in% names(baseline))
biomass_all <- bind_runs(baseline$biomass)  # Species, Year, Biomass, size1..size5, run_id

# Long biomass (Species–Year–value)
Bio <- biomass_all %>%
  rename(Biomass = any_of(c("Biomass","biomass"))) %>%
  select(run_id, Species, Year, Biomass) %>%
  arrange(run_id, Species, Year)

## 2) RECRUITMENT (R) assuming recruitment = to size1 
Rec <- biomass_all %>%
  mutate(Recruitment = if ("size1" %in% names(.)) size1 else NA_real_) %>%
  select(run_id, Species, Year, Recruitment) %>%
  arrange(run_id, Species, Year)

## 3) CATCH (species-specific) 
stopifnot("catch" %in% names(baseline))
catch_all <- bind_runs(baseline$catch)

Catch <- catch_all %>%
  transmute(
    run_id,
    Species = species,
    Year    = year,
    PredCatch = predcatch,
    ObsCatch  = obscatch
  ) %>%
  arrange(run_id, Species, Year)

## 4) BIOMASS-AT-LENGTH (all bins) 
# Long: one row per species-year-sizebin
Bio_size <- biomass_all %>%
  pivot_longer(cols = matches("^size\\d+$"),
               names_to = "sizebin",
               values_to = "bio_at_len") %>%
  arrange(run_id, Species, Year, sizebin)

##  5) Fishing mortality 

# (a) By species
#stopifnot("Fyrspecies" %in% names(baseline))
#Fsp_all <- bind_runs(baseline$Fyrspecies) %>%
#  rename_with(tolower)       # ensure: species, fleet, v3..vn, run_id

#F_species <- Fsp_all %>%
#  pivot_longer(
#    cols = matches("^v\\d+$"),          # all columns named V<number>
#    names_to = "vcol",
#    values_to = "F"
#  ) %>%
#  mutate(
#    Year = as.integer(str_remove(vcol, "^v"))  # "v3" -> 3
#  ) %>%
#  select(run_id, species, fleet, Year, F) %>%
#  arrange(run_id, species, fleet, Year)

#F_species <- F_species %>%
#  rename(Species = species, Fleet = fleet)


# (b) different way with years 
stopifnot("Fyrspecies" %in% names(baseline))

Fsp_all <- bind_runs(baseline$Fyrspecies) %>%
  rename_with(tolower)  # species, fleet, v3..vn, run_id

F_species <- Fsp_all %>%
  tidyr::pivot_longer(
    cols = matches("^v\\d+$"),
    names_to = "vcol",
    values_to = "F"
  ) %>%
  dplyr::mutate(
    vnum = as.integer(sub("^v", "", vcol)),
    Year = vnum - 2L          # => V3=1, V4=2, V5=3, ...
  ) %>%
  dplyr::arrange(run_id, species, fleet, vnum) %>%
  dplyr::select(run_id, Species = species, Fleet = fleet, Year, F)


# (b) By fleets
stopifnot("Fyrfleets" %in% names(baseline))
Ffleets_all <- bind_runs(baseline$Fyrfleets)

#NOT SURE HOW TO DO THIS DIFFERENT YEAR LENGTH 

##  Quick  checks 
 dplyr::glimpse(Bio)
 dplyr::glimpse(Rec)
 dplyr::glimpse(Catch)
 dplyr::glimpse(Bio_size)
 dplyr::glimpse(F_species)
 dplyr::glimpse(F_fleets)

###CREATING ECOSYSTEM INDICATORS ---- 
 # interannual variability (IAV) 
 # interannual variability (IAV) for a time series.
 # first value is NA (no previous year to compare),
 # absolute year-to-year change divided by the average of the two years (a symmetric % change).
 
 iav <- function(x) {
   if (length(x) < 2) return(rep(NA_real_, length(x)))
   c(NA_real_, abs(diff(x)) / ((x[-1] + x[-length(x)]) / 2))
 }
 
 # 1) Total biomass and SD across species (per run_id, Year)\
 # biomass of species𝑠in that year - mean biomass across species / n number of species- 1) 

 B_tot <- Bio %>%
   group_by(run_id, Year) %>%
   summarize(
     B_total = sum(Biomass, na.rm = TRUE),
     B_sd    = sd(Biomass, na.rm = TRUE),
     .groups = "drop"
   )
 
 # 2) Total yield (pred & obs), then interannual variability on totals
 C_tot <- Catch %>%
   group_by(run_id, Year) %>%
   summarize(
     Y_total_pred = if ("PredCatch" %in% names(.)) sum(PredCatch, na.rm = TRUE) else NA_real_,
  #   Y_total_obs  = if ("ObsCatch"  %in% names(.)) sum(ObsCatch,  na.rm = TRUE) else NA_real_,
     .groups = "drop"
   ) %>%
   arrange(run_id, Year) %>%
   group_by(run_id) %>%
   mutate(
     C_IAV_pred = if ("Y_total_pred" %in% names(.)) iav(Y_total_pred) else NA_real_,
  #   C_IAV_obs  = if ("Y_total_obs"  %in% names(.)) iav(Y_total_obs)  else NA_real_
   ) %>%
   ungroup()
 
 # 3) Collapse count: species below threshold
 # Threshold = 20% of the run-specific max biomass for that species
 thresh <- Bio %>%
   group_by(run_id, Species) %>%
   summarize(B_thresh = 0.2 * max(Biomass, na.rm = TRUE), .groups = "drop")
 
 Collapse <- Bio %>%
   left_join(thresh, by = c("run_id","Species")) %>%
   mutate(collapsed = as.integer(Biomass < B_thresh)) %>%
   group_by(run_id, Year) %>%
   summarize(Collapse_count = sum(collapsed, na.rm = TRUE), .groups = "drop")
 
 # 4) Assemble ecosystem indicators table
 Eco_ind <- B_tot %>%
   left_join(C_tot,     by = c("run_id","Year")) %>%
   left_join(Collapse, by = c("run_id","Year")) %>%
   arrange(run_id, Year)
 
#  dplyr::glimpse(Eco_ind)
#  head(Eco_ind)  
 
  

#### CREATING LAGGED VARIABLES AND ROLLING MEAN  ---- 
  
# rolling mean helper (right-aligned)
roll_mean_k <- function(x, k) slide_dbl(x, mean, .before = k - 1, .complete = TRUE, na.rm = TRUE)
  
# generic lags+rolls adder for a long panel (grouped by keys, ordered by Year)
add_lags_rolls <- function(df, group_keys, cols, lags = c(1, 2), rolls = c(3, 5)) {
    df <- df %>%
      group_by(across(all_of(group_keys))) %>%
      arrange(Year, .by_group = TRUE)
    for (col in cols) {
      # lags
      for (L in lags) {
        df <- df %>% mutate(!!paste0(col, "_lag", L) := dplyr::lag(.data[[col]], L))
      }
      # rolling means
      for (k in rolls) {
        df <- df %>% mutate(!!paste0(col, "_roll", k) := roll_mean_k(.data[[col]], k))
      }
    }
    ungroup(df)
  }
  
  Var_ind <- Bio %>%
    full_join(Rec,         by = c("run_id","Species","Year")) %>%
    full_join(Catch,         by = c("run_id","Species","Year")) %>%
    full_join(F_species, by = c("run_id","Species","Year"))
  
  species_cols <- intersect(c("Biomass","Recruitment","PredCatch","ObsCatch","F"),
                            names(Var_ind))
  
  species_features <- add_lags_rolls(
    df         = Var_ind,
    group_keys = c("run_id","Species"),
    cols       = species_cols,
    lags       = c(1, 2),
    rolls      = c(3, 5)
  )
  
#FLEET-LEVEL (F by fleet) 
  # Expect: F_fleets_long(run_id, Fleet, Year, F)
 
  
# ECOSYSTEM-LEVEL 
  # Expect: eco_ind(run_id, Year, B_total, Y_total_pred, Y_total_obs, ...)
  if (exists("Eco_ind")) {
    eco_cols <- intersect(c("B_total","Y_total_pred","Y_total_obs"), names(Eco_ind))
    eco_features <- add_lags_rolls(
      df         = Eco_ind,
      group_keys = "run_id",
      cols       = eco_cols,
      lags       = c(1, 2),
      rolls      = c(3, 5)
    )
  }
  
Eco_ind

write_csv(species_features, "species_features.csv")
