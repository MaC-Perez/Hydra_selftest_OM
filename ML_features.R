### libraries ----
library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(slider)
library(tidymodels)
library(ggplot2)

# Definir la ruta donde están los archivos .rds
dataListRoute<- "OM_MUST/data"

# Crear una lista con los nombres de los archivos .rds
files <- list.files(path = dataListRoute, pattern = "\\.rds$", full.names = TRUE)
filestoread <- files #[12]#[1:36] 
# make a list with the scenarios
dataList <- lapply(filestoread, readRDS)
#write_rds(dataList, "rds_files.rds")

#read saved object with 36 scenarios
#rdsDataList <- readRDS("rds_files.rds")

biomass_all <- tibble(
  ID = seq_along(dataList),
  scenario = dataList
) %>%
  mutate(
    biomass = map(scenario, "biomass")
  ) %>%
  select(ID, biomass) %>%
  unnest_longer(biomass, indices_to = "isim") %>%
  mutate(
    biomass = map(biomass, as_tibble)
  ) %>%
  unnest(biomass) %>%
  rename(
    species = Species,
    year = Year,
    biomass = Biomass) %>%
  select(ID, isim, species, year, biomass, size1, size2, size3, size4, size5)


catch_all <- tibble(
  ID = seq_along(dataList),
  scenario = dataList
) %>%
  mutate(
    catch = map(scenario, "catch")
  ) %>%
  select(ID, catch) %>%
  unnest_longer(catch, indices_to = "isim") %>%
  mutate(
    catch = map(catch, as_tibble)
  ) %>%
  unnest(catch)


Ffleet_all <- tibble(
  ID = seq_along(dataList),
  scenario = dataList
) %>%
  mutate(
    Ffleet = map(scenario, "Fyrfleets")
  ) %>%
  select(ID, Ffleet) %>%
  unnest_longer(Ffleet, indices_to = "isim") %>%
  mutate(
    Ffleet = map(Ffleet, as_tibble)
  ) %>%
  unnest(Ffleet)


Fspecies_all <- tibble(
  ID = seq_along(dataList),
  scenario = dataList
) %>%
  mutate(
    Fspecies = map(scenario, "Fyrspecies")
  ) %>%
  select(ID, Fspecies) %>%
  unnest_longer(Fspecies, indices_to = "isim") %>%
  mutate(
    Fspecies = map(Fspecies, as_tibble)
  ) %>%
  unnest(Fspecies) %>%
  rename_with(
    ~ as.character(seq_along(.)),
    starts_with("V")
  )

Fspecies_all <- Fspecies_all %>%
  pivot_longer(
    cols = matches("^\\d+$"),
    names_to = "year",
    values_to = "F"
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  select(ID, isim, species, fleet, year, F)

#write.csv(Ffleet_all, "Ffleet_all.csv", row.names = FALSE)

#construir base de datos

ML_data <- biomass_all %>%
  select(ID, isim, species, year,
         biomass)

ML_data <- ML_data %>%
  left_join(catch_all, by = c("ID", "isim", "species", "year"))

ML_data <- ML_data %>%
  left_join(Fspecies_all, by = c("ID", "isim", "species", "year"))

biomass_wide <- ML_data %>%
  select(ID, isim, year, species, biomass) %>%
  pivot_wider(
    names_from = species,
    values_from = biomass,
    names_prefix = "biomass_sp"
  )

ML_data <- ML_data %>%
  left_join(biomass_wide, by = c("ID", "isim", "year")) %>%
  filter(year <= 42)

ML_data <- ML_data %>%
  select(-fleet.x, -area, -obscatch)

#saveRDS(ML_data, "ML_data.rds")
#write.csv(ML_data, "ML_data.csv", row.names = FALSE)

