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


#foragebiomass_all <- tibble(
#  ID = seq_along(dataList),
#  scenario = dataList
#) %>%
#  mutate(
#    foragebiomass = map(scenario, "foragebiomass")
#  ) %>%
#  select(ID, foragebiomass) %>%
#  unnest_longer(foragebiomass, indices_to = "isim") %>%
#  mutate(
#    foragebiomass = map(foragebiomass, as_tibble)
#  ) %>%
#  unnest(foragebiomass) %>%
#  rename(
#    year = Year,
#    foragebiomass = Biomass
#  ) %>%
#  select(ID, isim, year, foragebiomass)


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

write.csv(Ffleet_all, "Ffleet_all.csv", row.names = FALSE)

#construir base de datos

ML_data <- biomass_all %>%
  select(ID, isim, species, year,
         biomass, size1, size2, size3, size4, size5)

ML_data <- ML_data %>%
  left_join(
    foragebiomass_all %>%
      rename(
        foragebiomass = foragebiomass
      ),
    by = c("ID", "isim", "year")
  )

ML_data <- ML_data %>%
  left_join(catch_all, by = c("ID", "isim", "species", "year"))

ML_data <- ML_data %>%
  left_join(Fspecies_all, by = c("ID", "isim", "species", "year"))

ML_data <- ML_data %>%
  select(-fleet.x, -fleet.y)

#saveRDS(ML_data, "ML_data.rds")
#write.csv(ML_data, "ML_data.csv", row.names = FALSE)

### Building features

features <- ML_data %>%
  arrange(ID, isim, species, year) %>%
  group_by(ID, isim, species) %>%
  mutate(
    biomass_lag1 = lag(biomass, 1),
    biomass_lag2 = lag(biomass, 2),
   # biomass_roll3 = slide_dbl(biomass, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
   # biomass_roll5 = slide_dbl(biomass, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
   # foragebiomass_lag1 = lag(foragebiomass, 1),
   # foragebiomass_lag2 = lag(foragebiomass, 2),
   # foragebiomass_roll3 = slide_dbl(foragebiomass, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
   #  foragebiomass_roll5 = slide_dbl(foragebiomass, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
   # predcatch_lag1 = lag(predcatch, 1),
   # predcatch_lag2 = lag(predcatch, 2),
   # predcatch_roll3 = slide_dbl(predcatch, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
   # predcatch_roll5 = slide_dbl(predcatch, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
   # obscatch_lag1 = lag(obscatch, 1),
   # obscatch_lag2 = lag(obscatch, 2),
   # obscatch_roll3 = slide_dbl(obscatch, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
   # obscatch_roll5 = slide_dbl(obscatch, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
   # F_lag1 = lag(F, 1),
   # F_lag2 = lag(F, 2),
   # F_roll3 = slide_dbl(F, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
   # F_roll5 = slide_dbl(F, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
   # prop_size1 = if_else(biomass > 0, size1 / biomass, NA_real_),
   # prop_size2 = if_else(biomass > 0, size2 / biomass, NA_real_),
   # prop_size3 = if_else(biomass > 0, size3 / biomass, NA_real_),
   # prop_size4 = if_else(biomass > 0, size4 / biomass, NA_real_),
   # prop_size5 = if_else(biomass > 0, size5 / biomass, NA_real_)
  ) %>%
  ungroup()

features <- features %>%
  select(-size1, -size2, -size3, -size4, -size5, -area, -obscatch, -foragebiomass)

biomass_wide <- features %>%
  select(ID, isim, year, species, biomass) %>%
  pivot_wider(
    names_from = species,
    values_from = biomass,
    names_prefix = "biomass_sp"
  )

features_full <- features %>%
  left_join(biomass_wide, by = c("ID", "isim", "year"))

saveRDS(features_full, "features.rds")
#write.csv(features, "features.csv", row.names = FALSE)

################################
###### ML ######################
################################

features <- readRDS("features.rds")

#correlation matrix
library(ggcorrplot)

features %>%
  filter(species == 1) %>%
  select(where(is.numeric), -ID, -isim, -species) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot(type = "lower")

#---------------------------
# 1. Split runs into train/test
#---------------------------
set.seed(123)

features <- features %>%
  mutate(isim_id = paste(ID, isim, sep = "_"))

runs <- unique(features$isim_id)
train_runs <- sample(runs, size = floor(0.75 * length(runs)))

train_all <- features %>% filter(isim_id %in% train_runs)
test_all  <- features %>% filter(!isim_id %in% train_runs)

# select one species and get the training and testing data for that species

fit_rf_species <- function(sp_name, train_df, test_df) {
  
  train_data <- train_df %>%
    filter(species == sp_name) %>%
    arrange(isim_id, year)
  
  test_data <- test_df %>%
    filter(species == sp_name) %>%
    arrange(isim_id, year)
  
  # remove same species biomass 
  col_to_remove <- paste0("biomass_sp", sp_name)
  
  train_data <- train_data %>% select(-all_of(col_to_remove))
  test_data  <- test_data %>% select(-all_of(col_to_remove))
  
  if (nrow(train_data) == 0 || nrow(test_data) == 0) {
    return(NULL)
  }
  
  # Cross-validation grouped by run
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 3) # try with 3 
  
  # Recipe
  rf_recipe <- recipe(biomass ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  # Model
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 100 # try with 100, 200 etc 
  ) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  
  # Workflow
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  # Grid
  n_preds <- train_data %>%
    select(-biomass,-ID, -isim, -isim_id,  -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(10, n_preds))), # try with 10 
    min_n(range = c(2, 10)), # try with 10
    levels = 3 # try with 3 and 5  
  )
  
  # Tune
  rf_tuned <- tune_grid(
    rf_workflow,
    resamples = cv_splits,
    grid = rf_grid,
    metrics = metric_set(rmse, mae)
  )
  
  best_params <- select_best(rf_tuned, metric = "rmse")
  
  final_rf <- finalize_workflow(rf_workflow, best_params)
  
  rf_fit <- fit(final_rf, data = train_data)
  
  rf_preds <- predict(rf_fit, test_data) %>%
    bind_cols(test_data)
  
  model_metrics <- rf_preds %>%
    metrics(truth = biomass, estimate = .pred)
  
  list(
    species = sp_name,
    fit = rf_fit,
    preds = rf_preds,
    metrics = model_metrics,
    tuning = rf_tuned,
    best_params = best_params
  )
}


species_list <- 1
#species_list <- sort(unique(features$species))

library(doParallel)

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

start_time <- Sys.time()
rf_results <- map(species_list, fit_rf_species,
                  train_df = train_all,
                  test_df = test_all)

end_time <- Sys.time()
print(end_time - start_time)

names(rf_results) <- species_list

# Remove NULLs if any species had no train/test rows
rf_results <- compact(rf_results)


all_metrics <- map_dfr(rf_results, ~ .x$metrics %>% mutate(species = .x$species))

all_metrics

stopCluster(cl)


sp_example <- species_list[1]

rf_results[[sp_example]]$preds %>%
  ggplot(aes(x = biomass, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = paste("Observed vs Predicted", sp_example),
    x = "Observed biomass",
    y = "Predicted biomass"
  ) +
  theme_minimal()
