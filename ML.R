### libraries ----
library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(slider)
library(tidymodels)
library(ggplot2)

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
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5) # try with 3 
  
  # Recipe
  rf_recipe <- recipe(biomass ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  # Model
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200 # try with 100, 200 etc 
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
    mtry(range = c(1, min(20, n_preds))), # try with 10 
    min_n(range = c(2, 20)), # try with 10
    levels = 5 # try with 3 and 5  
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
