### libraries ----
library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(slider)
library(tidymodels)
library(ggplot2)

#species_names <- c(
#  "Atlantic_cod",
#  "Atlantic_herring",
#  "Atlantic_mackerel",
#  "Goosefish",
#  "Haddock",
#  "Silver_hake",
#  "Spiny_dogfish",
#  "Winter_flounder",
#  "Winter_skate",
#  "Yellowtail_flounder"
#)

################################
###### ML ######################
################################

ML_data <- readRDS("ML_data.rds")
ML_data <- ML_data %>%
  arrange(ID, isim, species, year)
# to test select one species 

#####  Creates lagged variables for all the 10 species 
###   not using this yet 
##########################################

year_features <- ML_data %>%
  arrange(ID, isim, year) %>%
  distinct(ID, isim, year, .keep_all = TRUE) %>%
  group_by(ID, isim) %>%
  mutate(
    across(
      starts_with("biomass_sp"),
      ~ dplyr::lag(.x, 1),
      .names = "{.col}_lag1"
    )
  ) %>%
  ungroup() %>%
  select(ID, isim, year, matches("^biomass_sp\\d+_lag1$"))

features <- ML_data %>%
  left_join(year_features, by = c("ID", "isim", "year")) %>%
  mutate(isim_id = paste(ID, isim, sep = "_"))

features <- features %>%
  select(-matches("^biomass_sp\\d+$"))

# predcatch_lag1 = lag(predcatch, 1),
    # predcatch_lag2 = lag(predcatch, 2),
    # predcatch_roll3 = slide_dbl(predcatch, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    # predcatch_roll5 = slide_dbl(predcatch, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
    # F_lag1 = lag(F, 1),
    # F_lag2 = lag(F, 2),
    # F_roll3 = slide_dbl(F, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    # F_roll5 = slide_dbl(F, mean, .before = 4, .complete = TRUE, na.rm = TRUE),


######### remove features I dont want to use 
features <- features %>%
 select(-predcatch, -fleet.y)

#correlation matrix
library(ggcorrplot)

features %>%
  filter(species == 1) %>%
  select(where(is.numeric), -ID, -isim, -species) %>%
  cor(use = "complete.obs") %>%
  ggcorrplot(type = "lower")

biomass_wide_lags <- biomass_wide %>%
  group_by(ID, isim) %>%
  mutate(
    across(
      starts_with("biomass_sp"),
      list(
        lag1 = ~lag(.x, 1),
        lag2 = ~lag(.x, 2)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()


###################
#### START ML #####
###################

set.seed(123)

#split sims train and test by isimID
runs <- unique(features$isim_id)
train_runs <- sample(runs, size = floor(0.75 * length(runs)))

train_all <- features %>% filter(isim_id %in% train_runs)
test_all  <- features %>% filter(!isim_id %in% train_runs)

#dim(train_all)
#[1] 189000      7
#> dim(test_all)
#[1] 63000     7

### start with one species to test 
species_list <- 1
#species_list <- sort(unique(features$species))


fit_rf_species <- function(sp_name, train_df, test_df) {
  
  train_data <- train_df %>%
    filter(species == sp_name) %>%
    arrange(isim_id, year) %>%
    drop_na()
  
  test_data <- test_df %>%
    filter(species == sp_name) %>%
    arrange(isim_id, year) %>%
    drop_na()
  
  dup_current <- paste0("biomass_sp", sp_name)
  
  train_data <- train_data %>%
    select(-any_of(dup_current))
  
  test_data <- test_data %>%
    select(-any_of(dup_current))
  
  if (nrow(train_data) == 0 || nrow(test_data) == 0) {
    return(NULL)
  }
  
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
  
  rf_recipe <- recipe(biomass ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_zv(all_predictors())
  
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200
  ) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  n_preds <- train_data %>%
    select(-biomass, -ID, -isim, -isim_id, -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(20, n_preds))),
    min_n(range = c(2, 20)),
    levels = 5
  )
  
  rf_tuned <- tune_grid(
    rf_workflow,
    resamples = cv_splits,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq, mae)
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

library(doParallel)
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

start_time <- Sys.time()

rf_results <- map(
  species_list,
  fit_rf_species,
  train_df = train_all,
  test_df = test_all
)

end_time <- Sys.time()
print(end_time - start_time)

stopCluster(cl)

rf_results <- compact(rf_results)

all_metrics <- map_dfr(rf_results, ~ .x$metrics %>% mutate(species = .x$species))

all_metrics

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

