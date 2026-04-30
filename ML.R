### LIBRARIES ----
library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(slider)
library(tidymodels)
library(ggplot2)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(rsample)
library(yardstick)
library(dials)
library(vip)
library(doParallel)
library(tuneR)


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

ML_data <- readRDS("ML_data.rds")

ML_data <- ML_data %>%
  arrange(ID, isim, species, year)

species_list <- 1:10
#species_list <- 1

#****************
### MODEL 0 ----
#****************
#sp_name<-1
model_name <- "m0"

all_results_m0 <- list()
all_metrics_m0 <- list()

model_start <- Sys.time()

for (sp_name in species_list) {
  
  cat("Running Model 0, species:", sp_name, "\n")
  
  target_col <- paste0("biomass_sp", sp_name)
  
  features_m0 <- ML_data %>%
    arrange(ID, isim, year) %>%
    distinct(ID, isim, year, .keep_all = TRUE) %>%
    group_by(ID, isim) %>%
    mutate(
      # response = biomass at time t
      biomass_target = .data[[target_col]],
      
      # lagged predictors (t-1)
      across(
        starts_with("biomass_sp"),
        ~ lag(.x, 1),
        .names = "{.col}_lag1"
      ),
      
      # fishing lag
      F_lag1 = lag(F, 1)
    ) %>%
    ungroup() %>%
    mutate(
      species = sp_name,
      isim_id = paste(ID, isim, sep = "_")
    ) %>%
    select(
      ID, isim, isim_id, species, year,
      biomass_target,
      F_lag1,
      matches("^biomass_sp\\d+_lag1$")
    ) %>%
    drop_na()
  
  set.seed(123)
  
  runs <- unique(features_m0$isim_id)
  train_runs <- sample(runs, size = floor(0.75 * length(runs)))
  
  train_data <- features_m0 %>%
    filter(isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  test_data <- features_m0 %>%
    filter(!isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
  
  rf_recipe <- recipe(biomass_target ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_zv(all_predictors())
  
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  n_preds <- train_data %>%
    select(-biomass_target, -ID, -isim, -isim_id, -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(20, n_preds))),
    min_n(range = c(2, 20)),
    levels = 5
  )
  
  start_time <- Sys.time()
  
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
  
  end_time <- Sys.time()
  runtime_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("Species:", sp_name,
      "Model:", model_name,
      "Runtime (min):", round(runtime_min, 2), "\n")
  
  model_metrics <- rf_preds %>%
    metrics(truth = biomass_target, estimate = .pred) %>%
    mutate(
      species = sp_name,
      model = model_name,
      runtime_min = runtime_min
    )
  
  all_results_m0[[paste0("sp", sp_name)]] <- list(
    species = sp_name,
    model = model_name,
    fit = rf_fit,
    preds = rf_preds,
    metrics = model_metrics
  )
  
  all_metrics_m0[[paste0("sp", sp_name)]] <- model_metrics
}

model_end <- Sys.time()

metrics_m0 <- bind_rows(all_metrics_m0)

#metrics_m0

#p_vip_m0_sp1 <- vip::vip(all_results_m0[["sp1"]]$fit$fit$fit)

for (sp_name in species_list) {
  
  result_name <- paste0("sp", sp_name)
  
  # Variable importance plot
  p_vip <- vip::vip(all_results_m0[[result_name]]$fit$fit$fit)
  
  ggsave(
    paste0("plots/RF/M0/var_m0_sp", sp_name, ".png"),
    plot = p_vip,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Observed vs predicted plot
  p_model <- all_results_m0[[result_name]]$preds %>%
    ggplot(aes(x = biomass_target, y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = paste0("Observed vs Predicted Mod 0", sp_name),
      x = "Observed biomass(t)",
      y = "Predicted biomass(t)"
    ) +
    theme_minimal()
  
  ggsave(
    paste0("plots/RF/M0/model_m0_sp", sp_name, ".png"),
    plot = p_model,
    width = 8,
    height = 6,
    dpi = 300
  )
}

#****************
### MODEL 1 ----
#****************
  
all_results_m1 <- list()
all_metrics_m1 <- list()

for (sp_name in species_list) {
  model_name <- "t1"
  model_start <- Sys.time()
  
  cat("Running Model 1, species:", sp_name, "\n")
  
  target_col <- paste0("biomass_sp", sp_name)
  
  features_full <- ML_data %>%
    arrange(ID, isim, year) %>%
    distinct(ID, isim, year, .keep_all = TRUE) %>%
    group_by(ID, isim) %>%
    mutate(
      biomass_target = lead(.data[[target_col]], 1),
      F_t = F
    ) %>%
    ungroup() %>%
    mutate(
      species = sp_name,
      isim_id = paste(ID, isim, sep = "_")
    ) %>%
    select(
      ID, isim, isim_id, species, year,
      biomass_target,
      F_t,
      starts_with("biomass_sp")
    ) %>%
    drop_na()
  
  set.seed(123)
  
  runs <- unique(features_full$isim_id)
  train_runs <- sample(runs, size = floor(0.75 * length(runs)))
  
  train_data <- features_full %>%
    filter(isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  test_data <- features_full %>%
    filter(!isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
  
  rf_recipe <- recipe(biomass_target ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_zv(all_predictors())
  
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  n_preds <- train_data %>%
    select(-biomass_target, -ID, -isim, -isim_id, -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(20, n_preds))),
    min_n(range = c(2, 20)),
    levels = 5
  )
  
  start_time <- Sys.time()
  
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
  
  end_time <- Sys.time()
  
  runtime_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("Species:", sp_name,
      "Model:", "t1",
      "Runtime (min):", round(runtime_min, 2), "\n")
  
  model_metrics <- rf_preds %>%
    metrics(truth = biomass_target, estimate = .pred) %>%
    mutate(
      species = sp_name,
      model = "t1",
      runtime_min = runtime_min
    )
  
  all_results_m1[[paste0("sp", sp_name)]] <- list(
    species = sp_name,
    model = "t1",
    fit = rf_fit,
    preds = rf_preds,
    metrics = model_metrics,
    tuning = rf_tuned,
    best_params = best_params
  )
  
  all_metrics_m1[[paste0("sp", sp_name)]] <- model_metrics
}

model_end <- Sys.time()

metrics_m1 <- bind_rows(all_metrics_m1)
#metrics_m1
#p_vip_m1_sp1 <- vip::vip(all_results_m1[["sp1"]]$fit$fit$fit)

for (sp_name in species_list) {
  
  result_name <- paste0("sp", sp_name)
  
  # Variable importance plot
  p_vip <- vip::vip(all_results_m0[[result_name]]$fit$fit$fit)
  
  ggsave(
    paste0("plots/RF/M1/var_m1_sp", sp_name, ".png"),
    plot = p_vip,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Observed vs predicted plot
  p_model <- all_results_m0[[result_name]]$preds %>%
    ggplot(aes(x = biomass_target, y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = paste0("Observed vs Predicted Mod 1", sp_name),
      x = "Observed biomass(t)",
      y = "Predicted biomass(t)"
    ) +
    theme_minimal()
  
  ggsave(
    paste0("plots/RF/M1/model_m1_sp", sp_name, ".png"),
    plot = p_model,
    width = 8,
    height = 6,
    dpi = 300
  )
}

#saveRDS(all_results_m1, "model1_t1_all_species.rds")
#write.csv(metrics_m1, "model1_t1_all_species.csv", row.names = FALSE)

#****************
### MODEL 2 ----
#****************
  
all_results_m2 <- list()
all_metrics_m2 <- list()

for (sp_name in species_list) {
  model_name <- "t2"
  model_start <- Sys.time()
  
  cat("Running Model 2, species:", sp_name, "\n")
  
  target_col <- paste0("biomass_sp", sp_name)
  
  features_full <- ML_data %>%
    arrange(ID, isim, year) %>%
    distinct(ID, isim, year, .keep_all = TRUE) %>%
    group_by(ID, isim) %>%
    mutate(
      biomass_target = lead(.data[[target_col]], 2),
      F_t  = F,
      F_t1 = lead(F, 1),
      F_t2 = lead(F, 2)
    ) %>%
    ungroup() %>%
    mutate(
      species = sp_name,
      isim_id = paste(ID, isim, sep = "_")
    ) %>%
    select(
      ID, isim, isim_id, species, year,
      biomass_target,
      F_t, F_t1, F_t2,
      starts_with("biomass_sp")
    ) %>%
    drop_na()
  
  set.seed(123)
  
  runs <- unique(features_full$isim_id)
  train_runs <- sample(runs, size = floor(0.75 * length(runs)))
  
  train_data <- features_full %>%
    filter(isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  test_data <- features_full %>%
    filter(!isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
  
  rf_recipe <- recipe(biomass_target ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_zv(all_predictors())
  
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  n_preds <- train_data %>%
    select(-biomass_target, -ID, -isim, -isim_id, -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(20, n_preds))),
    min_n(range = c(2, 20)),
    levels = 5
  )
  
  start_time <- Sys.time()
  
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
  
  end_time <- Sys.time()
  
  runtime_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("Species:", sp_name,
      "Model:", "t2",
      "Runtime (min):", round(runtime_min, 2), "\n")
  
  model_metrics <- rf_preds %>%
    metrics(truth = biomass_target, estimate = .pred) %>%
    mutate(
      species = sp_name,
      model = "t2",
      runtime_min = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    )
  
  all_results_m2[[paste0("sp", sp_name)]] <- list(
    species = sp_name,
    model = "t2",
    fit = rf_fit,
    preds = rf_preds,
    metrics = model_metrics,
    tuning = rf_tuned,
    best_params = best_params
  )
  
  all_metrics_m2[[paste0("sp", sp_name)]] <- model_metrics
}

model_start <- Sys.time()

metrics_m2 <- bind_rows(all_metrics_m2)
#metrics_m2
#p_vip_m2_sp1 <- vip::vip(all_results_m2[["sp1"]]$fit$fit$fit)

for (sp_name in species_list) {
  
  result_name <- paste0("sp", sp_name)
  
  # Variable importance plot
  p_vip <- vip::vip(all_results_m0[[result_name]]$fit$fit$fit)
  
  ggsave(
    paste0("plots/RF/M2/var_m2_sp", sp_name, ".png"),
    plot = p_vip,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Observed vs predicted plot
  p_model <- all_results_m0[[result_name]]$preds %>%
    ggplot(aes(x = biomass_target, y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = paste0("Observed vs Predicted Mod2", sp_name),
      x = "Observed biomass(t)",
      y = "Predicted biomass(t)"
    ) +
    theme_minimal()
  
  ggsave(
    paste0("plots/RF/M2/model_m2_sp", sp_name, ".png"),
    plot = p_model,
    width = 8,
    height = 6,
    dpi = 300
  )
}

#saveRDS(all_results_m2, "model2_t2_all_species.rds")
#write.csv(metrics_m2, "model2_t2_all_species.csv", row.names = FALSE)

#****************
### MODEL 3 ----
#****************
  
all_results_m3 <- list()
all_metrics_m3 <- list()

for (sp_name in species_list) {
  model_name <- "t3"
  model_start <- Sys.time()
  
  cat("Running Model 3, species:", sp_name, "\n")
  
  target_col <- paste0("biomass_sp", sp_name)
  
  features_full <- ML_data %>%
    arrange(ID, isim, year) %>%
    distinct(ID, isim, year, .keep_all = TRUE) %>%
    group_by(ID, isim) %>%
    mutate(
      biomass_target = lead(.data[[target_col]], 3),
      F_t  = F,
      F_t1 = lead(F, 1),
      F_t2 = lead(F, 2),
      F_t3 = lead(F, 3)
    ) %>%
    ungroup() %>%
    mutate(
      species = sp_name,
      isim_id = paste(ID, isim, sep = "_")
    ) %>%
    select(
      ID, isim, isim_id, species, year,
      biomass_target,
      F_t, F_t1, F_t2, F_t3,
      starts_with("biomass_sp")
    ) %>%
    drop_na()
  
  set.seed(123)
  
  runs <- unique(features_full$isim_id)
  train_runs <- sample(runs, size = floor(0.75 * length(runs)))
  
  train_data <- features_full %>%
    filter(isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  test_data <- features_full %>%
    filter(!isim_id %in% train_runs) %>%
    arrange(isim_id, year)
  
  cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
  
  rf_recipe <- recipe(biomass_target ~ ., data = train_data) %>%
    update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
    step_zv(all_predictors())
  
  rf_model <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 200
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")
  
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model)
  
  n_preds <- train_data %>%
    select(-biomass_target, -ID, -isim, -isim_id, -year, -species) %>%
    ncol()
  
  rf_grid <- grid_regular(
    mtry(range = c(1, min(20, n_preds))),
    min_n(range = c(2, 20)),
    levels = 5
  )
  
  start_time <- Sys.time()
  
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
  
  end_time <- Sys.time()
  
  runtime_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("Species:", sp_name,
      "Model:", "t3",
      "Runtime (min):", round(runtime_min, 2), "\n")
  
  model_metrics <- rf_preds %>%
    metrics(truth = biomass_target, estimate = .pred) %>%
    mutate(
      species = sp_name,
      model = "t3",
      runtime_min = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    )
  
  all_results_m3[[paste0("sp", sp_name)]] <- list(
    species = sp_name,
    model = "t3",
    fit = rf_fit,
    preds = rf_preds,
    metrics = model_metrics,
    tuning = rf_tuned,
    best_params = best_params
  )
  
  all_metrics_m3[[paste0("sp", sp_name)]] <- model_metrics
}

model_start <- Sys.time()
model_end <- Sys.time()

metrics_m3 <- bind_rows(all_metrics_m3)
#metrics_m3
#p_vip_m3_sp1 <- vip::vip(all_results_m3[["sp1"]]$fit$fit$fit)

for (sp_name in species_list) {
  
  result_name <- paste0("sp", sp_name)
  
  # Variable importance plot
  p_vip <- vip::vip(all_results_m0[[result_name]]$fit$fit$fit)
  
  ggsave(
    paste0("plots/RF/M3/var_m3_sp", sp_name, ".png"),
    plot = p_vip,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Observed vs predicted plot
  p_model <- all_results_m0[[result_name]]$preds %>%
    ggplot(aes(x = biomass_target, y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = paste0("Observed vs Predicted Mod 3", sp_name),
      x = "Observed biomass(t)",
      y = "Predicted biomass(t)"
    ) +
    theme_minimal()
  
  ggsave(
    paste0("plots/RF/M3/model_m3_sp", sp_name, ".png"),
    plot = p_model,
    width = 8,
    height = 6,
    dpi = 300
  )
}

#saveRDS(all_results_m3, "model3_t3_all_species.rds")
#write.csv(metrics_m3, "model3_t3_all_species.csv", row.names = FALSE)

#****************
### ALL METRICS ----
#****************

metrics_all_models <- bind_rows(metrics_m1, metrics_m2, metrics_m3)
#write.csv(metrics_all_models, "all_models_all_species.csv", row.names = FALSE)

metrics_all_models

ggplot(all_metrics_df, aes(x = factor(species), y = runtime_min)) +
  geom_col() +
  facet_wrap(~ model) +
  labs(x = "Species", y = "Runtime (minutes)")
