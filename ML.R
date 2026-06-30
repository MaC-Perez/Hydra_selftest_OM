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


##########
# XGBOOST
##########

### XGBOOST — ALL 10 SPECIES, MODELS 2 & 3 (t+2, t+3) ----
# Adapted from ML.R's Random Forest workflow. Same feature construction,
# same run-level (isim_id) splitting logic, swapped to boost_tree() / xgboost
# engine, and generalized into one function so t+2 and t+3 share code instead
# of being duplicated blocks.

dir.create("plots/XGB", recursive = TRUE, showWarnings = FALSE)

# ---- function: runs one horizon (h years ahead) across all species ----
run_xgb_horizon <- function(h, model_label) {
  
  all_results <- list()
  all_metrics <- list()
  folder <- file.path("plots/XGB", toupper(model_label))
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  
  for (sp_name in species_list) {
    
    cat("Running XGBoost", model_label, "- species:", sp_name, "\n")
    
    target_col <- paste0("biomass_sp", sp_name)
    
    # base features: target h years ahead, F at t
    features_full <- ML_data %>%
      arrange(ID, isim, year) %>%
      distinct(ID, isim, year, .keep_all = TRUE) %>%
      group_by(ID, isim) %>%
      mutate(
        biomass_target = lead(.data[[target_col]], h),
        F_t = F
      )
    
    # add F_t1 ... F_t{h} (forward F trajectory through the horizon)
    for (k in 1:h) {
      features_full <- features_full %>%
        mutate(!!paste0("F_t", k) := lead(F, k))
    }
    
    features_full <- features_full %>%
      ungroup() %>%
      mutate(
        species = sp_name,
        isim_id = paste(ID, isim, sep = "_")
      ) %>%
      select(
        ID, isim, isim_id, species, year,
        biomass_target,
        F_t, matches(paste0("^F_t[1-", h, "]$")),
        starts_with("biomass_sp")
      ) %>%
      drop_na()
    
    set.seed(123)
    
    runs <- unique(features_full$isim_id)
    train_runs <- sample(runs, size = floor(0.80 * length(runs)))  # 80/20 split
    
    train_data <- features_full %>%
      filter(isim_id %in% train_runs) %>%
      arrange(isim_id, year)
    
    test_data <- features_full %>%
      filter(!isim_id %in% train_runs) %>%
      arrange(isim_id, year)
    
    cv_splits <- group_vfold_cv(train_data, group = isim_id, v = 5)
    
    xgb_recipe <- recipe(biomass_target ~ ., data = train_data) %>%
      update_role(ID, isim, isim_id, year, species, new_role = "ID") %>%
      step_zv(all_predictors())
    
    xgb_model <- boost_tree(
      trees          = 200,
      tree_depth     = tune(),
      learn_rate     = tune(),
      loss_reduction = tune(),
      sample_size    = tune(),
      mtry           = tune()
    ) %>%
      set_engine("xgboost", importance = "gain") %>%
      set_mode("regression")
    
    xgb_workflow <- workflow() %>%
      add_recipe(xgb_recipe) %>%
      add_model(xgb_model)
    
    n_preds <- train_data %>%
      select(-biomass_target, -ID, -isim, -isim_id, -year, -species) %>%
      ncol()
    
    xgb_grid <- grid_latin_hypercube(
      tree_depth(range = c(3, 8)),
      learn_rate(range = c(-3, -1)),     # log10 scale -> 0.001-0.1
      loss_reduction(),
      sample_prop(range = c(0.5, 1)),
      mtry(range = c(1, min(15, n_preds))),
      size = 10
    )
    
    start_time <- Sys.time()
    
    xgb_tuned <- tune_grid(
      xgb_workflow,
      resamples = cv_splits,
      grid = xgb_grid,
      metrics = metric_set(rmse, rsq, mae)
    )
    
    best_params <- select_best(xgb_tuned, metric = "rmse")
    final_xgb <- finalize_workflow(xgb_workflow, best_params)
    xgb_fit <- fit(final_xgb, data = train_data)
    
    xgb_preds <- predict(xgb_fit, test_data) %>%
      bind_cols(test_data)
    
    end_time <- Sys.time()
    runtime_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    cat("Species:", sp_name,
        "Model:", model_label,
        "Runtime (min):", round(runtime_min, 2), "\n")
    
    model_metrics <- xgb_preds %>%
      metrics(truth = biomass_target, estimate = .pred) %>%
      mutate(
        species = sp_name,
        model = model_label,
        runtime_min = runtime_min
      )
    
    all_results[[paste0("sp", sp_name)]] <- list(
      species = sp_name,
      model = model_label,
      fit = xgb_fit,
      preds = xgb_preds,
      metrics = model_metrics,
      tuning = xgb_tuned,
      best_params = best_params
    )
    
    all_metrics[[paste0("sp", sp_name)]] <- model_metrics
  }
  
  metrics_all <- bind_rows(all_metrics)
  
  # plots, per species
  for (sp_name in species_list) {
    result_name <- paste0("sp", sp_name)
    
    p_vip <- vip::vip(all_results[[result_name]]$fit$fit$fit)
    ggsave(
      file.path(folder, paste0("var_", model_label, "_sp", sp_name, ".png")),
      plot = p_vip, width = 8, height = 6, dpi = 300
    )
    
    p_model <- all_results[[result_name]]$preds %>%
      ggplot(aes(x = biomass_target, y = .pred)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(
        title = paste0("XGBoost Observed vs Predicted - ", model_label, " - species ", sp_name),
        x = paste0("Observed biomass (t+", h, ")"),
        y = paste0("Predicted biomass (t+", h, ")")
      ) +
      theme_minimal()
    
    ggsave(
      file.path(folder, paste0("model_", model_label, "_sp", sp_name, ".png")),
      plot = p_model, width = 8, height = 6, dpi = 300
    )
  }
  
  list(results = all_results, metrics = metrics_all)
}

# ---- run both horizons across all 10 species ----
xgb_t2 <- run_xgb_horizon(h = 2, model_label = "t2")
xgb_t3 <- run_xgb_horizon(h = 3, model_label = "t3")

metrics_xgb_all <- bind_rows(xgb_t2$metrics, xgb_t3$metrics)

write.csv(metrics_xgb_all, "xgb_t2_t3_all_species.csv", row.names = FALSE)
saveRDS(xgb_t2$results, "xgb_t2_all_species.rds")
saveRDS(xgb_t3$results, "xgb_t3_all_species.rds")

metrics_xgb_all

#########
# tables plots
#########

library(scales)

dir.create("plots/XGB/summary", recursive = TRUE, showWarnings = FALSE)

metrics_raw <- read_csv("xgb_t2_t3_all_species.csv")

# ---- species names ----
species_names <- tibble(
  species = 1:10,
  species_name = c(
    "Atlantic cod", "Atlantic herring", "Atlantic mackerel", "Goosefish",
    "Haddock", "Silver hake", "Spiny dogfish", "Winter flounder",
    "Winter skate", "Yellowtail flounder"
  )
)

ML_data <- readRDS("ML_data.rds")

mean_biomass <- ML_data %>%
  select(starts_with("biomass_sp")) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "species_col", values_to = "mean_biomass") %>%
  mutate(species = as.integer(str_remove(species_col, "biomass_sp"))) %>%
  select(species, mean_biomass)

# ---- build wide summary table ----
summary_table <- metrics_raw %>%
  pivot_wider(
    id_cols = c(species, model, runtime_min),
    names_from = .metric,
    values_from = .estimate
  ) %>%
  left_join(species_names, by = "species") %>%
  left_join(mean_biomass, by = "species") %>%
  mutate(
    horizon = recode(model, t2 = "t+2", t3 = "t+3"),
    rel_rmse = rmse / mean_biomass * 100,   # RMSE as % of mean biomass
    rel_mae  = mae  / mean_biomass * 100
  ) %>%
  arrange(species, model) %>%
  select(species, species_name, horizon, rmse, rsq, mae, rel_rmse, rel_mae, runtime_min)

write_csv(summary_table, "xgb_summary_table.csv")
print(summary_table, n = Inf)

# order species by mean biomass (largest to smallest) for consistent plotting
sp_order <- mean_biomass %>%
  left_join(species_names, by = "species") %>%
  arrange(desc(mean_biomass)) %>%
  pull(species_name)

summary_table <- summary_table %>%
  mutate(species_name = factor(species_name, levels = sp_order))

theme_set(theme_minimal(base_size = 13))
horizon_colors <- c("t+2" = "#4C72B0", "t+3" = "#DD8452")

# ---- Figure: RMSE by species and horizon ----
p_rmse <- ggplot(summary_table, aes(x = species_name, y = rmse, fill = horizon)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = horizon_colors) +
  labs(title = "XGBoost RMSE by species and forecast horizon",
       x = NULL, y = "RMSE (mt)", fill = "Horizon") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("plots/XGB/summary/fig_rmse_by_species.png", p_rmse, width = 9, height = 5.5, dpi = 300)

# ---- Figure: relative RMSE (% of mean biomass) by species and horizon ----
p_rel_rmse <- ggplot(summary_table, aes(x = species_name, y = rel_rmse, fill = horizon)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = horizon_colors) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "XGBoost relative RMSE by species and forecast horizon",
       x = NULL, y = "RMSE as % of mean biomass", fill = "Horizon") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("plots/XGB/summary/fig_relative_rmse_by_species.png", p_rel_rmse, width = 9, height = 5.5, dpi = 300)

# ---- Figure: R^2 by species and horizon ----
p_rsq <- ggplot(summary_table, aes(x = species_name, y = rsq, fill = horizon)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = horizon_colors) +
  coord_cartesian(ylim = c(0.7, 1.0)) +
  labs(title = expression("XGBoost R"^2*" by species and forecast horizon"),
       x = NULL, y = expression(R^2), fill = "Horizon") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("plots/XGB/summary/fig_rsq_by_species.png", p_rsq, width = 9, height = 5.5, dpi = 300)

# ---- Figure: runtime by species and horizon ----
p_runtime <- ggplot(summary_table, aes(x = species_name, y = runtime_min, fill = horizon)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = horizon_colors) +
  labs(title = "XGBoost runtime by species and forecast horizon",
       x = NULL, y = "Runtime (minutes)", fill = "Horizon") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("plots/XGB/summary/fig_runtime_by_species.png", p_runtime, width = 9, height = 5.5, dpi = 300)

# ---- Figure: t+2 -> t+3 RMSE slope chart, by species ----
p_slope <- ggplot(summary_table, aes(x = horizon, y = rmse, group = species_name)) +
  geom_line(color = "grey50", alpha = 0.8) +
  geom_point(color = "grey30", size = 2) +
  geom_text(
    data = summary_table %>% filter(horizon == "t+2"),
    aes(label = species_name), hjust = 1.05, size = 3.2
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.55, 0.15))) +
  labs(title = "RMSE degradation from t+2 to t+3, by species (XGBoost)",
       x = NULL, y = "RMSE (mt)")

ggsave("plots/XGB/summary/fig_slope_t2_t3.png", p_slope, width = 7, height = 6, dpi = 300)

cat("Summary table written to xgb_summary_table.csv\n")
cat("Figures written to plots/XGB/summary/\n")

############################
### trying for cod
############################


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
