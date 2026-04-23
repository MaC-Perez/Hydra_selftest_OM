# Chapter 3: Machine learning 

Use the ML_feautres.R code to:

1) Loads multiple .rds simulation files from the pMSE project and organizes them into a list of scenarios.
2) It extracts key outputs from each scenario (biomass, catch, and fishing mortality) and reshapes them into tidy tables.
3) These datasets are merged into a single dataset (ML_data) containing species, time, biomass, and fishing information.
4) It builds time-series features such as lagged biomass (previous years) for each species and simulation.
5) The dataset is expanded to include biomass of all species as predictors using a wide format.
6) This data frame is saved under "features.rds" name so you dont have to run the first portion of the code

    START HERE

Use the ML.R code to:
   
7) upload features matrix "features.rds"   
8) A correlation matrix is generated to explore relationships among predictors for a selected species.
9) The data is split into training and testing sets based on simulation runs (isim_id).
10) A Random Forest model is defined, tuned (via cross-validation), and fitted separately for each species.
11) Model performance is evaluated using metrics like RMSE and MAE on the test data.
12) Finally, predictions are visualized and the total computation time is recorded using parallel processing.
