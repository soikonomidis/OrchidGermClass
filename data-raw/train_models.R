# ==============================================================================
# DATA PREPARATION & MODEL TRAINING SCRIPT
# This script is meant to be run ONCE by the developer.
# It trains the KNN and SVM models and saves them internally to the package.
# ==============================================================================

# 1. Load required libraries
library(tidyverse)
library(caret)
library(e1071)
library(kernlab)
library(readxl)
library(usethis)

# Define the local classification function used for training
categorize_germination <- function(x) {
  cut(x,
      breaks = c(-Inf, 30, 50, 80, Inf),
      labels = c("Low", "Mid", "High", "Max"),
      right = TRUE)
}

# 2. Read and clean the dataset
# Since this script runs from the package root, we point to data-raw/
data_file <- "data-raw/analysis_dataset_references_ES.xlsx"

if (!file.exists(data_file)) {
  stop("Excel file not found. Ensure it is placed inside the 'data-raw' folder.")
}


train_data <- readxl::read_xlsx(data_file) %>%
  rename(
    FG_perc = `FG%`,
    ES_ratio = `E:S`,
    Growth_habit = `Growth habit`,
    Climate_zone = `Climate zone`
  ) %>%
  mutate(
    across(c(Subfamily, Growth_habit, Habitat, Climate_zone), as.factor),
    Germ_Class = categorize_germination(as.numeric(FG_perc))
  ) %>%
  drop_na(FG_perc, ES_ratio, Subfamily, Growth_habit, Habitat, Climate_zone)

# 3. Define the Model Formula
formula_step5 <- FG_perc ~ ES_ratio + Subfamily + Growth_habit + Habitat + Climate_zone

# Training controls (5-fold Cross Validation)
ctrl <- trainControl(method = "cv", number = 5)
set.seed(123)

# 4. Train the Models

model_knn <- train(formula_step5, data = train_data, method = "knn",
                   trControl = ctrl,
                   preProcess = c("nzv", "center", "scale"),
                   tuneGrid = expand.grid(k = c(1, 3, 5, 7, 9)))


model_svm <- train(formula_step5, data = train_data, method = "svmRadial",
                   trControl = ctrl,
                   preProcess = c("nzv", "center", "scale"),
                   tuneLength = 5)

# 5. Save the models internally into the package

usethis::use_data(model_knn, model_svm, internal = TRUE, overwrite = TRUE)


