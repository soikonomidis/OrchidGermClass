#' Predict Orchid Seed Germination Class
#'
#' This function uses pre-trained KNN and SVM models to predict the germination
#' potential of orchid species based on ecological and morphological traits.
#'
#' @param new_data A data frame containing the required columns:
#'   \code{ES_ratio}, \code{Subfamily}, \code{Growth_habit}, \code{Habitat},
#'   and \code{Climate_zone}.
#' @param model A string specifying which model to use: \code{"knn"} (default) or \code{"svm"}.
#'
#' @return A data frame with the original data and the appended prediction class.
#' @export
#' @importFrom stats predict na.pass
#'
#' @examples
#' \dontrun{
#' # Example usage with new data
#' my_orchids <- data.frame(
#'   ES_ratio = c(0.45, 0.12),
#'   Subfamily = c("O", "E"),
#'   Growth_habit = c("T", "E"),
#'   Habitat = c("O", "S"),
#'   Climate_zone = c("Te", "Tr")
#' )
#'
#' # Get predictions
#' predict_germination(my_orchids, model = "knn")
#' }
predict_germination <- function(new_data, model = c("knn", "svm")) {

  # 1. Validate the chosen model
  model <- match.arg(model)
  target_model <- if (model == "knn") model_knn else model_svm

  # Define the classification cutoffs used during original training
  categorize_germination <- function(x) {
    cut(x,
        breaks = c(-Inf, 30, 50, 80, Inf),
        labels = c("Low", "Mid", "High", "Max"),
        right = TRUE)
  }

  # 2. Strict Input and Factor Level Validation
  if (!"ES_ratio" %in% names(new_data)) {
    stop("Required predictor column 'ES_ratio' is missing from new_data.", call. = FALSE)
  }
  new_data$ES_ratio <- as.numeric(new_data$ES_ratio)

  # Align all factor variables with the exact training levels stored in the model
  expected_factors <- names(target_model$xlevels)

  for (col in expected_factors) {
    if (col %in% names(new_data)) {
      new_vals <- unique(as.character(new_data[[col]]))
      allowed_levels <- target_model$xlevels[[col]]
      unseen_vals <- setdiff(new_vals[!is.na(new_vals)], allowed_levels)

      # Warn the user AND show them the allowed categories
      if (length(unseen_vals) > 0) {
        warning(sprintf(
          "Column '%s' contains categories not seen during model training: %s.\n-> Allowed categories are: %s\nThese unknown values will be predicted as NA.",
          col,
          paste(sprintf("'%s'", unseen_vals), collapse = ", "),
          paste(sprintf("'%s'", allowed_levels), collapse = ", ")
        ), call. = FALSE)
      }

      # Reconstruct the factor matching the model's exact level ordering
      new_data[[col]] <- factor(new_data[[col]], levels = allowed_levels)
    } else {
      stop(sprintf("Required predictor column '%s' is missing from new_data.", col), call. = FALSE)
    }
  }

  # 3. Generate predictions (na.pass prevents it from crashing on unknown categories)
  raw_preds <- stats::predict(target_model, newdata = new_data, na.action = stats::na.pass)

  # 4. Categorize into discrete classes
  new_data$Predicted_FG_perc <- round(raw_preds, 2)
  new_data$Predicted_Class <- categorize_germination(raw_preds)

  return(new_data)
}
