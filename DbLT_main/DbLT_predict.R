
## This script contains all the functions which are used for DbLT tree model prediction.

# Function: Calculate the predicted probabilities using the linear predictor
calculate_predicted_probability <- function(data, model_coefficients) {
  # Initialize the linear predictor with the intercept
  linear_predictor <- rep(model_coefficients[["(Intercept)"]], nrow(data))
  # Add the contribution of each variable
  for (var_name in names(data)) {
    if (var_name %in% names(model_coefficients)) {
      # For numeric variables, simply multiply the term by the variable's value
      term <- model_coefficients[[var_name]]
      linear_predictor <- linear_predictor + term * data[[var_name]]
    } else if (is.factor(data[[var_name]])) {
      # For categorical variables, find matching coefficients for each level
      level_coefs <- grep(paste0("^", var_name, "\\d+$"), names(model_coefficients), value = TRUE)
      for (coef_name in level_coefs) {
        term <- model_coefficients[[coef_name]]
        level <- as.numeric(gsub(var_name, "", coef_name))
        # Add the term multiplied by an indicator (0 or 1) for the corresponding level
        linear_predictor <- linear_predictor + term * (as.numeric(as.character(data[[var_name]])) == level)
      }
    }
  }
  # Apply the logistic function to get predicted probabilities
  predicted_probability <- 1 / (1 + exp(-linear_predictor))
  return(predicted_probability)
}

## Function: Predict the new data point
DbLT_predict_model_func <- function(node, data_point) {
  # root node
  decision_rule <- node$Node_detail
  split_var <- strsplit(decision_rule, "\\s+")[[1]][2]
  operator <- strsplit(decision_rule, "\\s+")[[1]][3]
  threshold <- strsplit(decision_rule, "\\s+")[[1]][6] # convert it to number for comparison

  # Extract feature value from data_point
  new_data <- data_point[[split_var]]

  # Make decision based on the rule
  if (operator == "split") {
    # categorical variable split by class
    if ((as.numeric(new_data) <= as.numeric(threshold)) & !is.null(node$Left_child_node)) {
      DbLT_predict_model_func(node$Left_child_node, data_point) # repeat the function again
    } else if ((as.numeric(new_data) <= as.numeric(threshold)) & is.null(node$Left_child_node)) {
      left_prob_class_1 = as.numeric(calculate_predicted_probability(data_point, node$left_coefficients))
      return(c(class_0 = 1-left_prob_class_1, class_1 = left_prob_class_1))
    } else if ((as.numeric(new_data) > as.numeric(threshold)) & !is.null(node$Right_child_node)) {
      DbLT_predict_model_func(node$Right_child_node, data_point) # repeat the function again
    } else if ((as.numeric(new_data) > as.numeric(threshold)) & is.null(node$Right_child_node)) {
      right_prob_class_1 = as.numeric(calculate_predicted_probability(data_point, node$right_coefficients))
      return(c(class_0 = 1-right_prob_class_1, class_1 = right_prob_class_1))
    } else {
      return(c(class_0 = NA, class_1 = NA))
    }

  } else if (operator == "<=") {
    if ((as.numeric(new_data) <= as.numeric(threshold)) & !is.null(node$Left_child_node)) {
      DbLT_predict_model_func(node$Left_child_node, data_point) # repeat the function again
    } else if ((as.numeric(new_data) <= as.numeric(threshold)) & is.null(node$Left_child_node)) {
      left_prob_class_1 = as.numeric(calculate_predicted_probability(data_point, node$left_coefficients))
      return(c(class_0 = 1-left_prob_class_1, class_1 = left_prob_class_1))
    } else if ((as.numeric(new_data) > as.numeric(threshold)) & !is.null(node$Right_child_node)) {
      DbLT_predict_model_func(node$Right_child_node, data_point) # repeat the function again
    } else if ((as.numeric(new_data) > as.numeric(threshold)) & is.null(node$Right_child_node)) {
      right_prob_class_1 = as.numeric(calculate_predicted_probability(data_point, node$right_coefficients))
      return(c(class_0 = 1-right_prob_class_1, class_1 = right_prob_class_1))
    } else {
      return(c(class_0 = NA, class_1 = NA))
    }
  }
}

# Function:Make predictions for whole dataset
DbLT_predict <- function(node, dataset, type = "probability") {
  # Initialize an empty list to store prediction probabilities
  prediction_probs <- list()
  binary_prediction <- vector("numeric", nrow(dataset)) # Initialize a numeric vector for binary predictions

  # Iterate over each row of the dataframe
  for (i in 1:nrow(dataset)) {
    data_point <- dataset[i, , drop = FALSE]
    prob <- DbLT_predict_model_func(node, data_point)
    prediction_probs[[i]] <- prob
  }
  prediction_probs <- do.call(rbind, prediction_probs)
  colnames(prediction_probs) <- c("class_0", "class_1")

  # Determine the type of outputs
  if (type == "class") {
    for (i in 1:nrow(prediction_probs)) {
      if (prediction_probs[i, "class_0"] > prediction_probs[i, "class_1"]) {
        binary_prediction[i] <- 0
      } else {
        binary_prediction[i] <- 1
      }
    }
    return(binary_prediction)
  } else {
    return(prediction_probs)
  }
}
