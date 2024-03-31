
## This file stores all the functions which are used build up the logistic tree model
## There are two main types of functions: categorical and numerical variables

## Import necessary packages
library(data.tree)
library(DiagrammeR)

## Section 1: Categorical variable

## Function 1: Select the best categorical variable as split variable
cat_split_var <- function(data, response_var) {
  # Step 1: identify categorical variables
  all_vars <- names(data)
  categorical_vars <- c()
  for (var in all_vars) {
    # Exclude the response variable y
    if (var != response_var && (class(data[[var]]) == "factor" || class(data[[var]]) == "character")) {
      categorical_vars <- c(categorical_vars, var)
    }
  }

  # Step 2: perform chi-square test for each categorical variable and select the one with the smallest p-value
  significance_prob <- c()
  for (var in categorical_vars) {
    contingency_table <- table(data[[var]], data[[response_var]])
    chi_square_test <- chisq.test(contingency_table)
    # Save the p-value into significance_prob list
    significance_prob <- c(significance_prob, chi_square_test$p.value)
  }

  smallest_p_value <- min(significance_prob)
  smallest_p_value_variable <- categorical_vars[which.min(significance_prob)]

  return(smallest_p_value_variable)
}

## Function 2: Select the split point of the categorical split variable
cat_split_func <- function(data, response_var) {
  categorical_var <- cat_split_var(data, response_var)
  unique_classes <- unique(data[[categorical_var]])
  data <- data[order(data[[categorical_var]]), ]
  split_results <- list()
  best_split_point <- NULL
  min_sum_deviance <- Inf

  # Determine loop condition based on the number of unique classes
  loop_condition <- ifelse(length(unique_classes) == 2, length(unique_classes) - 1, length(unique_classes))

  # Iterate over each unique class to split the data
  for (i in 1:loop_condition) {
    class_value <- levels(unique_classes)[i]
    subset1 <- data[data[[categorical_var]] %in% unique_classes[1:i], ]
    subset2 <- data[data[[categorical_var]] %in% unique_classes[(i + 1):length(unique_classes)], ]

    if (nlevels(factor(subset1[[categorical_var]])) == 1) {
      subset1 <- subset1[, !names(subset1) %in% categorical_var]
    }
    if (nlevels(factor(subset2[[categorical_var]])) == 1) {
      subset2 <- subset2[, !names(subset2) %in% categorical_var]
    }

    # Initialize deviance values
    deviance1 <- Inf
    deviance2 <- Inf

    # Try to fit the glm model for both subsets and calculate deviance
    try({
      model1 <- glm(formula = as.formula(paste(response_var, "~ .")), data = subset1, family = binomial)
      deviance1 <- deviance(model1)
    }, silent = TRUE)

    try({
      model2 <- glm(formula = as.formula(paste(response_var, "~ .")), data = subset2, family = binomial)
      deviance2 <- deviance(model2)
    }, silent = TRUE)

    # Calculate the sum of deviance if both models converged
    if (deviance1 < Inf && deviance2 < Inf) {
      sum_deviance <- deviance1 + deviance2
      split_results[[as.character(class_value)]] <- sum_deviance

      if (sum_deviance < min_sum_deviance) {
        min_sum_deviance <- sum_deviance
        best_split_point <- class_value
      }
    }
  }

  return(list(split_results = split_results,
              best_split_point = as.numeric(best_split_point),
              best_split_variable = categorical_var,
              min_sum_deviance = min_sum_deviance)
  )
}


## Section 2: Numerical variable

## Function: Select the best numerical variable as split variable
num_split_var <- function(data, response_var) {
  # Initialize variables
  deviance_hat_list <- list()
  best_variable <- NULL

  # Get numerical variables
  numerical_vars <- sapply(data, is.numeric)

  # Remove response variable if it's numeric
  if (is.numeric(data[[response_var]])) {
    numerical_vars[response_var] <- FALSE
  }

  # Iterate over each numerical variable
  for (variable in names(data)[numerical_vars]) {
    # Build the model
    current_formula <- as.formula(paste("factor(",response_var, ")~", variable))

    # Try to fit the GLM model, skip if it does not converge
    try({
      glm_model <- glm(current_formula, data = data, family = binomial)

      # Calculate deviance_hat
      deviance_hat <- glm_model$deviance / glm_model$df.residual

      # Store the deviance_hat and variable name in the list
      deviance_hat_list[[variable]] <- deviance_hat

      # Update variables if deviance_hat is smaller
      if (is.null(best_variable) || deviance_hat < deviance_hat_list[[best_variable]]) {
        best_variable <- variable
      }
    }, silent = TRUE)  # silent = TRUE suppresses warning messages
  }

  # Return the best numerical variable
  return(best_variable)
}


## Function: remove categorical or numeric variable when it only has one class
# remove_constant_vars <- function(dataset, response_var) {
#   response_var_index <- which(names(dataset) == response_var)
#   other_vars_indices <- setdiff(1:ncol(dataset), response_var_index)
#   constant_vars <- sapply(dataset[, other_vars_indices, drop = FALSE], function(x) {
#     (is.factor(x) && length(unique(x)) == 1)
#   })
#   constant_vars_full <- rep(FALSE, ncol(dataset))
#   constant_vars_full[other_vars_indices] <- constant_vars
#   cleaned_dataset <- dataset[, !constant_vars_full, drop = FALSE]
#   return(cleaned_dataset)
# }
remove_constant_vars <- function(dataset, response_var) {
  response_var_index <- which(names(dataset) == response_var)
  other_vars_indices <- setdiff(1:ncol(dataset), response_var_index)
  constant_vars <- sapply(dataset[, other_vars_indices, drop = FALSE], function(x) {
    if (is.factor(x)) {
      # Check if there's only one level or if each level only has one case
      length(unique(x)) == 1 || all(table(x) <= 1)
    } else {
      FALSE
    }
  })
  constant_vars_full <- rep(FALSE, ncol(dataset))
  constant_vars_full[other_vars_indices] <- constant_vars
  cleaned_dataset <- dataset[, !constant_vars_full, drop = FALSE]
  return(cleaned_dataset)
}


## Function: Select the best split point
num_split_func <- function(data, response_var) {
  split_variable <- num_split_var(data, response_var)

  # If no suitable variable is found, return NULL
  if (is.null(split_variable)) {
    return(NULL)
  }

  # Sort the dataset based on the selected variable
  sorted_dataset <- data[order(data[[split_variable]]), ]
  rownames(sorted_dataset) <- NULL

  # Define quantiles to search
  # quantiles <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  quantiles <- c(0.3, 0.4, 0.5, 0.6, 0.7)

  # Initialize variables to store results
  best_quantile <- 0
  min_residual_deviance <- Inf
  best_variable_value <- NULL

  # Iterate through quantiles
  for (quantile_value in quantiles) {
    # Split the dataset based on the quantile of the selected variable
    split_index <- round(nrow(sorted_dataset) * quantile_value)
    subset1_raw <- sorted_dataset[1:split_index, ]
    subset2_raw <- sorted_dataset[(split_index + 1):nrow(sorted_dataset), ]

    # Remove the variables when there is only one class
    subset1 <- remove_constant_vars(subset1_raw, response_var)
    subset2 <- remove_constant_vars(subset2_raw, response_var)

    # Initialize deviance values
    deviance1 <- 0
    deviance2 <- 0

    # Check and update deviance for subset1
    if (length(unique(subset1[[response_var]])) > 1) {
      try({
        model1 <- glm(formula = as.formula(paste(response_var, "~ .")), data = subset1, family = binomial)
        deviance1 <- deviance(model1)
      }, silent = TRUE)
    }

    # Check and update deviance for subset2
    if (length(unique(subset2[[response_var]])) > 1) {
      try({
        model2 <- glm(formula = as.formula(paste(response_var, "~ .")), data = subset2, family = binomial)
        deviance2 <- deviance(model2)
      }, silent = TRUE)
    }

    # Calculate total sum of residual deviance
    total_deviance <- deviance1 + deviance2

    # Update the best quantile if the current one has a smaller residual deviance
    if (total_deviance < min_residual_deviance && total_deviance > 0) {  # Ensure that deviance is non-zero
      min_residual_deviance <- total_deviance
      best_quantile <- quantile_value
      best_variable_value <- sorted_dataset[[split_variable]][split_index]
    }
  }

  # Return the best quantile
  return(list(best_split_variable = split_variable,
              best_split_value = best_variable_value)
  )
}


## Function: determine variable types
determine_var_types <- function(data, response_var) {
  num_var_list <- character(0)  # Initialize empty list for numerical variables
  cat_var_list <- character(0)  # Initialize empty list for categorical variables

  # Remove response variable from consideration if provided
  if (!is.null(response_var)) {
    data <- data[, !(names(data) %in% response_var), drop = FALSE]
  }

  # Loop through each column of the dataset
  for (col_name in names(data)) {
    # Check the data type of the column
    col_type <- class(data[[col_name]])

    # If the column type is numeric, add it to numerical variables list
    if (col_type %in% c("numeric", "integer")) {
      num_var_list <- c(num_var_list, col_name)
    }
    # If the column type is factor or character, add it to categorical variables list
    else if (col_type %in% c("factor", "character")) {
      cat_var_list <- c(cat_var_list, col_name)
    }
  }

  # Return the lists of numerical and categorical variables
  return(list(num_var_list = num_var_list,
              cat_var_list = cat_var_list)
        )
}


## Section 3: Split the dataset

## Function: split function to determine the split variable, split point and variable type
split_node_func <- function(data, response_var, threshold) {
  # Determine the variable types
  var_types <- determine_var_types(data, response_var)
  cat_vars <- var_types$cat_var_list
  num_vars <- var_types$num_var_list

  split_var <- NULL
  split_value <- NULL
  variable_type <- NULL
  categorical_split <- FALSE
  best_p_value <- Inf  # Initialize with a large value

  # Check if there are categorical variables with at least 2 classes
  if (length(cat_vars) > 0) {
    # Check if any categorical variable has at least 2 classes
    for (var in cat_vars) {
      if (length(unique(data[[var]])) >= 2) {
        # Calculate expected frequencies for chi-square test
        contingency_table <- table(data[[var]], data[[response_var]])
        expected_freq <- chisq.test(contingency_table, simulate.p.value = TRUE)$expected

        # Check if all expected frequencies are >= 5
        if (all(expected_freq >= 5)) {
          # Calculate chi-square test between categorical variable and response_var
          # Use try-catch to skip iteration if chi-square test fails or returns an error
          try({
            chi_sq_test <- chisq.test(contingency_table)
            # If p-value is less than threshold and smaller than the current best_p_value, update
            if (chi_sq_test$p.value < threshold && chi_sq_test$p.value < best_p_value) {
              split_var <- var
              split_value <- cat_split_func(data, response_var)$best_split_point
              variable_type <- "category"
              categorical_split <- TRUE
              best_p_value <- chi_sq_test$p.value
            }
          }, silent = TRUE)  # silent = TRUE suppresses error messages
        }
      }
    }
  }

  if (!categorical_split && length(num_vars) > 0) {
    # Use num_split_func if there are no suitable categorical variables or none met the threshold
    split_info <- num_split_func(data, response_var)
    split_var <- split_info$best_split_variable
    split_value <- split_info$best_split_value
    variable_type <- "numeric"
  }

  # Return the split variable, value, and variable type
  return(list(split_var = split_var,
              split_value = split_value,
              variable_type = variable_type)
  )
}


## Function: Split the data into two subsets based on the split variable and split point
split_subsets <- function(data, response_var, threshold) {
  # determine the split variable and split value
  split_node_info <- split_node_func(data, response_var, threshold)

  if (is.null(split_node_info$split_var) || split_node_info$split_var == "") {
    return(NULL)
  }

  split_var <- split_node_info$split_var
  split_value <- as.character(split_node_info$split_value)
  variable_type <- split_node_info$variable_type

  if (variable_type == "category") {
    # Sort the data by the split variable
    sorted_data <- data[order(data[[split_var]]), ]
    rownames(sorted_data) <- NULL

    # Find the index of the split class
    split_index <- max(which(sorted_data[[split_var]] == split_value))

    # Create subsets based on the split class
    subset_left <- sorted_data[1:split_index, ]
    subset_right <- sorted_data[(split_index + 1):nrow(sorted_data), ]

    # Recode the factor variable
    subset_left[[split_var]] <- factor(subset_left[[split_var]], levels = unique(subset_left[[split_var]]))
    subset_right[[split_var]] <- factor(subset_right[[split_var]], levels = unique(subset_right[[split_var]]))
  } else if (variable_type == "numeric") {
    # Sort the data by the split variable
    sorted_data <- data[order(data[[split_var]]), ]

    # Find the index of the split point
    split_index <- sum(sorted_data[[split_var]] <= split_value)

    # Create subsets based on the split point
    subset_left <- sorted_data[1:split_index, ]
    subset_right <- sorted_data[(split_index + 1):nrow(sorted_data), ]
  } else {
    stop("Invalid variable type. It should be either 'category' or 'numeric'")
  }

  rownames(subset_left) <- NULL
  rownames(subset_right) <- NULL

  return(list(subset_left = subset_left,
              subset_right = subset_right,
              split_var = split_var,
              split_value = split_value)
        )
}


## Section 4: Build up tree structure

## Function: Add child nodes
add_child_nodes <- function(parent_node, child_results) {
  if (is.null(child_results)) {
    return()
  }

  left_child_data <- paste(paste("n =", child_results$Data_child_nodes["left_set"]),
                           child_results$Left_child_node$Node_detail,
                           paste(paste("y = 0:", child_results$response_distri[["left_0"]]), "|", paste("y = 1:", child_results$response_distri[["left_1"]])),
                           sep = "\n")
  right_child_data <- paste(paste("n =", child_results$Data_child_nodes["right_set"]),
                            child_results$Right_child_node$Node_detail,
                            paste(paste("y = 0:", child_results$response_distri[["right_0"]]), "|", paste("y = 1:", child_results$response_distri[["right_1"]])),
                            sep = "\n")

  # Add left child node and its children
  left_child_node <- parent_node$AddChild(left_child_data)
  if (!is.null(child_results$Left_child_node)) {
    add_child_nodes(left_child_node, child_results$Left_child_node)
  }

  # Add right child node and its children
  right_child_node <- parent_node$AddChild(right_child_data)
  if (!is.null(child_results$Right_child_node)) {
    add_child_nodes(right_child_node, child_results$Right_child_node)
  }
}

## Function: Build up the tree structure
create_tree_structure <- function(results) {
  # Create the root node
  tree <- Node$new(paste(paste("n =", sum(results$Data_child_nodes)),
                         results$Node_detail,
                         sep = "\n"))

  # Create Initial child nodes
  left_node_init <- tree$AddChild(paste(paste("n =", results$Data_child_nodes["left_set"]),
                                        results$Left_child_node$Node_detail,
                                        paste(paste("y = 0:", results$response_distri[["left_0"]]), "|", paste("y = 1:", results$response_distri[["left_1"]])),
                                        sep = "\n"))
  right_node_init <- tree$AddChild(paste(paste("n =",
                                         results$Data_child_nodes["right_set"]),
                                         results$Right_child_node$Node_detail,
                                         paste(paste("y = 0:", results$response_distri[["right_0"]]), "|", paste("y = 1:", results$response_distri[["right_1"]])),
                                         sep = "\n"))

  # Add left child node and its children
  add_child_nodes(left_node_init, results$Left_child_node)

  # Add right child node and its children
  add_child_nodes(right_node_init, results$Right_child_node)

  return(tree)
}

## Function: Extract Decision rules from the tree results
extract_data <- function(model_outputs) {
  data <- list(
    Node_detail = model_outputs$Node_detail,
    response_distri = model_outputs$response_distri
  )

  if (!is.null(model_outputs$Left_child_node)) {
    data$Left_child_node <- extract_data(model_outputs$Left_child_node)
  }

  if (!is.null(model_outputs$Right_child_node)) {
    data$Right_child_node <- extract_data(model_outputs$Right_child_node)
  }

  return(data)
}


## Function to check stopping criteria (pre-pruning criteria)
check_base_cases <- function(data, response_var, min_sample, max_depth, current_depth, method) {
  if (length(unique(data[[response_var]])) == 1) {
    return(TRUE)
  }
  if (!is.null(min_sample) && nrow(data) < min_sample) {
    return(TRUE)
  }
  if (!is.null(max_depth) && current_depth >= max_depth) {
    return(TRUE)
  }
  if (is.null(min_sample) && is.null(max_depth)) {
    if (method == "class" && any(table(data[[response_var]]) == nrow(data))) {
      return(TRUE)
    } else if (method == "probability" && any(round(prop.table(table(data[[response_var]])), 4) == 1)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


## Function to compute response distribution
compute_response_distribution <- function(subset, response_var, method) {
  if (method == "class") {
    response_0_count <- table(subset[[response_var]])[[1]]
    response_1_count <- ifelse(length(table(subset[[response_var]])) >= 2, table(subset[[response_var]])[[2]], 0)
    return(c(response_0_count, response_1_count))
  } else { # method == "probability"
    response_0_percent <- round((table(subset[response_var])[[1]]) / sum(table(subset[response_var])), 4)
    response_1_percent <- ifelse(length(table(subset[response_var])) >= 2, round((table(subset[response_var])[[2]]) / sum(table(subset[response_var])), 4), 0)
    return(c(response_0_percent, response_1_percent))
  }
}


## Function to build up the tree for plot
DbLTree_model <- function(data, response_var, min_sample = NULL, threshold, max_depth = NULL, current_depth = 0, method = "probability") {
  # check depth limit
  if (current_depth > 20) {
    return(NULL)
  }

  # check base case
  if (check_base_cases(data, response_var, min_sample, max_depth, current_depth, method)) {
    return(NULL)
  }

  # remove the variable when it only has one level
  cleaned_data <- remove_constant_vars(data, response_var)

  # check EPV for the parent node
  num_events_parent <- table(data[[paste0(response_var)]])[["1"]]
  num_predictors_parent <- ncol(data) - 1
  EPV_parent <- round(num_events_parent/num_predictors_parent, 0)

  # fit glm model with current node
  parent_glm = glm(paste0(response_var, "~."), data = cleaned_data, family = binomial, control = list(maxit = 50))
  if (!parent_glm$converged) {
    return(NULL)  # Handle non-convergence
  }
  summary_parent_glm = summary(parent_glm)
  parent_aic = AIC(parent_glm)
  parent_bic = BIC(parent_glm)
  parent_deviance_hat = summary_parent_glm$deviance/summary_parent_glm$df.residual

  # split the dataset
  subsets = split_subsets(data, response_var, threshold)

  # construct split description
  split_desc <- if (is.factor(data[[subsets$split_var]])) {
    paste("cat", subsets$split_var, "split at class", subsets$split_value)
  } else {
    paste("num", subsets$split_var, "<= & >", subsets$split_value)
  }

  # compute subset sizes and response distribution
  left_size <- nrow(subsets$subset_left)
  right_size <- nrow(subsets$subset_right)
  left_response_distri <- compute_response_distribution(subsets$subset_left, response_var, method)
  right_response_distri <- compute_response_distribution(subsets$subset_right, response_var, method)

  # clean up both left and right subsets before fitting the glm model
  cleaned_subset_left = remove_constant_vars(subsets$subset_left, response_var)
  cleaned_subset_right = remove_constant_vars(subsets$subset_right, response_var)

  # fit the glm model
  left_glm <- glm(paste0(response_var, "~."), data = cleaned_subset_left, family = binomial, control = list(maxit = 50))
  right_glm <- glm(paste0(response_var, "~."), data = cleaned_subset_right, family = binomial, control = list(maxit = 50))

  left_coefficients = left_glm$coefficients
  right_coefficients = right_glm$coefficients
  left_aic = AIC(left_glm)
  right_aic = AIC(right_glm)
  left_bic = BIC(left_glm)
  right_bic = BIC(right_glm)

  # Replace NA coefficients with 0
  left_coefficients[is.na(left_coefficients)] <- 0
  right_coefficients[is.na(right_coefficients)] <- 0

  # recursively split subsets
  Left_child_node <- DbLTree_model(cleaned_subset_left, response_var, min_sample, threshold, max_depth, current_depth + 1, method)
  Right_child_node <- DbLTree_model(cleaned_subset_right, response_var, min_sample, threshold, max_depth, current_depth + 1, method)

  # construct and return the tree structure
  tree <- list(
    Node_detail = split_desc,
    parent_aic = parent_aic,
    parent_bic = parent_bic,
    parent_deviance_hat = parent_deviance_hat,
    Data_child_nodes = c(left_set = left_size, right_set = right_size),
    response_distri = c(left_0 = left_response_distri[1], left_1 = left_response_distri[2], right_0 = right_response_distri[1], right_1 = right_response_distri[2]),
    left_coefficients = left_coefficients,
    right_coefficients = right_coefficients,
    child_aic = (left_aic + right_aic),
    child_bic = (left_bic + right_bic),
    EPV_node = EPV_parent
  )

  # clean up the child nodes
  if (!is.null(Left_child_node)) {
    tree$Left_child_node <- Left_child_node
  }
  if (!is.null(Right_child_node)) {
    tree$Right_child_node <- Right_child_node
  }

  return(tree)
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
DbLT_predict_model <- function(node, dataset, type = "probability") {
  # Initialize an empty list to store prediction probabilities
  prediction_probs <- list()
  binary_prediction <- vector("numeric", nrow(dataset)) # Initialize a numeric vector for binary predictions

  # Iterate over each row of the dataframe
  for (i in 1:nrow(dataset)) {
    data_point <- dataset[i, ]
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


################################################################################
# The following are new helpers functions
# Function: calculate the linear predictor
# Function to calculate the linear predictor
calculate_linear_predictor <- function(data, model_coefficients) {
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
  return(linear_predictor)
}


# Function: Calculate the predicted probabilities using the linear predictor
# Function to calculate the linear predictor
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














