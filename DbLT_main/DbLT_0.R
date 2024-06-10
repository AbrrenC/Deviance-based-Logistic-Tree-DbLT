# This function is used to perform the analysis at the root node (zero node)

DbLT_0 <- function(response_var = "y", dataset, option = NULL) {
  # Determine the response variable column index to exclude it from predictors
  response_index <- which(names(dataset) == response_var)

  # Check if option is 'simple' or if option is NULL (treat NULL as defaulting to else condition)
  if (!is.null(option) && option == "simple") {
    # Title for simple logistic regression analysis
    cat("List of P-values for Variable Coefficients - Simple Logistic Regression with each variable:\n")

    # Initialize a list to store p-values and model diagnostics
    p_values_list <- list()
    model_diagnostics <- list()

    # Loop through each predictor variable in the dataset, excluding the response variable
    for (var in names(dataset)[-response_index]) {
      cat("\n--- P-values and Model Diagnostics for", var, "---\n")

      # Check if the variable is categorical (factor or character)
      if (is.factor(dataset[[var]]) || is.character(dataset[[var]])) {
        # Fit the logistic regression model treating the variable as categorical
        dataset[[var]] <- as.factor(dataset[[var]])  # Ensure it's treated as a factor
        formula <- as.formula(paste(response_var, "~", var))
        glm_model <- glm(formula, family = binomial, data = dataset)
        summary_glm <- summary(glm_model)

        # Extract the p-values for all categories of the variable
        p_values <- summary_glm$coefficients[-1, "Pr(>|z|)"]  # Exclude the intercept
        p_values_list[[var]] <- p_values

        # Display each category's p-value and model diagnostics
        for (cat in names(p_values)) {
          cat(cat, ":", p_values[cat], "\n")
        }
      } else {
        # Fit the logistic regression model for numerical predictors
        formula <- as.formula(paste(response_var, "~", var))
        glm_model <- glm(formula, family = binomial, data = dataset)
        summary_glm <- summary(glm_model)

        # Extract the p-value for the variable (excluding the intercept)
        if (length(coef(summary_glm)) > 1) {  # Ensure there's more than just an intercept
          p_value <- summary_glm$coefficients[-1, "Pr(>|z|)"]  # Second row is the variable
        } else {
          p_value <- NA  # In case the model didn't fit well or there was no variation
        }

        # Store and display the p-value
        p_values_list[[var]] <- p_value
        cat(var, ":", p_value, "\n")
      }

      # Extract model diagnostics
      deviance <- glm_model$deviance
      aic <- AIC(glm_model)
      bic <- BIC(glm_model)

      # Store model diagnostics
      model_diagnostics[[var]] <- list(Deviance = deviance, AIC = aic, BIC = bic)

      # Display model diagnostics
      cat("Deviance:", deviance, "\n")
      cat("AIC:", aic, "\n")
      cat("BIC:", bic, "\n")
    }
  } else {
    # Fit the logistic regression model using all variables except the response
    formula <- as.formula(paste(response_var, "~ ."))
    glm_model <- glm(formula, family = binomial, data = dataset)
    summary_glm <- summary(glm_model)
    p_values <- summary_glm$coefficients[-1, "Pr(>|z|)"]  # Exclude the intercept
    deviance <- glm_model$deviance

    # Display the p-values and deviance with a title
    cat("List of P-values for Variable Coefficients - Fit Entire Dataset:\n")
    cat("Deviance:", deviance, "\n")
    for (name in names(p_values)) {
      cat(name, ":", p_values[name], "\n")
    }
  }
}
