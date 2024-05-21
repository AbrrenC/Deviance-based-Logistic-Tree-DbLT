# This script is used to store the simulation dataset creation function.

dataset1_sim <- function(n) {
  # Simulate covariates x1, x2 (independent) and x3 (dependent with x1 and x2)
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, and x3 levels
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- 0.3 - 2 * x1[i] + 2 * x2[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- 0.3 - 1 * x1[i] + 1 * x2[i]
    } else {
      log_odd <- 0.3 - 0.5 * x1[i] + 0.5 * x2[i]
    }
    # Simulating y based on log odds
    p <- exp(log_odd) / (1 + exp(log_odd))
    y[i] <- rbinom(1, 1, p)
  }

  # Create a data frame
  y <- as.factor(y)
  dataset <- data.frame(x1, x2, x3, y)
  return(dataset)
}

# Function is used to generate multi-normal distribution data
generate_multinorm_data <- function(n, num_vars, rho) {
  # Create a correlation matrix
  correlation_matrix <- matrix(rho, nrow = num_vars, ncol = num_vars)
  diag(correlation_matrix) <- 1  # Set the variance of each variable is 1

  # Generate data from a multivariate normal distribution
  data <- mvrnorm(n = n, mu = rep(0, num_vars), Sigma = correlation_matrix)

  # Naming the variables x1 to x10
  colnames(data) <- paste0("x", 1:num_vars)

  return(data)
}

# Function 2: This function is used to create simulation dataset for variable correlation
dataset2_sim <- function(n, num_vars, rho) {
  # Generate data from a multivariate normal distribution
  data = generate_multinorm_data(n, num_vars, rho)

  # Calculate the log-odd value based on x1 and x2
  log_odd <- 0.4 - 1 * data[, "x1"] + 1 * data[, "x2"]

  # Simulating y based on log odds
  p <- exp(log_odd) / (1 + exp(log_odd))
  y <- rbinom(n, 1, p)

  # Create a data frame and add y as a factor
  dataset <- as.data.frame(data)
  dataset$y <- as.factor(y)

  return(dataset)
}

# Function 3: This function is used to create simulation dataset for variable selection
dataset3_sim <- function(n) {
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  x4 <- rnorm(n, 0, 1)
  x5 <- rnorm(n, 0, 1)
  x6 <- rnorm(n, 0, 1)
  y <- numeric(n) # Initializing y

  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- 0.3 - 2 * x1[i] + 2 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- 0.3 - 1 * x1[i] + 1 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
    } else {
      log_odd <- 0.3 - 0.5 * x1[i] + 0.5 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
    }

    # Simulating y based on log odds
    p <- exp(log_odd) / (1 + exp(log_odd))
    y[i] <- rbinom(1, 1, p)
  }

  # Create a data frame
  y <- as.factor(y)
  dataset <- data.frame(x1, x2, x3, x4, x5, x6, y)

  return(dataset)
}




