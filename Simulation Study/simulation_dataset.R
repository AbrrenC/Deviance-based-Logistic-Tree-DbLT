
# This script is used to store the simulation dataset creation function.

# Function 1: This function is used to create simulation dataset 1
dataset1_sim <- function(n) {
  # Simulate covariates x1, x2 (independent) and x3 (dependent with x1 and x2)
  x1 <- runif(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, and x3 levels
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i]
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


# Function 2: This function is used to create simulation dataset 2
dataset2_sim <- function(n) {
  # Simulate covariates x1, x2 (independent) and x3 (dependent with x1 and x2)
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, and x3 levels
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i]
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


# Function 3: This function is used to create simulation dataset 3
dataset3_sim <- function(n, beta) {
  # Simulate covariates x1, x2 (independent) and x3 (dependent with x1 and x2)
  x1 <- rnorm(n, 0, 1)
  x2 <- beta * x1 + rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, and x3 levels
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i]
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


# Function 4: This function is used to create simulation dataset 4 with variables x4 and x5
dataset4_sim <- function(n) {
  # Simulate covariates x1, x2 (independent) and x3 (dependent with x1 and x2)
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  x4 <- rnorm(n, 0, 1)
  x5 <- rnorm(n, 0, 1)
  x6 <- rnorm(n, 0, 1)
  y <- numeric(n) # Initializing y
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i] + 0*x4[i] + 0*x5[i] + 0*x6[i]
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


# Function 5: This function is used to for comparsion between models in terms of complex relationships
dataset5_sim <- function(n) {
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  x4 <- rnorm(n, 0, 1)
  x5 <- rnorm(n, 0, 1)
  x6 <- x1 * x2 # Interaction term
  x7 <- x1^2 # Non-linear term
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, x3 levels, and interactions
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i] + 0 * x4[i] + 0 * x5[i] + 1.5 * x6[i] - 1.2 * x7[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i] + 0 * x4[i] + 0 * x5[i] + 0.5 * x6[i] + 0.5 * x7[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i] + 0 * x4[i] + 0 * x5[i] - 1 * x6[i] - 0.5 * x7[i]
    }
    # Simulating y based on log odds
    p <- exp(log_odd) / (1 + exp(log_odd))
    y[i] <- rbinom(1, 1, p)
  }

  # Create a data frame
  y <- as.factor(y)
  dataset <- data.frame(x1, x2, x3, x4, x5, x6, x7, y)

  return (dataset)
}


# Function 6: This function is used to create simulation dataset 5 with interaction and non-linear terms
dataset6_sim <- function(n) {
  x1 <- rnorm(n, 0, 1)
  x2 <- 1.5 * x1 + rnorm(n, 0, 1)
  x3 <- as.factor(sample(1:6, n, replace = TRUE))
  x4 <- rnorm(n, 0, 1)
  x5 <- rnorm(n, 0, 1)
  x6 <- x1 * x2 # Interaction term
  x7 <- x1^2 # Non-linear term
  y <- numeric(n) # Initializing y

  # Simulating y based on x1, x2, x3 levels, and interactions
  for (i in 1:n) {
    if (x3[i] %in% c(1, 2)) {
      log_odd <- -1 * x1[i] + 0.5 * x2[i] + 0 * x4[i] + 0 * x5[i] + 1.5 * x6[i] - 1.2 * x7[i]
    } else if (x3[i] %in% c(3, 4)) {
      log_odd <- -0.5 * x1[i] + 1.5 * x2[i] + 0 * x4[i] + 0 * x5[i] + 0.5 * x6[i] + 0.5 * x7[i]
    } else {
      log_odd <- 1 * x1[i] - 0.5 * x2[i] + 0 * x4[i] + 0 * x5[i] - 1 * x6[i] - 0.5 * x7[i]
    }
    # Simulating y based on log odds
    p <- exp(log_odd) / (1 + exp(log_odd))
    y[i] <- rbinom(1, 1, p)
  }

  # Create a data frame
  y <- as.factor(y)
  dataset <- data.frame(x1, x2, x3, x4, x5, x6, x7, y)

  return (dataset)
}







