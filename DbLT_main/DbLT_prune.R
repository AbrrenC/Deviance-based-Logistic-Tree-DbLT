
## This script contains all the functions which are used for DbLT tree model pruning.

# Function: Prune tree from bottom to top based on AIC or BIC
prune_tree_aic_bic <- function(node, type = "AIC", is_root = TRUE) {
  # Base case: if the node is NULL, return the node
  if (is.null(node)) {
    return(node)
  }
  # Recursively prune the left and right child nodes
  node$Left_child_node <- prune_tree_bottom_top(node$Left_child_node, type, is_root = FALSE)
  node$Right_child_node <- prune_tree_bottom_top(node$Right_child_node, type, is_root = FALSE)
  # Determine which metric to use for comparison
  parent_metric <- if (type == "AIC") node$parent_aic else node$parent_bic
  child_metric <- if (type == "AIC") node$child_aic else node$child_bic
  # Check if the required metric values are missing
  if (is.null(parent_metric) || is.null(child_metric)) {
    return(node)
  }
  # Prune the node by setting it to NULL
  if (parent_metric < child_metric && !is_root) {
    node <- NULL
  } else if (parent_metric < child_metric && is_root) {
    node$Left_child_node <- NULL
    node$Right_child_node <- NULL
    return(node)
  } else {
    return(node)
  }
  # Return the (possibly pruned) node
  return(node)
}

# Function: Prune tree from bottom up based on the defined EPV value
prune_tree_epv <- function(node, threshold = 10, is_root = TRUE) {
  # Base case: if the node is NULL, return the node
  if (is.null(node)) {
    return(node)
  }
  # Recursively prune the left and right child nodes
  node$Left_child_node <- prune_tree_epv(node$Left_child_node, threshold, is_root = FALSE)
  node$Right_child_node <- prune_tree_epv(node$Right_child_node, threshold, is_root = FALSE)
  # Check if the required metric values are missing
  EPV_node <- node$EPV_node
  if (is.null(EPV_node)) {
    return(node)
  }
  # Prune the node by setting it to NULL
  if (EPV_node < threshold && !is_root) {
    node <- NULL
  } else if (EPV_node < threshold && is_root) {
    node$Left_child_node <- NULL
    node$Right_child_node <- NULL
    return(node)
  } else {
    return(node)
  }
  # Return the pruned node
  return(node)
}

# Function: Create a function to combine the above pruning functions
DbLT_prune <- function(node, pruning_metric = "EPV", threshold = 10, is_root = TRUE) {
  # Determine the pruning strategy based on the pruning_metric
  if (pruning_metric %in% c("AIC", "BIC")) {
    # Prune using AIC or BIC method
    return(prune_tree_aic_bic(node, type = pruning_metric, is_root))
  } else if (pruning_metric == "EPV") {
    # Prune using the EPV method
    return(prune_tree_epv(node, threshold, is_root))
  } else {
    stop("Invalid pruning metric specified. Use 'EPV', 'AIC', or 'BIC'.")
  }
}

























