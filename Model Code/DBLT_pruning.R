
## This script stores all the functions which are used for DbLT tree model pruning.

# Function: Extract leaf nodes from the prediction tree using the validation dataset
extract_leaf_nodes_prediction <- function(predictions, current_path = "Root", leaf_nodes = list()) {
  left_path <- paste(current_path, "Left", sep = "_")
  right_path <- paste(current_path, "Right", sep = "_")

  has_left <- !is.null(predictions[[left_path]])
  has_right <- !is.null(predictions[[right_path]])

  if (!has_left && !has_right) {
    # Current node is a leaf node
    leaf_nodes[[current_path]] <- predictions[[current_path]]$response_distri
  } else {
    if (has_left) {
      leaf_nodes <- extract_leaf_nodes_prediction(predictions, left_path, leaf_nodes)
    }

    if (has_right) {
      leaf_nodes <- extract_leaf_nodes_prediction(predictions, right_path, leaf_nodes)
    }

    if (has_left && !has_right) {
      # Right child is a leaf node
      leaf_nodes[[right_path]] <- predictions[[current_path]]$response_distri[c("right_0", "right_1")]
    }

    if (!has_left && has_right) {
      # Left child is a leaf node
      leaf_nodes[[left_path]] <- predictions[[current_path]]$response_distri[c("left_0", "left_1")]
    }
  }

  return(leaf_nodes)
}


# Function: Extract leaf nodes from the tree output
extract_leaf_nodes <- function(node) {
  leaf_nodes <- list()
  # function traverse to go through all the possible subnodes
  traverse_tree <- function(node) {
    if (is.null(node$Left_child_node) && is.null(node$Right_child_node)) {  # condition 1: no leaf and right child nodes
      leaf_nodes <<- append(leaf_nodes, list(node$response_distri))
    } else {
      if (!is.null(node$Left_child_node) && is.null(node$Right_child_node)) { # condition 2: no right child node
        leaf_nodes <<- append(leaf_nodes, list(node$response_distri[3:4]))
        traverse_tree(node$Left_child_node)
      } else if (is.null(node$Left_child_node) && !is.null(node$Right_child_node)) { # condition 3: no left child node
        leaf_nodes <<- append(leaf_nodes, list(node$response_distri[1:2]))
        traverse_tree(node$Right_child_node)
      } else {
        traverse_tree(node$Left_child_node)
        traverse_tree(node$Right_child_node)
      }
    }
  }
  traverse_tree(node)
  return(leaf_nodes)
}


# Function: Prune tree from top to bottom based on AIC or BIC
prune_tree_top_bottom <- function(node, type = "AIC", is_root = TRUE) {
  # Check if the node is NULL
  if (is.null(node)) {
    return(node)
  }
  # Determine which metric to use for comparison
  parent_metric <- if (type == "AIC") node$parent_aic else node$parent_bic
  child_metric <- if (type == "AIC") node$child_aic else node$child_bic
  # Check if the required metric values are missing
  if (is.null(parent_metric) || is.null(child_metric)) {
    return(node)
  }
  # Compare parent_metric with child_metric
  if (parent_metric < child_metric && !is_root) {
    # Prune the node by setting it to NULL
    node <- NULL
  } else if (parent_metric < child_metric && is_root) {
    node$Left_child_node <- NULL
    node$Right_child_node <- NULL
    return(node)
  } else {
    # Recursively process the left and right child nodes
    node$Left_child_node <- prune_tree_top_bottom(node$Left_child_node, type, is_root = FALSE)
    node$Right_child_node <- prune_tree_top_bottom(node$Right_child_node, type, is_root = FALSE)
  }
  # Return the pruned node
  return(node)
}


# Function: Prune tree from bottom to top based on AIC or BIC
prune_tree_bottom_top <- function(node, type = "AIC", threshold = 0.05, is_root = TRUE) {
  # Base case: if the node is NULL, return the node
  if (is.null(node)) {
    return(node)
  }
  # Recursively prune the left and right child nodes
  node$Left_child_node <- prune_tree_bottom_top(node$Left_child_node, type, threshold, is_root = FALSE)
  node$Right_child_node <- prune_tree_bottom_top(node$Right_child_node, type, threshold, is_root = FALSE)
  # Determine which metric to use for comparison
  parent_metric <- if (type == "AIC") node$parent_aic else node$parent_bic
  child_metric <- if (type == "AIC") node$child_aic else node$child_bic
  # Check if the required metric values are missing
  if (is.null(parent_metric) || is.null(child_metric)) {
    return(node)
  }
  # Prune the node by setting it to NULL
  if (parent_metric < child_metric * (1 - threshold) && !is_root) {
    node <- NULL
  } else if (parent_metric < child_metric * (1 - threshold) && is_root) {
    node$Left_child_node <- NULL
    node$Right_child_node <- NULL
    return(node)
  } else {
    return(node)
  }
  # Return the (possibly pruned) node
  return(node)
}


# Function: extract the decision rules from the model outputs
extract_decision_rules <- function(model_outputs) {
  data <- list(
    Node_detail = model_outputs$Node_detail
  )

  if (!is.null(model_outputs$Left_child_node)) {
    data$Left_child_node <- extract_decision_rules(model_outputs$Left_child_node)
  }

  if (!is.null(model_outputs$Right_child_node)) {
    data$Right_child_node <- extract_decision_rules(model_outputs$Right_child_node)
  }

  return(data)
}


# Function: extract the decision variable names from the decision rules
extract_decision_vars <- function(model_outputs) {
  tree <- extract_decision_rules(model_outputs)
  variable_names <- c()
  if ("Node_detail" %in% names(tree)) {
    # Extract variable name from the Node_detail
    variable_name <- strsplit(tree$Node_detail, " ")[[1]][2]
    variable_names <- c(variable_names, variable_name)
  }
  # Recursively extract variable names from child nodes
  if ("Left_child_node" %in% names(tree)) {
    variable_names <- c(variable_names, extract_decision_vars(tree$Left_child_node))
  }
  if ("Right_child_node" %in% names(tree)) {
    variable_names <- c(variable_names, extract_decision_vars(tree$Right_child_node))
  }
  return(unique(variable_names))
}


# Function: Prune tree from bottom up based on the EPV value >= 10
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
  # Return the (possibly pruned) node
  return(node)
}


























