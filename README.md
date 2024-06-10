# Deviance-based Logistic Tree (DbLT) Model
Welcome to the official repository for the **Deviance-based Logistic Tree (DbLT) Model**, an innovative approach to classification that synergizes the interpretability of decision trees with the predictive power of logistic regression. This model employs a unique criterion based on deviance, facilitating a robust selection of variables that uncover intricate relationships within your data. It's designed for analysts and data scientists who require a more nuanced view of their classification tasks, beyond what traditional models offer.

## Key Features:
- **Hybrid Modeling:** Combines logistic regression and decision trees for a comprehensive analytical tool.
- **Deviance Criterion:** Utilizes deviance for continuous variables and chi-square tests for categorical variables, optimizing the selection process.
- **Iterative Validation:** Validates each node through logistic regression, ensuring the reliability of the splits.
- **Pruning Techniques:** Implements various pruning methods (AIC, BIC and EPV) to prevent overfitting and enhance model simplicity and performance.
- **Simulation Study:** Demonstrates predictive performance through extensive tests on both simulated data and actual medical datasets.

## This Repository Includes:
- **DbLT_main:**
  - Main code for DbLT model
  - Code for DbLT pruning function
  - Code for DbLT prediction function
  - Code for zero/root node analysis (DbLT_0), there are two sub-functions: 
    - Fit Logistic Regression for entire dataset
    - Fit Simple Logistic Regression for each variable
- **Simulation: Script**
  - The script is used generate three datasets for three simulation studies
- **Simulation: Results**
  - Simulation Study 1 - Table 1: Predictive Performance Comparison of Raw and pruned DbLT models with EPV (10 & 25), AIC and BIC
  - Simulation Study 1 - Table 2: Predictive Performance Comparison between Pruned DbLT, Pruned MOB, Pruned CART, Pruned LMT, and GLM-Logistic
  - Simulation Study 2 - Figure 1: Accuracy and AUC Comparison of Pruned DbLT with EPV = 10 + BIC, Pruned MOB with AIC, Pruned CART, GLM-Logistic and Pruned LMT across Variable Correlation Levels
  - Simulation Study 3 - Table 1: Comparison of Variable Selection Metrics across Pruned Logistic Tree and GLM-Logistic Models
  - Simulation Study 3 - Table 2: Variable Selection Frequency Analysis for Pruned Logistic Tree and GLM-Logistic Models
- **Application:**
  - Application Table 1: Model Predictive Performance Evaluation for Dementia Dataset
  - Application Table 2: Variable Selection Frequency Analysis for Dementia Dataset
- **Example:** Example outputs show the tree plot and tree model summary of DbLT.

DbLT Tree Plot

<img width="452" alt="DbLT_plot_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/4e9bc501-1de9-49ea-8411-9a07c2862b5b">

DbLT Tree Model Summary

<img width="452" alt="DbLT_summary_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/6654ae11-9f0a-4367-973c-42507cd0af11">

Zero Node Analysis Function 1: Fit Logistic regression for the entire dataset



Zero Node Analysis Function 2: Fit Simple Logistic regression for each variables

