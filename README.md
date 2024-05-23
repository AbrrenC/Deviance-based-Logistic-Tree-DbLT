# Deviance-based Logistic Tree (DbLT) Model

Welcome to the official repository for the **Deviance-based Logistic Tree (DbLT) Model**, an innovative approach to classification that synergizes the interpretability of decision trees with the predictive power of logistic regression. This model employs a unique criterion based on deviance, facilitating a robust selection of variables that uncover intricate relationships within your data. It's designed for analysts and data scientists who require a more nuanced view of their classification tasks, beyond what traditional models offer.

## Key Features:

- **Hybrid Modeling:** Combines logistic regression and decision trees for a comprehensive analytical tool.
- **Deviance Criterion:** Utilizes deviance for continuous variables and chi-square tests for categorical variables, optimizing the selection process.
- **Iterative Validation:** Validates each node through logistic regression, ensuring the reliability of the splits.
- **Pruning Techniques:** Implements various pruning methods (AIC, BIC and EPV) to prevent overfitting and enhance model simplicity and performance.
- **Simulated and Real-World Application:** Demonstrates predictive performance through extensive tests on both simulated data and actual medical datasets.

## This Repository Includes:

- **DbLT_main:** The core DbLT model code, the pruning and prediction code. (R programming language)
- **Simulation Scripts:** Scripts to generate simulated datasets.
- **Simulation Outputs:** All the simulation tables and figures.
- **Example Outputs:** Example ouptus show the tree plot and tree model summary of DbLT.
- **Application Outputs:** All the application outputs on our application dataset.

**DbLT Tree Plot**

<img width="452" alt="DbLT_plot_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/4e9bc501-1de9-49ea-8411-9a07c2862b5b">

**DbLT Tree Model Summary**

<img width="452" alt="DbLT_summary_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/6654ae11-9f0a-4367-973c-42507cd0af11">



