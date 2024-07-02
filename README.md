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
    - Zero Node Analysis Function 1: Fit Logistic Regression for entire dataset
    - Zero Node Analysis Function 2: Fit Simple Logistic Regression for each variable
- **Simulation: Script**
  - The script is used generate three datasets for three simulation studies
- **Simulation: Results**
  - Simulation Study 1 - Table 1: Predictive Performance Comparison of Raw and pruned DbLT models with EPV (10 & 25), AIC and BIC
  - Simulation Study 1 - Table 2: Predictive Performance Comparison between Pruned DbLT, Pruned MOB, Pruned CART, Pruned LMT, and GLM-Logistic
  - Simulation Study 1 - DbLT Tree Plot
  
  <img width="452" alt="DbLT_Sim_1" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/6c6ddb48-7b55-4d0b-909d-6747b5b2b7f6">

  - Simulation Study 2 - Table 1: Comparison of Variable Selection Metrics across Pruned Logistic Tree and GLM-Logistic Models
  - Simulation Study 2 - Table 2: Variable Selection Frequency Analysis for Pruned Logistic Tree and GLM-Logistic Models
  - Simulation Study 2 - DbLT Tree Plot

  <img width="452" alt="DbLT_Sim_2" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/b344dc32-ba7f-4158-b7a4-c7d02448e508">

- **Application:**
  - Application with variable mmse - Table 1: Model Predictive Performance Evaluation for Dementia Dataset
  - Application with variable mmse - Table 2: Variable Selection Frequency Analysis for Dementia Dataset
  - Application with variable mmse - DbLT Tree Plot
 
  <img width="452" alt="DbLT_OASIS3_withmmse" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/2170db25-b663-4c12-b0ac-aecb534d9f99">

  - Application without variable mmse - Table 1: Model Predictive Performance Evaluation for Dementia Dataset
  - Application without variable mmse - Table 2: Variable Selection Frequency Analysis for Dementia Dataset
  - Application without variable mmse - DbLT Tree Plot

  <img width="452" alt="DbLT_OASIS3_withoutmmse" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/fe1a3716-2de9-4034-b5e6-a7b024ab1690">

- **Example:** Example outputs show the tree plot and tree model summary of DbLT.
  - DbLT Tree Plot

<img width="452" alt="DbLT_plot_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/0daae52d-d6fa-483a-9131-0f6b3ed60810">

  - DbLT Tree Model Summary

<img width="452" alt="DbLT_summary_output" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/28b2b73e-23bb-4c0c-a120-0730a9105553">

  - Zero Node Analysis Function 1: Fit Logistic Regression for the entire dataset

<img width="452" alt="DbLT_0 Function 1" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/396784a5-b36a-4377-9f79-9b32d0e54065">

  - Zero Node Analysis Function 2: Fit Simple Logistic Regression for each variable

<img width="452" alt="DbLT_0 Function 2" src="https://github.com/AbrrenC/Deviance-based-Logistic-Tree-DbLT/assets/54808990/3d480a86-972d-414f-a6d0-7b20c72024d5">



