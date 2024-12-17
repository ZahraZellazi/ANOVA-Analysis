  # ANOVA Statistical Analysis Project 

This project performs a comprehensive statistical analysis on two datasets: **Gas Turbine Dataset** and **AI4I Dataset**. The focus is on **ANOVA (Analysis of Variance)** to uncover key factors influencing the target variables. Additionally, the project includes exploratory data analysis, visualization, and preprocessing steps to ensure reliable results.

---

## ✨ Features

- 🔄 **Data Preprocessing**:
  - 🛠️ Handling missing values and outliers (capping via IQR).
  - 📏 Standardization of numeric variables.
  - 🧪 Variable transformations (logarithmic, Box-Cox).
  
- 📊 **Exploratory Data Analysis**:
  - 📋 Descriptive statistics and data structure exploration.
  - 📈 Histograms, boxplots, and correlation heatmaps.
  
- 📋 **Statistical Tests**:
  - ✅ T-Tests, Wilcoxon tests, Chi-square tests, and Kruskal-Wallis tests.
  
- 📚 **Modeling and ANOVA**:
  - 🔍 Stepwise regression to select significant predictors.
  - 📐 Residual diagnostics for validation.
  - 📊 ANOVA for comparing multiple models.

---

## 🛠️ Prerequisites

The following R packages are required:

- **tidyverse**
- **ggplot2**
- **dplyr**
- **caret**
- **MASS**
- **reshape2**
- **GGally**
- **robustbase**
- **ggcorrplot**
- **car**
- **lmtest**
- **nortest**
- **moments**
- **boot**
- **conflicted**

---

## 🚀 How to Run

1.📥 Clone the Repository: Clone this project to your local machine using Git:

git clone https://github.com/ZahraZellazi/ANOVA-Analysis.git

2.🖥️ Set Up the Environment: Open the Anova-.r file in RStudio or any R-compatible editor.

3.📂 Load the Datasets: The script will prompt you to load the datasets. Prepare your CSV files for:
        💨 Gas Turbine Dataset (e.g., gt_2011.csv, gt_2012.csv).
        🤖 AI4I Dataset (e.g., ai4i2020.csv). Use the file selection prompt (file.choose()) to select each dataset as requested by the script.

---

## 🏆 Results Overview

💨 Gas Turbine Dataset:

🎯 Target Variable: CO
📝 Key Insights: Identified predictors through stepwise regression and ANOVA.
🤖 AI4I Dataset:

🎯 Target Variable: Machine.failure
📝 Key Insights: Explored failure trends, correlations, and performed regression analysis.

---

## 🤝 Contributions

Contributions are welcome! Fork the repository and submit your pull requests.

