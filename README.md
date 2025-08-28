# GPU Rendering Performance Analysis Using Statistical Methods

This repository contains the final report for the group project in the Probability and Statistics course (MT2013) at Ho Chi Minh City University of Technology (HCMUT), Faculty of Computer Science and Engineering. The project explores the relationship between GPU technical specifications and rendering performance, focusing on predicting the Pixel Rate metric through advanced statistical modeling.

## Project Overview

In this project, we analyze a dataset of over 3,400 GPU models to understand how key technical parameters influence rendering efficiency. Using statistical techniques such as multiple linear regression, correlation analysis, ANOVA, and cross-validation, we build predictive models to forecast GPU performance. Key highlights include:

- **Data Preprocessing**: Handling missing values, log transformations, and duplicate removal.
- **Exploratory Data Analysis (EDA)**: Visualizations like histograms, boxplots, scatter plots, and Pearson correlation matrices.
- **Inferential Statistics**: Hypothesis testing (Shapiro-Wilk, Levene), Kruskal-Wallis tests with Dunn post-hoc, and multicollinearity checks via VIF.
- **Modeling**: Constructing multiple linear regression models segmented by manufacturers (NVIDIA, AMD, Intel) and memory types (GDDR, DDR).
- **Evaluation**: Metrics like MAE, MSE, RMSE, R², along with prediction intervals and scenario-based forecasting.
- **Insights**: Identifying core influencers like Core Speed, ROPs, and Memory Speed on Pixel Rate, with practical applications for GPU design and selection.

This work demonstrates the application of probability and statistics in computer hardware analysis, providing actionable insights for engineers and researchers.

## File Descriptions

### BTL_XSTK.pdf
The complete project report (53 pages) in Vietnamese, including introduction, methodology, data preprocessing, descriptive statistics, inferential analysis, model building, evaluation, and conclusions. It features detailed visualizations, statistical results, and discussions on limitations and future work.

### All_GPUs.csv
The original dataset used for analysis (not included in this repository). Download it from [Kaggle](https://www.kaggle.com/datasets/iliassekkaf/computerparts) to replicate our work.

*Note*: The R script used for data analysis (`Code_BTL.R`) is included in the repository and documented below.

## Requirements

No specific software requirements for viewing the report—just a PDF reader like Adobe Acrobat or any web browser.

To replicate the analysis:
- **R** (version 4.x or higher recommended).
- Install the required R packages by running the following command in R:
  ```R
  install.packages(c("stringr", "tidyr", "dplyr", "zoo", "Metrics", "caret", "MASS", "ggplot2", "reshape2", "mltools", "DescTools", "plotly", "car", "effectsize", "boot", "patchwork", "rstatix", "PMwR", "FSA"))

## Usage

1. Clone the repository: git clone https://github.com/NghiaTran-Billy/Probability-and-Statistics-Assignment.git
2. Open `BTL_XSTK.pdf` to read the full report.
3. For hands-on exploration:
- Download the dataset from [Kaggle]((https://www.kaggle.com/datasets/iliassekkaf/computerparts)).
- Run any accompanying scripts (to be added) for statistical computations and visualizations.

Feel free to fork and contribute improvements, such as translating the report to English or adding interactive Jupyter notebooks!

## Authors

Group 9, Class DL04, Semester 243

- **Vương Nhật Minh** (MSSV: 2212094)
- **Trần Trung Nghĩa** (MSSV: 2412278, GitHub: [NghiaTran-Billy](https://github.com/NghiaTran-Billy))

**Supervisor**: Ths. Huỳnh Thái Duy Phương  
**Institution**: Ho Chi Minh City University of Technology (HCMUT), Faculty of Computer Science and Engineering  
**Course**: Probability and Statistics (MT2013), Semester 243  
**Completion Date**: August 15, 2025
