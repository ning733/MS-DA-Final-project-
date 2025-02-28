# MS Data Analytics Final Project

## Project Overview
This project follows a systematic pipeline comprising data exploration, dimensionality reduction, and model development. The objective is to clean, explore, and model data using various machine learning techniques to predict outcomes and evaluate model performance.

## Dataset 1
- **Name**: Taxi-cancellation-case
- **Source**: [Kaggle - Taxi Cancellation Case](https://www.kaggle.com/datasets/ahmedalyazuri/taxi-cancellation-case)
- **Description**: The dataset contains information about taxi ride cancellations. It includes features like pickup and dropoff locations, timestamps, weather conditions, and other ride-related data. The aim is to predict whether a taxi ride will be canceled based on these features.

## Dataset 2
- **Name**: Mortgage Payback
- **Source**: [International Financial Research] https://www.internationalfinancialresearch.org)
- **Description**: The mortgage dataset reports origination and performance observations for 50,000 U.S. residential mortgage borrowers over 60 periods. It is a randomized selection of mortgage-loan-level data collected from the portfolios underlying U.S. residential mortgage-backed securities (RMBS) securitization portfolios, provided by International Financial Research.

**Note**: The Dataset 2 source link has become outdated. It may be useful for historical context, but newer versions of the dataset may be available from alternative sources or through direct contact with the data provider.

## Dataset 2
- **Name**: Human Resource Dataset
- **Source**: [Kaggle - Human Resource Analytics](https://www.kaggle.com/datasets/colara/human-resource)
- **Description**: The human resource dataset includes data about employees' details and their attrition status in a company. It contains features like employee satisfaction, job involvement, work-life balance, and more. The dataset is used for predicting employee attrition and analyzing the factors contributing to it.

## Code Structure
The project is organized into the following folders:
- `data/` → Contains raw datasets in a zipped folder.
- `scripts/` → Contains R scripts for data cleaning, exploration, and modeling.
- `notebooks/` → Empty.
- `reports/` → Contains generated reports, visualizations, and insights.

## Methodology
1. **Data Processing & Exploration**: 
   - Cleaned and reshaped data using the `tidyr` package.
   - Explored data patterns and visualized insights with `ggplot2`.

2. **Model Development**:
   - Developed and evaluated models including:
     - Generalized Linear Models (GLM)
     - k-Nearest Neighbors (KNN)
     - Decision Tree
     - Random Forest
   - Used the `caret` package for model training and evaluation.

3. **Dimensionality Reduction**:
   - Applied Principal Component Analysis (PCA) for feature engineering and dimensionality reduction to improve model accuracy.

## How to Run
To replicate the analysis on your local machine:
1. Install the necessary R packages:
   ```R
   install.packages(c("caret", "ggplot2", "tidyr", "randomForest", "e1071"))
