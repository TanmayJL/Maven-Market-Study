# Maven Market Predictive Analysis

## Overview
**Business objective:** Identify **high‑spending** versus **low‑spending** customers for the Washington branch to inform targeting, marketing, and operational decisions.  
**Scope:** Analysis limited to customers located in **Washington State** using the Maven Market relational database.

## Dataset
The Maven Market database contains six relational tables: **Customers**, **Products**, **Stores**, **Regions**, **Transactions**, and **Returns**. This repository includes the extracted subset and joins used to build customer‑level features and monthly expenditure summaries for Washington.

## Data Preparation and Exploratory Data Analysis
**Key preprocessing steps**
- **Missing expenditures:** Replace `NA` monthly expenditures with **0** (no transaction that month).  
- **Income:** Convert income intervals to numeric using **midpoint** values.  
- **Age:** Compute age using `lubridate` as of **1999-01-01**.  
- **Outliers:** Remove customers with average monthly spending **< $20**.  
- **Store accessibility:** Label customers with no local store as **"No Store"**.

**EDA highlights**
- **Seasonality:** Monthly expenditure shows clear seasonal variation.  
- **Store type effect:** Supermarkets show higher average spending than small groceries.  
- **City-level differences:** Customer city is the strongest predictor of spending.  
- **Demographics:** Income, gender, marital status, and number of children show weak relationships with spending in this dataset.

## Modeling and Results
**Problem framing:** Binary classification (`highOrLow`) — high‑spending vs low‑spending customers.  
**Train/validation split:** 2/3 training, 1/3 validation.

**Models evaluated**
- **CART (Decision Tree)**  
- **Bagging**  
- **Random Forest**

**Performance summary (validation)**
- **Accuracy:** ~80% (error rate ~20%)  
- **CART:** Sensitivity ~53%, Specificity ~95%  
- **Bagging / Random Forest:** Sensitivity ~59%, Specificity ~91%

**Key insight:** **Customer city** dominates predictive power; other available features add little. CART was chosen as the final model for interpretability and comparable performance.

## Technical Setup

### System requirements
- **R** >= 4.0  
- **SQLite** (if you want to re-run extraction from the original database)  
- Recommended: use an R project (`.Rproj`) or `renv` to isolate package versions.

### Required R packages
```r
install.packages(c(
  "tidyverse",
  "DBI",
  "RSQLite",
  "lubridate",
  "janitor",
  "rpart",
  "rpart.plot",
  "randomForest",
  "caret",
  "ranger",
  "data.table",
  "knitr",
  "kableExtra"
))
