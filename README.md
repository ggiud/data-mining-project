# 🏠 Milan Housing Price Prediction – Data Mining Project

## **Overview**

This project focuses on predicting housing prices in Milan using a dataset of **12,800 real estate listings**.

The main objective is to build an accurate and interpretable predictive model through:

* data preprocessing
* feature engineering
* clustering techniques
* statistical modeling
* model validation

The dataset was divided into:

* **8,000 training observations**
* **4,800 test observations**

The target variable is **selling_price**.

---

## **Dataset & Feature Engineering**

The original dataset contained **16 variables**, which were transformed into a richer feature space through preprocessing and feature engineering.

In particular:

* the variable **other_features** was decomposed into several dummy variables
* additional categorical transformations were applied
* the final dataset included more than **60 predictors**

### **Main preprocessing steps**

* Missing value imputation
* Outlier detection and correction
* Dummy variable creation
* Variable aggregation
* Log transformations
* Spatial clustering

---

## **Data Cleaning & Missing Value Imputation**

Several variables required preprocessing and imputation.

### **Examples**

* **square_meters**

  * unrealistic values (<13 sqm) were treated as errors
  * imputed using the average size of similar properties

* **bathrooms_number**

  * missing values imputed using ordinal logistic regression (POLR)

* **year_of_construction**

  * imputed using grouped averages based on:

    * property condition
    * energy class
    * availability

* **condominium_fees**

  * extreme values (>12000) treated as anomalies
  * missing values imputed using grouped averages

* **energy_efficiency_class**

  * classes aggregated into:

    * low
    * medium
    * high

Several additional variables were imputed using:

* logistic regression
* cumulative logistic regression
* grouped statistics

---

## **Feature Highlights**

Main variables used in the final model:

* **square_meters**
* **bathrooms_number**
* **rooms_number**
* **distance from Milan city center**
* **energy efficiency class**
* **parking availability**
* **floor and building characteristics**
* **security level**
* **balcony / terrace**
* **garden**
* **clustered geographical area**

The variable **zone** was not directly included in the model to reduce overfitting.

---

## **Spatial Clustering**

Milan zones were clustered using **K-Means** based on:

* average price per square meter

This produced a new categorical variable:

* **cluster** (20 levels)

The clustering successfully separated:

* central areas
* peripheral areas

while reducing the number of geographical coefficients required by the model.

---

## **Exploratory Data Analysis**

The log-transformed distribution of:

* **selling_price**
* **square_meters**

appeared approximately normal.

A strong linear relationship emerged between:

* log(square_meters)
* log(selling_price)

Additional analyses highlighted:

* strong relationships between price and location
* relevant effects of parking availability
* higher prices for central properties
* correlations between rooms, bathrooms, and house size

---

## **Predictive Modeling**

The final predictive model was a **linear regression model** on log-transformed prices.

### **Model Features**

The model included:

* log(square_meters)
* bathrooms_number
* rooms_number
* condominium_fees
* floor
* lift
* energy_efficiency_class
* parking
* distance
* cluster
* security_level
* additional housing features

Interaction terms were also included.

---

## **Model Comparison**

The linear model was compared with:

* **Elastic Net**
* **Generalized Additive Model (GAM)**

### **Results**

Validation strategy:

* 75% train
* 25% validation

Performance metric:

* **Mean Absolute Error (MAE)**

Results:

* **Linear Model MAE ≈ 77,342**
* **GAM MAE ≈ 76,094**

Although GAM achieved slightly better predictive performance, the linear model was selected due to:

* interpretability
* simplicity
* computational efficiency

A **10-fold cross-validation** confirmed good generalization performance.

---

## **Key Findings**

The most influential variables were:

* property size
* geographical area
* parking availability
* distance from the city center

The introduction of clustered geographical areas significantly reduced prediction error while avoiding overfitting.

Log-transforming the response variable improved:

* normality assumptions
* linearity between predictors and target

---

## **Conclusion**

The final linear model proved to be:

* simple
* interpretable
* computationally efficient
* effective for real estate price prediction

Despite the availability of more complex methods, the linear approach achieved competitive predictive performance while remaining easy to understand and apply.

This makes the model potentially useful even for non-technical users operating in the real estate market.

---

## **Methods & Techniques**

* Data preprocessing
* Missing value imputation
* Feature engineering
* K-Means clustering
* Linear regression
* Elastic Net
* Generalized Additive Models (GAM)
* Cross-validation
* Model evaluation with MAE

---

## **Tech Stack**

* **R**
* **tidyverse**
* **ggplot2**
* **caret**
* **MASS**
* **mgcv**

