**🏠 Milan Housing Price Prediction – Data Mining Project**
Overview

This project focuses on predicting housing prices in Milan using a dataset of 12,800 real estate listings. The goal is to build an interpretable and effective predictive model leveraging extensive feature engineering, statistical modeling, and machine learning techniques.

The dataset is split into:

8,000 training observations
4,800 test observations

Target variable: selling_price

Data Processing & Feature Engineering

The original dataset contains 16 variables, which were extensively transformed into 61 features through feature engineering and encoding.

Key preprocessing steps include:

Missing value imputation
Regression-based imputation (logistic / ordinal models)
Group-based mean imputation for structured variables
Outlier handling
Correction of unrealistic values (e.g., square meters < 13)
Capping extreme condominium fees
Feature transformations
Log transformation of skewed variables (price, square meters)
Aggregation of categorical levels (energy class, parking, floors)
Dummy variable expansion
Decomposition of other_features into binary indicators
Feature Highlights

Main predictors used in the final model:

Square meters (log-transformed)
Bathrooms and rooms number
Distance from city center
Building characteristics (floor, lift, condition, energy class)
Amenities (parking, balcony, garden, concierge, etc.)
Zone-based clustering (see below)
Spatial Clustering

To capture geographical heterogeneity, Milan zones were clustered using K-Means, based on price per square meter.

Resulting variable: cluster (20 levels)
Central areas and peripheral zones are clearly separated
Helps reduce dimensionality while preserving spatial structure
Modeling Approach

The final predictive model is a linear regression model on log-transformed prices, defined as:

Response: log(selling_price)
Predictors: housing features + interaction terms + zone clusters

Alternative models tested:

Elastic Net
Generalized Additive Model (GAM)

Although GAM slightly outperformed in validation, the linear model was selected due to:

Higher interpretability
Simplicity and stability
Strong economic coherence of coefficients
Model Evaluation
Validation strategy: 75% train / 25% validation split
Metric: Mean Absolute Error (MAE)

Results:

Linear model MAE: ~77,342
GAM MAE: ~76,094
10-fold cross-validation confirmed good generalization performance
Key Insights
Property size is the strongest price driver
Parking availability significantly increases price
Distance from city center negatively impacts value
Spatial clustering captures strong geographical price variation
Log transformation improves linearity and normality assumptions
Conclusion

The final model provides a robust and interpretable solution for housing price prediction in Milan. Despite the availability of more complex models, the linear approach was preferred due to its transparency and strong economic interpretability.

The inclusion of spatial clustering and feature engineering significantly improved predictive performance while maintaining model simplicity.
