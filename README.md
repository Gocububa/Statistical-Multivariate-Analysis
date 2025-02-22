# Air Quality and Public Health Impact Analysis
### 1. Exploratory Data Analysis (EDA)
- Initial data exploration, missing values, and outlier detection.
- Correlation matrix visualization for understanding variable relationships.

### 2. Inferences About a Mean Vector
- Hypothesis tests to check if pollutant levels meet safety thresholds.

### 3. Comparison of Multivariate Means (MANOVA)
- Multivariate analysis of variance to assess if pollutant levels (PM10, PM2.5, SO2, NO2, O3) differ across temperature ranges.

### 4. Principal Component Analysis (PCA) & Principal Component Regression
- **PCA:** Used to reduce multicollinearity and extract principal components.
- **Regression:** Built a model using principal components to predict *Health Impact Score*.

### 5. Factor Analysis & Factor Rotation
- Identified latent factors among variables.
- Applied factor rotation for better interpretability.

### 6. Discrimination & Classification (LDA)
- **Linear Discriminant Analysis (LDA)** for classifying **Air Quality Index (AQI)** as "Acceptable" or "Unhealthy".
- Achieved **96% accuracy** in classification.

### 7. Clustering Analysis
- Applied **Hierarchical Clustering** (Agglomerative & Divisive).
- Used **Elbow Method** to determine the optimal number of clusters (**k=4**).

### 8. Canonical Correlation Analysis
- Examined relationships between weather variables (temperature, wind speed, humidity) and pollutant levels.
