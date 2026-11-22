install.packages("readr")

library(readr)

df <- read_csv("housing.csv")

head(df)
dim(df)
names(df)

#dataset structure
str(df)

#summary stats
summary(df)

#Data cleaning and preparation

#missing values
sum(is.na(df))

#missing values in columns
colSums(is.na(df))

#duplicate rows
sum(duplicated(df))

#infinite values
sum(sapply(df,function(x) {
  if(is.numeric(x)) sum(is.infinite(x)) else 0
}))

#checking categories
table(df$ocean_proximity)

#handling missing values
#the total_Bedrooms columns contains missing values.

#percentage of missing bedrooms
missing_percentage <- sum(is.na(df$total_bedrooms)) / nrow(df) * 100

missing_percentage

#copy for cleaning
df_clean <- df

# Calculating median of bedrooms
median_bedrooms <- median(df_clean$total_bedrooms, na.rm = TRUE)

# Replacing missing values with the median
df_clean$total_bedrooms[is.na(df_clean$total_bedrooms)] <- median_bedrooms

# Checking remaining missing values
sum(is.na(df_clean))

#Dataset summary table
dataset_summary <- data.frame(
  Description = c(
    "Original Rows",
    "Original Columns",
    "Missing Values Before Cleaning",
    "Missing Values After Cleaning",
    "Cleaned Rows",
    "Cleaned Columns"
  ),
  Value = c(
    nrow(df),
    ncol(df),
    sum(is.na(df)),
    sum(is.na(df_clean)),
    nrow(df_clean),
    ncol(df_clean)
  )
)

print(dataset_summary)

# Checking dataset dimensions
dim(df_clean)

# Displaying summary of cleaned bedrooms
summary(df_clean$total_bedrooms)

# Comparing original and cleaned bedroom values
summary(df$total_bedrooms)

summary(df_clean$total_bedrooms)

#Outlier Detection

# Boxplot for median house value
boxplot(df_clean$median_house_value,
        main = "Distribution of House Prices",
        ylab = "House Value ($)",
        col = "lightblue")

# Boxplot for median income
boxplot(df_clean$median_income,
        main = "Distribution of Median Income",
        ylab = "Median Income",
        col = "lightgreen")

# Boxplot for population
boxplot(df_clean$population,
        main = "Distribution of Population",
        ylab = "Population",
        col = "lightpink")

# Function to count outliers using IQR
count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  sum(x < lower | x > upper)
}

# Counting outliers in selected columns
count_outliers(df_clean$median_house_value)
count_outliers(df_clean$median_income)
count_outliers(df_clean$population)


# Creating outlier results table
outlier_results <- data.frame(
  Variable = c("House Value", "Median Income", "Population"),
  Outliers = c(
    count_outliers(df_clean$median_house_value),
    count_outliers(df_clean$median_income),
    count_outliers(df_clean$population)
  )
)

# Displaying results in Console
print(outlier_results)

# Opening table in Data Viewer
View(outlier_results)

#Outlier Treatment

# Retaining outliers as they may represent genuine values
# No outlier removal is performed

# Checking cleaned dataset dimensions
dim(df_clean)

# Checking remaining missing values
sum(is.na(df_clean))

#Exploratory Data analysis
library(ggplot2)

#distribution of house prices - histogram
#This histogram shows the distribution of median house values.
#most housing districts have values in the lower and middle ranges.
#the concentration near $500000 reflects the datasets price cap.
ggplot(df_clean, aes(x = median_house_value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of California House Prices",
    x = "Median House Value ($)",
    y = "Frequency"
  ) +
  theme_minimal()

#median income vs house prices - scatter plot
#This scatter plot compares median income with median house value.
#It shows a generally positive relationship between income and house prices with also some variation present.
#Higher-income districts tend to have higher house values.
ggplot(df_clean, aes(x = median_income,
                     y = median_house_value)) +
  geom_point(alpha = 0.3, color = "blue") +
  labs(
    title = "Median Income vs House Prices",
    x = "Median Income (Tens of Thousands of Dollars)",
    y = "Median House Value ($)"
  ) +
  theme_minimal()

#House Prices by Ocean Proximity - boxplot
#This boxplot compares house values across ocean proximity categories.
#It helps identify differences in median prices and price variation.
#it also highlights potential outliers within each category.
ggplot(df_clean, aes(x = ocean_proximity,
                     y = median_house_value)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "House Prices by Ocean Proximity",
    x = "Ocean Proximity",
    y = "Median House Value ($)"
  ) +
  theme_minimal()

#Correlation Heatmap

# Selecting numerical columns
numeric_data <- df_clean[sapply(df_clean, is.numeric)]

# Calculating correlation matrix
cor_matrix <- cor(numeric_data)

# Converting matrix into a data frame
cor_data <- as.data.frame(as.table(cor_matrix))

# Creating heatmap
#this heatmap shows correlations between numerical variables.
#it helps identify variables that have strong positive or negative relationships.
#these relationships are useful when interpreting the PCA results.
ggplot(cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "Correlation Heatmap of Housing Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Categorical Encoding
# Displaying ocean proximity categories
table(df_clean$ocean_proximity)

#Converting ocean proximity into dummy variables
ocean_encoded <- model.matrix(
  ~ ocean_proximity - 1,
  data = df_clean
)

#Viewing the first six rows
head(ocean_encoded)

#Removing original categorical column
df_numeric <- df_clean[, names(df_clean) != "ocean_proximity"]

#Adding encoded columns
df_encoded <- cbind(df_numeric, ocean_encoded)

dim(df_encoded)
names(df_encoded)

#Checking remaining missing values
sum(is.na(df_encoded))

#data scaling and PCA
#Standardising numerical variables
df_scaled <- scale(df_encoded)


head(df_scaled)
dim(df_scaled)

#Performing Principal Component Analysis
pca_result <- prcomp(df_scaled, center = FALSE, scale. = FALSE)

summary(pca_result)

#PCA Scree Plot

#Calculating explained variance
variance <- pca_result$sdev^2

variance_percentage <- variance / sum(variance) * 100

#Creating scree plot
#this scree plot shows the percentage of variance explained by each principal component.
#the first six components explain approx. 86.98% of the total variance.
plot(
  variance_percentage,
  type = "b",
  pch = 19,
  col = "blue",
  main = "PCA Scree Plot",
  xlab = "Principal Components",
  ylab = "Variance Explained (%)"
)

#PCA Scatter Plot
#this plot displays the housing observations using PC1 and PC2.
#PC1 mainly represents housing and population size.
#PC2 mainly represnets geographical location.
#The plot helps visualise patterns in the reduced dataset.
plot(
  pca_result$x[, 1],
  pca_result$x[, 2],
  col = "blue",
  pch = 16,
  cex = 0.3,
  main = "PCA: First Two Principal Components",
  xlab = "PC1",
  ylab = "PC2"
)

#Calculating cumulative explained variance
cumulative_variance <- cumsum(variance_percentage)

print(cumulative_variance)
which(cumulative_variance >= 80)[1]

#PCA Interpretation
#Displaying PCA variable loadings
print(round(pca_result$rotation[, 1:3], 3))

#Identifying the strongest contributors to PC1
pc1_loadings <- sort(
  abs(pca_result$rotation[, 1]),
  decreasing = TRUE
)

print(head(pc1_loadings, 5))

#Identifying the strongest contributors to PC2
pc2_loadings <- sort(
  abs(pca_result$rotation[, 2]),
  decreasing = TRUE
)

print(head(pc2_loadings, 5))

#Final PCA Summary
# Number of components explaining at least 80% variance
components_needed <- which(cumulative_variance >= 80)[1]

#Total variance retained
variance_retained <- cumulative_variance[components_needed]

#PCA Explained Variance Summary Table
pca_summary <- data.frame(
  Component = paste0("PC", seq_along(variance_percentage)),
  Variance_Explained = round(variance_percentage, 2),
  Cumulative_Variance = round(cumulative_variance, 2)
)

print(pca_summary)

#Displaying final results
cat("Components retained:", components_needed, "\n")
cat("Variance retained:", round(variance_retained, 2), "%\n")

#Creating reduced PCA dataset
df_pca <- as.data.frame(
  pca_result$x[, 1:components_needed]
)

#Checking reduced dataset dimensions
dim(df_pca)

#Final PCA conclusion
#The original encoded dataset contained 14 numerical variables.
#PCA reduced these to six principal components.
#these six components retained approx. 86.98% of the total variance.
#PC1 was mainly influenced by households,total bedrooms,total rooms and population.
#PC2 was mainly influenced by latitude,longitude and ocean proximity.
#therefore PCA reduced the number of variables while preserving most of the information in the dataset.

