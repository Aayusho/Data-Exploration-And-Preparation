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

# Function to count outliers
count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  sum(x < lower | x > upper, na.rm = TRUE)
}

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
