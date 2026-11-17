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
