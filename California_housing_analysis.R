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
