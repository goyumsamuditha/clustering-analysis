# load libraries
library(readr)
library(dplyr)
library(readxl)

# read in data
excel_path <- "data/raw/Wine data.xlsx"

red_wine <- read_excel(excel_path, sheet = "Red Wine") %>% mutate(wine_type = "red")
white_wine <- read_excel(excel_path, sheet = "White Wine") %>% mutate(wine_type = "white")
# check data
head(red_wine)
head(white_wine)

# combine red and white wine data
wine_data <- bind_rows(red_wine, white_wine)

# check combined data
head(wine_data)

# Data intergrity check
cat("Initial dataset dimensions:", dim(wine_data), "\n")
cat("Missing values found:", sum(is.na(wine_data)), "\n")
cat("Duplicate rows found:", sum(duplicated(wine_data)), "\n")

# executive summary of the data
summary(wine_data)

# check for missing values
sum(is.na(wine_data))

# check for duplicates
sum(duplicated(wine_data))
sum(duplicated(white_wine))


# remove duplicates if any
wine_data <- wine_data %>% distinct()
white_wine <- white_wine %>% distinct()

# clean column names
colnames(wine_data) <- gsub(" ", "_", colnames(wine_data))
colnames(white_wine) <- gsub(" ", "_", colnames(white_wine))

# check for outliers using boxplots
png(filename = "output/figures/boxplots.png", width = 900, height = 800)
par(mfrow = c(2, 2))
boxplot(wine_data$alcohol, main = "Alcohol Content")
boxplot(wine_data$quality, main = "Wine Quality")
boxplot(wine_data$pH, main = "pH Levels")
boxplot(wine_data$volatile_acidity, main = "Volatile Acidity")
dev.off()

message("Boxplots have been saved to output/figures/boxplots.png")

# save cleaned data
write_csv(wine_data, "data/processed/wine_data.csv")
write_csv(white_wine, "data/processed/white_wine_data.csv")

message("Data preprocessing completed successfully. Cleaned data saved to 'data/processed/' directory.")