# Inserting two datasets
datasetone <- read.csv("energy1.csv", sep=",")
View(datasetone)
unique_name_one <- unique(datasetone$name)
print(paste("Total Countries of the First Dataset:", length(unique_name_one)))

# print("Total Countries of the First Dataset: ", length(unique_name_one))
colnames_one <- colnames(datasetone)
print("Column Names and Corresponding Data Types:")

for (colname in colnames_one) {
  print(paste(colname, ":", class(datasetone[[colname]])))
}

datasettwo <- read.csv("energy2.csv", sep=",")
unique_name_two <- unique(datasettwo$Country)
unique_year_two <- unique(datasettwo$Year)
print(paste("Total Countries of the Second Dataset:", length(unique_name_one)))
print(unique_year_two)
View(datasettwo)
colnames_two <- colnames(datasettwo)
print("Column Names and Corresponding Data Types:")

for (colname in colnames_two) {
  print(paste(colname, ":", class(datasettwo[[colname]])))
}

library(dplyr)

# Filtering to only year 2016
datasettwo <- datasettwo %>%
  filter(Year==2016)

View(datasettwo)

# Integrating by country
# Looking for the same countries in both dataset
unique_values_one <- unique(datasetone$name)
print(unique_values_one)

unique_values_second <- unique(datasettwo$Country)
print(unique_values_second)

difference_one <- setdiff(unique_values_one, unique_values_second)

difference_two <- setdiff(unique_values_second, unique_values_one)

if (length(difference_one) > 0) {
  print("Values in datasetone but not in datasetsecond:")
  print(difference_one)
}

if (length(difference_two) > 0) {
  print("Values in datasetsecond but not in datasetone:")
  print(difference_two)
}

# Filtering the dataset with the same countries
datasetone_filtered <- datasetone %>%
  filter(name %in% unique_values_second)

datasettwo_filtered <- datasettwo %>%
  filter(Country %in% unique_values_one)

View(datasetone_filtered)
View(datasettwo_filtered)

unique_values_one <- unique(datasetone_filtered$name)
print(unique_values_one)

unique_values_second <- unique(datasettwo_filtered$Country)
print(unique_values_second)

difference_one <- setdiff(unique_values_one, unique_values_second)

difference_two <- setdiff(unique_values_second, unique_values_one)

if (length(difference_one) > 0) {
  print("Values in datasetone but not in datasetsecond:")
  print(difference_one)
}

if (length(difference_two) > 0) {
  print("Values in datasetsecond but not in datasetone:")
  print(difference_two)
}

View(datasettwo_filtered)

library(tidyverse)

print(colnames(datasettwo_filtered))

# Group by Country and Year, then summarize all energy types into individual columns
reshaped_data <- datasettwo_filtered %>%
  group_by(Country, Year) %>%
  summarize(
    GDP = first(GDP),
    Population = first(Population),
    Energy_intensity_per_capita = first(Energy_intensity_per_capita),
    Energy_intensity_by_GDP = first(Energy_intensity_by_GDP),
    
    Energy_consumption_coal = sum(Energy_consumption[Energy_type == "coal"], na.rm = TRUE),
    Energy_consumption_natural_gas = sum(Energy_consumption[Energy_type == "natural_gas"], na.rm = TRUE),
    Energy_consumption_petroleum = sum(Energy_consumption[Energy_type == "petroleum_n_other_liquids"], na.rm = TRUE),
    Energy_consumption_nuclear = sum(Energy_consumption[Energy_type == "nuclear"], na.rm = TRUE),
    Energy_consumption_renewables = sum(Energy_consumption[Energy_type == "renewables_n_others"], na.rm = TRUE),
    
    Energy_production_coal = sum(Energy_production[Energy_type == "coal"], na.rm = TRUE),
    Energy_production_natural_gas = sum(Energy_production[Energy_type == "natural_gas"], na.rm = TRUE),
    Energy_production_petroleum = sum(Energy_production[Energy_type == "petroleum_n_other_liquids"], na.rm = TRUE),
    Energy_production_nuclear = sum(Energy_consumption[Energy_production == "nuclear"], na.rm = TRUE),
    Energy_production_renewables = sum(Energy_consumption[Energy_production == "renewables_n_others"], na.rm = TRUE),
    
    CO2_emission_coal = sum(CO2_emission[Energy_type == "col"], na.rm = TRUE),
    CO2_emission_natural_gas = sum(CO2_emission[Energy_type == "natural_gas"], na.rm = TRUE),
    CO2_emission_petroleum = sum(CO2_emission[Energy_type == "petroleum_n_others_liquids"], na.rm = TRUE),
    CO2_emission_nuclear = sum(CO2_emission[Energy_type == "nuclear"], na.rm = TRUE),
    CO2_emission_renewables = sum(CO2_emission[Energy_type == "renewables_n_others"], na.rm = TRUE),
  ) %>%
  ungroup()

# View the reshaped data
View(reshaped_data)

library(dplyr)

# Merge the datasets based on common columns (Country and Year)
merged_data <- left_join(datasetone_filtered, reshaped_data, by = c("name" = "Country"))

# View the merged dataset
View(merged_data)
summary(merged_data)
ncol(merged_data)
# liat NA values for each of the column
total_rows <- nrow(merged_data)
null_counts <- sapply(merged_data, function(col) sum(is.na(col)))
null_percentage <- (null_counts / total_rows) * 100

null_summary <- data.frame(
  Column = names(null_counts),
  NullCount = null_counts,
  TotalRows = total_rows,
  NullPercentage = round(null_percentage, 2)
)
print(null_summary)
View(null_summary)

# Preprocessing
dim(merged_data)

# Removing column with a lot of missing values and zeroes
merged_data$coal_net_imports <- NULL
merged_data$coal_net_exports <- NULL
merged_data$gas_net_imports <- NULL
merged_data$gas_net_exports <- NULL
merged_data$oil_net_imports <- NULL
merged_data$oil_net_exports <- NULL
merged_data$Energy_consumption_renewables <- NULL
merged_data$Energy_production_nuclear <- NULL
merged_data$coal_year <- NULL
merged_data$coal_units <- NULL
merged_data$gas_year <- NULL
merged_data$gas_units <- NULL
merged_data$oil_year <- NULL
merged_data$oil_units <- NULL
merged_data$Energy_production_renewables <- NULL
merged_data$CO2_emission_coal <- NULL
merged_data$CO2_emission_petroleum <- NULL
merged_data$CO2_emission_nuclear <- NULL
merged_data$CO2_emission_renewables <- NULL
merged_data$Year <- NULL

merged_data <- na.omit(merged_data)

merged_data <- merged_data %>%
  mutate(across(everything(), ~ifelse(. == 0, mean(., na.rm = TRUE), .)))

# boxplot(merged_data$energy_consumption_btu)

summary(merged_data)

# Splitting country names and other data to be normalized
country_names <- data.frame(merged_data$name)
merged_data <- subset(merged_data, select = -name)

# Change percentage to decimal
merged_data <- as.data.frame(apply(merged_data,2, function(x){
  as.numeric(sub("%", "", x, fixed=TRUE))/100
}))

# Normalize the log-transformed data
library(caret)
# Perform Min-Max normalization (0 to 1)
minMax <- preProcess(merged_data, method = c("range"))
normalized_data <- predict(minMax, merged_data)




# View normalized data
View(normalized_data)

# Correlation Matrix
library(corrplot)
correlation_matrix <- cor(normalized_data)
corrplot(correlation_matrix, order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

# After EDA, we can remove columns with weak correlation with others
normalized_data$re_nuclear <- NULL
normalized_data$oil_consump <- NULL
normalized_data$gas_consump <- NULL
normalized_data$coal_consump <- NULL
normalized_data$co2_emiss_one_year_change <- NULL
normalized_data$coal_exports <- NULL
normalized_data$non_renewable <- NULL
normalized_data$Energy_intensity_by_GDP <- NULL
normalized_data$co2_emiss_per_capita <- NULL
normalized_data$pc_yearly_btu <- NULL
normalized_data$Energy_intensity_per_capita <- NULL
normalized_data$oil_reserves <- NULL
# We realized there are similar meaning columns because of integrating
# two different dataset (redundant data)
normalized_data$population_2016 <- NULL
normalized_data$oil_imports <- NULL
normalized_data$Energy_consumption_petroleum <- NULL
normalized_data$co2_emissions_tons_2016 <- NULL
normalized_data$country_share_of_world_co2 <- NULL
normalized_data$world_share <- NULL
normalized_data$Energy_consumption_coal <- NULL
normalized_data$Energy_production_coal <- NULL
normalized_data$Energy_consumption_natural_gas <- NULL
normalized_data$Energy_production_natural_gas <- NULL
normalized_data$CO2_emission_natural_gas <- NULL

# We realized there are similar meaning columns because of integrating
# two different dataset


correlation_matrix <- cor(normalized_data)
corrplot(correlation_matrix, order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

#bukan pca

# Merging country names with the data
final_data = cbind(country_names, normalized_data)

View(final_data)
library(isotree)

# Training using k-means (non-pca)

set.seed(123)  # For reproducibility
isolation_model <- isolation.forest(normalized_data, ndim = 3, ntrees = 100)

# Generate anomaly scores
anomaly_scores <- predict(isolation_model, normalized_data)

# Define the outlier threshold (anomaly score > 0.5)
threshold <- 0.5
outliers_if <- which(anomaly_scores > threshold)

# Print the outliers
cat("Outliers Detected by Isolation Forest (Anomaly Score > 0.5):\n")
print(final_data[outliers_if, "merged_data.name"])

# ✅ Evaluate the Model Using Average Anomaly Score
mean_anomaly_score <- mean(anomaly_scores)
cat("Average Anomaly Score:", mean_anomaly_score, "\n")

# Plot the histogram of anomaly scores with outliers and country names
hist(anomaly_scores, breaks = 50, col = "gray", main = "Anomaly Score Distribution", 
     xlab = "Anomaly Score", border = "white")

# Draw a red threshold line for anomaly score > 0.5
abline(v = threshold, col = "red", lwd = 2, lty = 2)

# Identify outliers based on the threshold
outliers_if <- which(anomaly_scores > threshold)

# Add red points for the outliers
points(anomaly_scores[outliers_if], rep(5, length(outliers_if)), col = "blue", pch = 19)

# Adding country names next to the outliers with better visibility adjustment
text(anomaly_scores[outliers_if], 
     rep(5.5, length(outliers_if)),  # Move slightly above the points for better visibility
     labels = final_data[outliers_if, "name"], 
     col = "blue", 
     pos = 4,  
     cex = 0.8, 
     offset = 0.5)

# Adding a legend to explain the plot with outliers included
legend("topright", 
       legend = c("Threshold (0.5)", "Outlier"), 
       col = c("red", "blue"), 
       lwd = c(2, NA), 
       pch = c(NA, 19), 
       pt.cex = c(NA, 1.5), 
       cex = 0.9, 
       bty = "n")

#ini pca resulted

pca_result <- prcomp(normalized_data, scale. = TRUE, rank. = 5)
summary(pca_result)

pca_components <- pca_result$x[, 1:3]

view(pca_components)
dim(pca_components)

final_data_pca = cbind(country_names, pca_components)
view(final_data_pca)

library(isotree)

isolation_model <- isolation.forest(pca_components, ndim = 3, ntrees = 100)

# Generate anomaly scores
anomaly_scores <- predict(isolation_model, pca_components)

# Define the outlier threshold (anomaly score > 0.5)
threshold <- 0.5
outliers_if <- which(anomaly_scores > threshold)

# Print the outliers
cat("Outliers Detected by Isolation Forest (Anomaly Score > 0.5):\n")
print(final_data_pca[outliers_if, "merged_data.name"])

# ✅ Evaluate the Model Using Average Anomaly Score
mean_anomaly_score <- mean(anomaly_scores)
cat("Average Anomaly Score:", mean_anomaly_score, "\n")

# Plot the histogram of anomaly scores with outliers in the legend
hist(anomaly_scores, breaks = 50, col = "gray", main = "Anomaly Score Distribution", 
     xlab = "Anomaly Score", border = "white")

# Draw a red threshold line for anomaly score > 0.5
abline(v = threshold, col = "red", lwd = 2, lty = 2)

# Identify outliers based on the new threshold
outliers_if <- which(anomaly_scores > threshold)

# Add red points for the outliers
points(anomaly_scores[outliers_if], rep(5, length(outliers_if)), col = "blue", pch = 19)

# Adding country names next to the outliers with better visibility adjustment
text(anomaly_scores[outliers_if], 
     rep(5.5, length(outliers_if)),  # Move slightly above the points for better visibility
     labels = final_data_pca[outliers_if, "name"], 
     col = "blue", 
     pos = 4,  
     cex = 0.8, 
     offset = 0.5)

# Adding a legend to explain the plot with outliers included
legend("topright", 
       legend = c("Threshold (0.5)", "Outlier"), 
       col = c("red", "blue"), 
       lwd = c(2, NA), 
       pch = c(NA, 19), 
       pt.cex = c(NA, 1.5), 
       cex = 0.9, 
       bty = "n")
