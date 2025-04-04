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

# Manually adjust the range from [0,1] to [-1,1]
normalized_data <- (normalized_data * 2) - 1


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

# Training using k-means 
kmeans.result <- kmeans(normalized_data, centers=3)
kmeans.result$centers
kmeans.result$cluster

centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.

distances <- sqrt(rowSums((normalized_data - centers)^2))

outliers <- order(distances, decreasing=T)[1:12]

print(outliers)

print(final_data[outliers, "merged_data.name"])
print(country_names[outliers, ])

columns_used = c("GDP", "Population")
plot(normalized_data[,columns_used], pch=19, col=kmeans.result$cluster, cex=1)
#points(kmeans.result$centers[,columns_used], col=1:3, pch=15, cex=2)
points(normalized_data[outliers, columns_used], pch="+", col=4, cex=3)

# Plot 3D
#install.packages("scatterplot3d")
library("scatterplot3d")
install.packages("plot3D")

columns_used = c("GDP", "Population", "energy_consumption_btu")
s3d <- scatterplot3d(normalized_data[, columns_used], pch=19, color=kmeans.result$cluster
                     , angle=120)
s3d$points3d(normalized_data[outliers, columns_used], pch="+", col=4, cex=3)

# install.packages("cluster")
library("cluster")

silhouette_scores <- silhouette(kmeans.result$cluster, dist(normalized_data))
print(silhouette_scores)

avg_silhouette_score <- mean(silhouette_scores[, 3])
print(avg_silhouette_score)

#ini pca resulted

pca_result <- prcomp(normalized_data, scale. = TRUE, rank. = 5)
summary(pca_result)

pca_components <- pca_result$x[, 1:3]

view(pca_components)
dim(pca_components)

final_data_pca = cbind(country_names, pca_components)
view(final_data_pca)

kmeans.result <- kmeans(final_data_pca[,-1], centers=3)
kmeans.result$centers
kmeans.result$cluster

centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.

distances <- sqrt(rowSums((pca_components - centers)^2))

outliers <- order(distances, decreasing=T)[1:5]

print(outliers)

print(final_data_pca[outliers, "merged_data.name"])

columns_used = c("PC1", "PC2")
plot(pca_components[,columns_used], pch=19, col=kmeans.result$cluster, cex=1)
#points(kmeans.result$centers[,columns_used], col=1:3, pch=15, cex=2)
points(pca_components[outliers, columns_used], pch="", col=4, cex=3)

# Plot 3D
#install.packages("scatterplot3d")
library("scatterplot3d")
#install.packages("plot3D")

columns_used = c("PC1", "PC2", "PC3")
s3d <- scatterplot3d(pca_components[, columns_used], pch=19, color=kmeans.result$cluster
                     , angle=120)
s3d$points3d(pca_components[outliers, columns_used], pch="+", col=4, cex=3)

# install.packages("cluster")
library("cluster")

silhouette_scores <- silhouette(kmeans.result$cluster, dist(normalized_data))
low_silhouette <- which(silhouette_scores[,3] < 0.2)
print(low_silhouette)
print(silhouette_scores)

print(final_data_pca[low_silhouette, "merged_data.name"])

avg_silhouette_score <- mean(silhouette_scores[, 3])
print(avg_silhouette_score)