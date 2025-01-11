data <- read.csv("World_Energy_By_Country_And_Region_1965_to_2023.csv", sep=";")

library(tidyverse)

missing_data <- data %>%
  pivot_longer(
    cols = -Country,      
    names_to = "Year",       
    values_to = "Energy"
  ) %>%
  group_by(Country) %>%
  summarize(
    Total_Years = n(),                       
    Missing_Years = sum(is.na(Energy)),      
    Missing_Percentage = (Missing_Years / Total_Years) * 100
  )

print(missing_data, n=110)

data_long <- data %>%
  pivot_longer(
    cols = -Country,         # Columns to pivot (all except "Country")
    names_to = "Year",       # New column for year
    values_to = "Energy"     # New column for energy values
  ) %>%
  mutate(
    Energy = as.numeric(Energy)  # Convert Energy to numeric
  )

# Check if all Energy values are numeric for each country
numeric_check <- data_long %>%
  group_by(Country) %>%
  summarize(
    Total_Values = n(),                      # Total values for the country
    Non_Numeric_Values = sum(is.na(Energy)), # Count of non-numeric values (NA after conversion)
    Fully_Numeric = all(!is.na(Energy))      # Check if all values are numeric
  )

# View the results
print(numeric_check, n=110)

filtered_countries <- numeric_check %>%
  filter(Non_Numeric_Values > 6)

print(filtered_countries)

# Remove countries in filtered_countries from data
data <- data %>%
  filter(!Country %in% filtered_countries$Country)

for (col in names(data)) {
  if(col == "Country"){
    next
  }
  if (is.character(data[[col]])) {  # Or check other conditions
    # Convert to numeric and replace non-numeric with 0
    data[[col]] <- as.numeric(data[[col]])
    data[[col]][is.na(data[[col]])] <- 0
  }
}

data <- data %>%
  filter(!grepl("Total|OECD|Union", Country))

view(data)

# Check if there are any missing values in the dataset
any_missing <- anyNA(data)
print(paste("Are there any missing values?", any_missing))

# Count the total number of missing values
total_missing <- sum(is.na(data))
print(paste("Total missing values:", total_missing))


library(ggplot2)

# Convert the data to a long format
data_long <- data %>%
  pivot_longer(
    cols = -Country,         # Pivot all columns except "Country"
    names_to = "Year",       # New column for year
    values_to = "Energy"     # New column for energy consumption
  )

# Get the unique list of countries
country_list <- unique(data_long$Country)
print(country_list)
# Loop through the countries in batches of 5
for (i in seq(1, length(country_list), by = 5)) {
  # Subset the data for the current batch of 5 countries
  subset_countries <- country_list[i:min(i + 4, length(country_list))]
  data_subset <- data_long %>%
    filter(Country %in% subset_countries)
  
  # Create the plot for the current subset of countries
  plot <- ggplot(data_subset, aes(x = Year, y = Energy, group = Country, color = Country)) +
    geom_line() +
    labs(
      title = paste("Energy Consumption for Countries", paste(subset_countries, collapse = ", "), "Over Time"),
      x = "Year",
      y = "Energy Consumption (Units)",
      color = "Country"   # This will display a legend showing the countries
    ) +
    theme_minimal() +
    theme(
      legend.position = "right"  # Position of the legend
    )
  
  # Print the plot
  print(plot)
}


library(forecast)

# Get the list of unique countries
countries <- unique(data_long$Country)

# Initialize a list to store results
anomaly_results <- list()

# Loop through each country
for (country in countries) {
  # Filter data for the country
  country_data <- data_long %>%
    filter(Country == country) %>%
    arrange(Year)
  
  # Convert Energy into a time series
  ts_data <- ts(country_data$Energy, start = min(country_data$Year), frequency = 1)
  
  # Fit ARIMA model
  arima_model <- auto.arima(ts_data)
  
  # Calculate residuals
  residuals <- residuals(arima_model)
  
  # Define anomaly threshold (e.g., 2x standard deviation)
  threshold <- 2 * sd(residuals, na.rm = TRUE)
  anomalies <- abs(residuals) > threshold
  
  # Store anomaly information
  anomaly_results[[country]] <- data.frame(
    Year = country_data$Year,
    Energy = country_data$Energy,
    Residuals = residuals,
    Anomaly = anomalies
  )
}

example_country <- "US"
country_anomalies <- anomaly_results[[example_country]]

library(ggplot2)

ggplot(country_anomalies, aes(x = Year, y = Energy)) +
  geom_line(color = "blue") +
  geom_point(data = country_anomalies %>% filter(Anomaly), aes(x = Year, y = Energy), color = "red", size = 3) +
  labs(
    title = paste("Anomaly Detection for", example_country),
    x = "Year",
    y = "Energy Consumption"
  ) +
  theme_minimal()

country_anomalies <- anomaly_results[[Canada]]
