
# Load required libraries
library(data.table)



# Convert Date column to Date type if it's not already
sales_data[, Date := as.Date(Date)]

# Filter data up to 03-04-2024
sales_data <- sales_data[Date <= as.Date("2024-03-04")]

# Calculate days since last purchase for each customer-product combination
sales_data[, LastPurchaseDate := max(Date), by = .(Customer, Product)]
sales_data[, DaysSinceLastPurchase := as.numeric(as.Date("2024-04-09") - LastPurchaseDate)]

# Calculate purchase frequency for each customer-product combination
sales_data[, PurchaseFrequency := .N / as.numeric(max(Date) - min(Date)), by = .(Customer, Product)]

# Predict likelihood of purchase on 04-09-2024
sales_data[, PurchaseLikelihood := 1 - exp(-PurchaseFrequency * DaysSinceLastPurchase)]

# Select top N products for each customer based on purchase likelihood
N <- 5  # Adjust this value as needed
predictions <- sales_data[, .SD[order(-PurchaseLikelihood)][1:N], by = Customer]

# Select relevant columns for the final output
final_predictions <- predictions[, .(Customer, Product, PurchaseLikelihood)]

# Sort the predictions by Customer and PurchaseLikelihood
setorder(final_predictions, Customer, -PurchaseLikelihood)

# Write the predictions to a CSV file
fwrite(final_predictions, "predicted_purchases_2024-04-09.csv")

# Print the first few rows of the predictions
print(head(final_predictions))
















# Load required libraries
library(data.table)

# Read the historical sales data
sales_data <- fread("path/to/your/sales_data.csv")

# Convert Date column to Date type if it's not already
sales_data[, Date := as.Date(Date)]

# Filter data up to 03-04-2024
sales_data <- sales_data[Date <= as.Date("2024-03-04")]

# Extract day of week from Date
sales_data[, DayOfWeek := weekdays(Date)]

# Calculate days since last purchase for each customer-product combination
sales_data[, LastPurchaseDate := max(Date), by = .(Customer, Product)]
sales_data[, DaysSinceLastPurchase := as.numeric(as.Date("2024-04-09") - LastPurchaseDate)]

# Calculate purchase frequency for each customer-product combination
sales_data[, PurchaseFrequency := .N / as.numeric(max(Date) - min(Date)), by = .(Customer, Product)]

# Calculate the probability of purchase based on the day of week
sales_data[, DayMatchProbability := ifelse(DayOfWeek == MostFrequentBuyDay, 1.2, 1)]

# Predict likelihood of purchase on 04-09-2024
sales_data[, PurchaseLikelihood := (1 - exp(-PurchaseFrequency * DaysSinceLastPurchase)) * DayMatchProbability]

# Adjust purchase likelihood based on customer cluster
cluster_weights <- c(1, 1.1, 1.2, 1.3)  # Example weights, adjust based on your data
sales_data[, PurchaseLikelihood := PurchaseLikelihood * cluster_weights[CustomerCluster]]

# Select top N products for each customer based on purchase likelihood
N <- 5  # Adjust this value as needed
predictions <- sales_data[, .SD[order(-PurchaseLikelihood)][1:N], by = Customer]

# Select relevant columns for the final output
final_predictions <- predictions[, .(Customer, Product, PurchaseLikelihood, MostFrequentBuyDay, CustomerCluster)]

# Sort the predictions by Customer and PurchaseLikelihood
setorder(final_predictions, Customer, -PurchaseLikelihood)

# Write the predictions to a CSV file
fwrite(final_predictions, "predicted_purchases_2024-04-09.csv")

# Print the first few rows of the predictions
print(head(final_predictions))