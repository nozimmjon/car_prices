
# Prepare data for Nexia
nexia_data <- chevrole_cars %>%
  filter(model == "Nexia") %>%
  arrange(date) %>%
  mutate(year_month = format(date, "%Y-%m"))

# Function to run regression for a 12-month window
run_nexia_regression <- function(data, start_date, end_date) {
  window_data <- data %>% 
    filter(date >= start_date & date <= end_date) %>%
    mutate(month_factor = factor(year_month, levels = unique(year_month)))
  
  model <- lm(log(price) ~ mileage + year_production + is_petrol + month_factor, data = window_data)
  
  coeffs <- coef(model)
  time_dummies <- c(1, exp(coeffs[grep("^month_factor", names(coeffs))]))
  
  return(time_dummies)
}

# Initial 12-month period
start_date <- min(nexia_data$date)
end_date <- start_date + months(11)

# Run initial regression
initial_time_dummies <- run_nexia_regression(nexia_data, start_date, end_date)

# Calculate initial index (base = 100 for average of first 12 months)
base_year_average <- mean(initial_time_dummies)
initial_index <- 100 * initial_time_dummies / base_year_average

# Prepare for subsequent periods
date_sequence <- seq(end_date + months(1), max(nexia_data$date), by = "month")
index_values <- c(initial_index, rep(NA, length(date_sequence)))

# Calculate index for subsequent periods
for (i in seq_along(date_sequence)) {
  current_end_date <- date_sequence[i]
  current_start_date <- current_end_date - months(11)
  
  # Run regression for current window
  current_time_dummies <- run_nexia_regression(nexia_data, current_start_date, current_end_date)
  
  # Calculate growth rate between the last two months of the current window
  growth_rate <- current_time_dummies[12] / current_time_dummies[11]
  
  # Calculate the new index value
  if (i == 1) {
    # For the first subsequent period, use the last value of the initial index
    index_values[length(initial_index) + i] <- index_values[length(initial_index)] * growth_rate
  } else {
    # For all other periods, use the previous calculated value
    index_values[length(initial_index) + i] <- index_values[length(initial_index) + i - 1] * growth_rate
  }
}

# Create result dataframe
result_df <- data.frame(
  date = c(seq(start_date, end_date, by = "month"), date_sequence),
  index = index_values
)

# Calculate month-to-month growth rates
result_df <- result_df %>%
  mutate(growth_rate = (index / lag(index) - 1) * 100)

# Create two plots: one for the index and one for the growth rate
plot_index <- ggplot(result_df, aes(x = date, y = index)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Nexia Price Index",
       x = "Date",
       y = "Index Value (Base = 100 for average of first 12 months)")

plot_growth_rate <- ggplot(result_df, aes(x = date, y = growth_rate)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Nexia Price Monthly Growth Rate",
       x = "Date",
       y = "Growth Rate (%)")
