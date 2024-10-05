

# Function to calculate index for a single model
calculate_model_index <- function(model_data) {
  model_data <- model_data %>%
    arrange(date) %>%
    mutate(year_month = format(date, "%Y-%m"))
  
  run_regression <- function(data, start_date, end_date) {
    window_data <- data %>% 
      filter(date >= start_date & date <= end_date) %>%
      mutate(month_factor = factor(year_month, levels = unique(year_month)))
    
    model <- lm(log(price) ~ mileage + year_production + is_petrol + month_factor, data = window_data)
    
    coeffs <- coef(model)
    time_dummies <- c(1, exp(coeffs[grep("^month_factor", names(coeffs))]))
    
    return(time_dummies)
  }
  
  start_date <- min(model_data$date)
  end_date <- start_date + months(11)
  
  initial_time_dummies <- run_regression(model_data, start_date, end_date)
  base_year_average <- mean(initial_time_dummies)
  initial_index <- 100 * initial_time_dummies / base_year_average
  
  date_sequence <- seq(end_date + months(1), max(model_data$date), by = "month")
  index_values <- c(initial_index, rep(NA, length(date_sequence)))
  
  for (i in seq_along(date_sequence)) {
    current_end_date <- date_sequence[i]
    current_start_date <- current_end_date - months(11)
    
    current_time_dummies <- run_regression(model_data, current_start_date, current_end_date)
    growth_rate <- current_time_dummies[12] / current_time_dummies[11]
    
    if (i == 1) {
      index_values[length(initial_index) + i] <- index_values[length(initial_index)] * growth_rate
    } else {
      index_values[length(initial_index) + i] <- index_values[length(initial_index) + i - 1] * growth_rate
    }
  }
  
  return(data.frame(
    date = c(seq(start_date, end_date, by = "month"), date_sequence),
    index = index_values
  ))
}

# Calculate indices for all models
models <- unique(chevrole_cars$model)
model_indices <- map(models, ~calculate_model_index(filter(chevrole_cars, model == .x)))
names(model_indices) <- models


# Print summary of each model's index
for (model_name in names(model_indices)) {
  cat("Summary for model:", model_name, "\n")
  print(summary(model_indices[[model_name]]$index))
  cat("\n")
}
# Calculate weights based on number of observations
model_weights <- chevrole_cars %>%
  group_by(model) %>%
  summarise(weight = n()) %>%
  mutate(weight = weight / sum(weight))

# Combine sub-indices into overall index
overall_index <- bind_rows(model_indices, .id = "model") %>%
  left_join(model_weights, by = "model") %>%
  group_by(date) %>%
  summarise(overall_index = weighted.mean(index, weight, na.rm = TRUE)) %>%
  ungroup()


# Calculate overall price change
first_index <- overall_index$overall_index[1]
last_index <- overall_index$overall_index[nrow(overall_index)]
overall_change <- (last_index / first_index - 1) * 100

# Calculate growth rates
overall_index <- overall_index %>%
  mutate(growth_rate = (overall_index / lag(overall_index) - 1) * 100)
