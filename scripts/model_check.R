

# Run the regression on the full dataset
model <- lm(log(price) ~ mileage + year_production + is_petrol + factor(month) + model, data = chevrole_cars)


model %>% model_dashboard()




------
  
  
library(tidyverse)
library(broom)
library(car)
library(lmtest)

# Function to run diagnostics on a single model
run_model_diagnostics <- function(model_data) {
  model_data <- model_data %>%
    arrange(date) %>%
    mutate(year_month = format(date, "%Y-%m"))
  
  # Use the most recent 12 months of data for the diagnostic model
  end_date <- max(model_data$date)
  start_date <- end_date - months(11)
  
  window_data <- model_data %>% 
    filter(date >= start_date & date <= end_date) %>%
    mutate(month_factor = factor(year_month, levels = unique(year_month)))
  
  model <- lm(log(price) ~ mileage + year_production + is_petrol + month_factor, data = window_data)
  
  # Gather diagnostic information
  diagnostics <- list(
    summary = glance(model),
    coefficients = tidy(model),
    vif = vif(model),
    bp_test = bptest(model),
    dw_test = dwtest(model)
  )
  
  return(diagnostics)
}

# Run diagnostics for all models
model_diagnostics <- map(models, function(model_name) {
  model_data <- filter(chevrole_cars, model == model_name)
  run_model_diagnostics(model_data)
})
names(model_diagnostics) <- models

# Function to print diagnostics summary
print_model_diagnostics <- function(model_name, diagnostics) {
  cat("Diagnostics for model:", model_name, "\n")
  cat("R-squared:", round(diagnostics$summary$r.squared, 3), "\n")
  cat("Adjusted R-squared:", round(diagnostics$summary$adj.r.squared, 3), "\n")
  cat("Residual standard error:", round(diagnostics$summary$sigma, 3), "\n")
  cat("F-statistic p-value:", format.pval(diagnostics$summary$p.value, digits = 3), "\n")
  
  cat("\nSignificant coefficients (p < 0.05):\n")
  print(filter(diagnostics$coefficients, p.value < 0.05) %>% select(term, estimate, p.value))
  
  cat("\nVariance Inflation Factors:\n")
  print(diagnostics$vif)
  
  cat("\nBreusch-Pagan test for heteroscedasticity:\n")
  print(diagnostics$bp_test)
  
  cat("\nDurbin-Watson test for autocorrelation:\n")
  print(diagnostics$dw_test)
  
  cat("\n-------------------------------------------\n\n")
}

# Print diagnostics for all models
walk2(names(model_diagnostics), model_diagnostics, print_model_diagnostics)

# Summarize overall model performance
overall_performance <- map_dfr(model_diagnostics, function(diag) {
  tibble(
    r_squared = diag$summary$r.squared,
    adj_r_squared = diag$summary$adj.r.squared,
    residual_se = diag$summary$sigma,
    bp_test_pvalue = diag$bp_test$p.value,
    dw_test_pvalue = diag$dw_test$p.value
  )
}, .id = "model")

print("Overall model performance summary:")
print(overall_performance)

# Plot R-squared values
ggplot(overall_performance, aes(x = reorder(model, r_squared), y = r_squared)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "R-squared values for Chevrolet models",
       x = "Model", y = "R-squared") +
  theme_minimal()

ggsave("chevrolet_model_rsquared.png", width = 10, height = 8)
print("R-squared plot saved as 'chevrolet_model_rsquared.png'")