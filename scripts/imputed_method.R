

september_data <- chevrole_cars %>% 
  filter(date == "2024-09-01")

august_data <- chevrole_cars %>% 
  filter(date == "2024-08-01")



sep_model <- lm(log(price) ~ mileage + year_production + is_petrol, data = september_data)


summary(sep_model)


aug_model <- lm(log(price) ~ mileage + year_production + is_petrol, data = august_data)


summary(aug_model)


## Extracting coefficients from both models
coeff_sep <- coef(sep_model)
coeff_aug <- coef(aug_model)

# Check the structure of your factor coefficients
names(coeff_sep)  # Check for the coefficient of is_petrol (e.g., 'is_petrol1')
names(coeff_aug)

# Calculate average characteristics for August
avg_mileage_aug <- mean(august_data$mileage)
avg_year_production_aug <- mean(august_data$year_production)

# Calculate the proportion of petrol cars in August
avg_is_petrol_aug <- mean(august_data$is_petrol == "Бензин")

# Create vector of average characteristics for August
# Order: (Intercept, avg_mileage, avg_year_production, avg_is_petrol)
z_aug <- c(1, avg_mileage_aug, avg_year_production_aug, avg_is_petrol_aug)

# Calculate Laspeyres-type price index
P_08_09 <- sum(coeff_sep * z_aug) / sum(coeff_aug * z_aug)

# Output the price index
P_08_09
