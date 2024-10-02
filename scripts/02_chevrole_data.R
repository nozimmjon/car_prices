chevrole_cars <- car_data %>% 
  # Filter for Chevrolet-related brands and apply data quality filters
  filter(str_detect(model, regex("Chevrolet|Chevy|Daewoo|Ravon", ignore_case = TRUE)),
         mileage < 600000) %>% 
  mutate(
    original_model = model,  # Keep original model name for reference
    model = str_remove_all(model, regex("Chevrolet|Chevy|Daewoo|Ravon", ignore_case = TRUE)),
    model = str_remove_all(model, "\\(.*\\)"),  # Remove anything in parentheses
    model = str_trim(model),
    # Add more comprehensive model consolidation here
    model = case_when(
      str_detect(model, regex("^Nexia|^R4|^R2", ignore_case = TRUE)) ~ "Nexia",
      str_detect(model, regex("^Matiz", ignore_case = TRUE)) ~ "Matiz",
      str_detect(model, regex("^Spark", ignore_case = TRUE)) ~ "Spark",
      str_detect(model, regex("^Lacetti", ignore_case = TRUE)) ~ "Lacetti",
      str_detect(model, regex("^Cobalt", ignore_case = TRUE)) ~ "Cobalt",
      str_detect(model, regex("^Captiva", ignore_case = TRUE)) ~ "Captiva",
      str_detect(model, regex("^Cruze", ignore_case = TRUE)) ~ "Cruze",
      str_detect(model, regex("^Aveo", ignore_case = TRUE)) ~ "Aveo",
      str_detect(model, regex("^Malibu", ignore_case = TRUE)) ~ "Malibu",
      str_detect(model, regex("^Epica", ignore_case = TRUE)) ~ "Epica",
      str_detect(model, regex("^Tracker", ignore_case = TRUE)) ~ "Tracker",
      str_detect(model, regex("^Trailblazer", ignore_case = TRUE)) ~ "Trailblazer",
      TRUE ~ model
    )
  ) %>%
  # Continue with the rest of the cleaning process
  mutate(
    # Convert relevant columns to appropriate types
    price = as.numeric(price),
    year_production = as.numeric(year_production),
    mileage = as.numeric(mileage),
    
    # Convert categorical variables to factors
    across(c(model, type, color, position, is_petrol), as.factor),
    
    # Format date and extract month
    date = as.Date(date),
    month = factor(month(date), levels = 1:12, labels = month.abb)
  ) %>%
  # Remove rows with NA in critical columns
  #filter(!is.na(price), !is.na(year_production), !is.na(mileage)) %>%
  # Filter out old production years (assuming we want cars from 2000 onwards)
  filter(year_production >= 2000) %>%
  # Remove outliers in price for each model
  group_by(model) %>%
  filter(between(price, quantile(price, 0.01, na.rm = TRUE), quantile(price, 0.99, na.rm = TRUE))) %>%
  ungroup() %>%
  # Filter out models with fewer than 400 observations
  group_by(model) %>%
  filter(n() >= 400) %>%
  ungroup()


-----
  # Analyze missing fuel type data by model
  chevrole_fuel_analysis <- chevrole_cars %>%
    group_by(model) %>%
    summarise(
      total_count = n(),
      missing_fuel_type = sum(is.na(is_petrol)),
      missing_percentage = (missing_fuel_type / total_count) * 100
    ) %>%
    arrange(desc(missing_percentage))
  
  # Function to perform mode imputation
  mode_imputation <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Separate models with high and low missing percentages
  high_missing <- chevrole_fuel_analysis$model[chevrole_fuel_analysis$missing_percentage > 20]
  low_missing <- chevrole_fuel_analysis$model[chevrole_fuel_analysis$missing_percentage <= 20]
  
  # Imputation for models with low missing percentages
  chevrole_cars_imputed <- chevrole_cars %>%
    group_by(model) %>%
    mutate(
      is_petrol_imputed = if_else(model %in% low_missing & is.na(is_petrol),
                                  mode_imputation(is_petrol),
                                  is_petrol)
    ) %>%
    ungroup()
  
  # Prepare data for decision tree imputation
  train_data <- chevrole_cars_imputed %>%
    filter(!is.na(is_petrol_imputed)) %>%
    select(model, year_production, mileage, price, is_petrol_imputed)
  
  # Train decision tree model
  tree_model <- rpart(is_petrol_imputed ~ ., data = train_data, method = "class")
  
  # Impute remaining missing values using the decision tree
  chevrole_cars_imputed <- chevrole_cars_imputed %>%
    mutate(
      is_petrol_imputed = if_else(is.na(is_petrol_imputed),
                                  predict(tree_model, newdata = ., type = "class"),
                                  is_petrol_imputed)
    )
  
  # Update the chevrole_cars dataset with imputed values
  chevrole_cars <- chevrole_cars_imputed %>%
    mutate(is_petrol = is_petrol_imputed) %>%
    select(-is_petrol_imputed)
  
  # List of models to exclude
  models_to_exclude <- c("Onix", "Equinox", "Monza", "Labo", "Epica")
  
  # Filter out the specified models
  chevrole_cars <- chevrole_cars %>%
    filter(!model %in% models_to_exclude)

  
  write_xlsx(chevrole_cars, "chevrole_cars.xlsx")


summary_stats <- chevrole_cars %>%
  group_by(model, date) %>%
  summarise(
    count = n(),
    avg_price = mean(price, na.rm = TRUE),
    min_year = min(year_production, na.rm = TRUE),
    max_year = max(year_production, na.rm = TRUE)
  ) %>%
  arrange(desc(count))