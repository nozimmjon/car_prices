
car_data <- combined_data %>% 
  select(-c(1:2, 7, 11, 13, 15:17)) %>%  # Selecting columns by index
  rename(model = 1, price = 2, year_production = 3, region = 4, combustion = 5, type = 6, mileage = 7, 
         color = 8, description = 9) %>%
  mutate(
    across(where(is.character), ~na_if(., "Null")),  # Convert "Null" to NA in character columns
    across(where(is.character), ~na_if(., "")),      # Convert empty strings to NA in character columns
    across(where(is.character), trimws),  # Trim whitespace from character columns
    price = gsub("[^[:digit:]]", "", price),
    year_production = gsub("[^[:digit:]]", "", year_production),
    mileage = gsub("[^[:digit:]]", "", mileage),
    mileage_original = mileage,
    across(c(price, year_production, mileage), ~as.numeric(.)),
    position = as.numeric(gsub("^.*?(\\d+).*позиция.*$", "\\1", model)),
    is_petrol = gsub("^.*\\((.*)\\).*$", "\\1", combustion),
    power_com = sub("\\s+\\(.*", "", combustion),
    model = sub("^(.*?),.*$", "\\1", model),
    description = str_remove_all(str_to_lower(description), "[.:]"),
    mileage = case_when(
      !is.na(mileage) ~ mileage,
      str_detect(description, "km") ~ parse_number(str_extract(description, "\\d+[.,]?\\d*")),
      str_detect(description, "км|yurgan|пробег|probeg|prabeg|prabek|минг|ming|MIng") ~ 
        as.numeric(str_extract(description, "\\d+(?=\\s*(км|yurgan|пробег|probeg|prabek|минг|ming|MIng))")),
      TRUE ~ NA_real_
    ), 
    across(where(is.character), ~na_if(., "Null")),  # Convert "Null" to NA in character columns
    across(where(is.character), ~na_if(., "")),      # Convert empty strings to NA in character columns
    is_petrol = case_when(
      is_petrol %in% c("Газ", "Газ-бензин") ~ "Газ",
      TRUE ~ is_petrol)
   ) %>%
  distinct()

#dfSummary(chevrole_cars) %>% stview()

car_data <- car_data %>% 
  mutate(date = as.Date(date))

# Improved mileage extraction function with better NA handling
extract_mileage <- function(description, mileage, mileage_original) {
  # If mileage is not NA, return it
  if (!is.na(mileage)) return(as.numeric(mileage))
  
  # If description is NA or empty, try mileage_original
  if (is.na(description) || description == "") {
    return(if(!is.na(mileage_original) && mileage_original != "") as.numeric(mileage_original) else NA_real_)
  }
  
  # Patterns for mileage
  km_pattern <- "\\b(\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|\\d+)\\s*(?:km|км|тыс[.]?\\s*км|тысяч\\s*км|к[.]м)\\b"
  thousand_km_pattern <- "\\b(\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|\\d+)\\s*тыс(?:\\.|яч)?\\s*(?:км|km)?\\b"
  other_mileage_pattern <- "\\b(\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|\\d+)\\s*(?:yurgan|пробег|probeg|prabeg|prabek|минг|ming|MIng)\\b"
  
  # Patterns to exclude (price-like numbers)
  price_pattern <- "\\b(?:1[4-9]|[2-9]\\d)\\s*(?:mil|млн|million|миллион)\\w*\\b"
  
  # Check for price-like pattern
  if (str_detect(description, price_pattern)) return(NA_real_)
  
  # Extract mileage
  if (str_detect(description, km_pattern)) {
    mileage_str <- str_extract(description, km_pattern)
    return(as.numeric(gsub("[^0-9.]", "", mileage_str)))
  } else if (str_detect(description, thousand_km_pattern)) {
    mileage_str <- str_extract(description, thousand_km_pattern)
    return(as.numeric(gsub("[^0-9.]", "", mileage_str)) * 1000)
  } else if (str_detect(description, other_mileage_pattern)) {
    mileage_str <- str_extract(description, other_mileage_pattern)
    return(as.numeric(gsub("[^0-9.]", "", mileage_str)))
  }
  
  # If no mileage found, try mileage_original
  if (!is.na(mileage_original) && mileage_original != "") {
    return(as.numeric(mileage_original))
  }
  
  return(NA_real_)
}

# Apply the improved mileage extraction
improved_car_data <- car_data %>%
  mutate(
    improved_mileage = pmap_dbl(list(description, mileage, mileage_original), 
                                ~extract_mileage(..1, ..2, ..3))
  )


# Update the car_data with the improved mileage
car_data <- improved_car_data %>%
  mutate(mileage = coalesce(mileage, improved_mileage))
