

car_data <- combined_data %>% 
  select(-c(1:2, 7, 11, 13, 15))  # Selecting columns by index
  

names(car_data) <- c("model", 
                     "price", 
                     "year", 
                     "region", 
                     "date", 
                     "combustion", 
                     "type", 
                     "mileage",
                     "transmission", 
                     "color", 
                     "description", 
                     "month")

car_data <- car_data %>% 
  mutate(across(c(price, year,mileage), 
                ~as.numeric(gsub("[^[:digit:]]", "", .)))) # Remove all characters except numbers

# Remove all characters except numbers in the price column
car_data$price <- gsub("[^[:digit:]]", "", car_data$price)
car_data$year <- gsub("[^[:digit:]]", "", car_data$year)
car_data$mileage <- gsub("[^[:digit:]]", "", car_data$mileage)

#recoding Null as NA
car_data_raw <-  car_data %>% 
  mutate_all(~ifelse(. == "Null", NA, .)) %>% 
  mutate(price = as.numeric(price),
         mileage = as.numeric(mileage), 
         year_production  = year(as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))) %>% 
  distinct() %>%
  mutate(position = as.numeric(gsub("^.*?(\\d+).*позиция.*$", "\\1", model)), # extracting the word "position" 
         is_petrol = gsub("^.*\\((.*)\\).*$", "\\1", combustion), # extracting the word in brackets in the new col
         power_com = sub("\\s+\\(.*", "", combustion),
         model = sub("^(.*?),.*$", "\\1", model)) %>% 
  mutate(description = str_remove_all(description, "[.:]"),
         description = str_to_lower(description)) %>% 
  mutate(mileage = ifelse(is.na(mileage), 
                          parse_number(str_extract(description, "\\d+[.,]?\\d*"))[str_detect(description, "km")], 
                                                  mileage)) %>% 
    mutate(mileage = ifelse(is.na(mileage), 
                          parse_number(str_extract(description,  "\\d+"))[str_detect(description, "км")], 
                          mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "yurgan"), 
                           str_extract(description, "\\d+(?=\\s*yurgan)") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "пробег"), 
                           str_extract(description, "\\d+(?=\\s*пробег)") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "probeg|prabeg|prabek"), 
                           str_extract(description, "\\d+(?=\\s*probeg|\\s*prabeg|\\s*prabek)") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "минг"), 
                           str_extract(description, "\\d+(?=\\s*минг)") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "ming|MIng"), 
                           str_extract(description, "\\d+(?=\\s*ming|\\s*MIng)") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "пробег"), 
                           str_extract(description, "(?<=пробег\\s)\\d+") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "probeg"), 
                           str_extract(description, "(?<=probeg\\s)\\d+") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "prabeg"), 
                           str_extract(description, "(?<=prabeg\\s)\\d+") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "prabegi"), 
                           str_extract(description, "(?<=prabegi\\s)\\d+") %>% as.numeric(), 
                           mileage)) %>% 
    mutate(mileage = if_else(is.na(mileage) & str_detect(description, "probek"), 
                           str_extract(description, "(?<=probek\\s)\\d+") %>% as.numeric(), 
                           mileage)) 


chevrole_cars <- car_data_raw %>% 
  filter(!is.na(position)) %>% 
  mutate(model = gsub("Chevrolet\\s*", "", model),
         model = gsub("^Daewoo\\s*", "", model),
         model = gsub("^Ravon\\s*", "", model),
         model= gsub("\\(.*\\)", "", model),
         model= trimws(model)) %>% 
  filter(mileage<600000)
 
chevrole_cars$model <- ifelse(chevrole_cars$model == "Nexia II", "Nexia 2", chevrole_cars$model)
chevrole_cars$model <- ifelse(chevrole_cars$model == "R2", "Nexia 2", chevrole_cars$model)
chevrole_cars$model <- ifelse(chevrole_cars$model == "Nexia R3", "Nexia 3", chevrole_cars$model)
chevrole_cars$model <- ifelse(chevrole_cars$model == "Matiz Creative", "Matiz", chevrole_cars$model)
chevrole_cars$model <- ifelse(chevrole_cars$model == "Nexia 2", "Nexia", chevrole_cars$model)
chevrole_cars$model <- ifelse(chevrole_cars$model == "Nexia 3", "Nexia", chevrole_cars$model)


chevrole_cars_cleaned <- chevrole_cars %>% 
  mutate(across(c(transmission, color, position, is_petrol), factor),
         model = str_trim(as.factor(model))) %>% 
  filter(!model %in% c("R4", "Orlando"),
         year > 2003,
         # mileage != 0,
         # year_production != 2023
  ) %>% 
  group_by(model) %>% 
  filter(between(price, quantile(price, 0.01), quantile(price, 0.99)),
         ) %>% 
  ungroup() %>% 
  mutate(model = if_else(model == "Matiz", "Matiz Best", model)) 


# Create a new column "monthss" with explicit levels for each month
chevrole_cars_cleaned <- chevrole_cars_cleaned %>%
  mutate(monthss = factor(month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), 
                          labels = c("January", "February", "March", "April", "May", "June", "July", 
                                     "August", "September", "October", "November")
  )
  )



write_xlsx(chevrole_cars_cleaned, "chevrole_data.xlsx")


summarytools::dfSummary(chevrole_cars_cleaned) %>% stview()

# new_chevrole <- chevrole_cars %>% 
#   filter(mileage == 0 | is.na(mileage)) %>% 
#   mutate(mileage = if_else(year_production == 2023 |   
#                            year_production == 2022, 0, mileage)) %>% 
#   select(model, price, description, year_production, mileage)




# After this transformation, let's check for any remaining "Null" values
null_check <- car_data %>%
  summarise(across(where(is.character), ~sum(. %in% c("Null", "NULL"), na.rm = TRUE)))

print("Number of remaining 'Null' or 'NULL' values in each character column:")
print(null_check)



missing_before <- sapply(car_data, function(x) sum(is.na(x)))
print("Missing values before cleaning:")
print(missing_before)

# Check mileage before and after extraction
print("Missing mileage values before extraction:")
print(sum(is.na(car_data$mileage)))
print("Missing mileage values after extraction:")
print(sum(is.na(car_data$mileage_extracted)))
  