# Set the path to your Excel file
here()
#Function to read files from a specific year folder
    read_year_files <- function(year) {
      folder_path <- paste0("data/", year, "/")
      files <- list.files(folder_path, pattern = "*.xlsx", full.names = TRUE)
      
      data_list <- map(files, function(file) {
        df <- read_excel(file)
        month <- as.numeric(substr(basename(file), 1, 2))
        df$month <- month
        df$year <- as.numeric(year)
        return(df)
      })
      
      return(bind_rows(data_list))
    }

#Read data from 2023 and 2024
    data_2023 <- read_year_files("2023")
    data_2024 <- read_year_files("2024")
    
#Combine all data
    combined_data <- bind_rows(data_2023, data_2024) %>% 
      mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
      arrange(date) 