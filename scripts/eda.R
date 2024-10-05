

chevrole_cars %>% is.na() %>% colSums()


library(naniar)
vis_miss(chevrole_cars, warn_large_data = FALSE)



chevrole_cars %>% dim()

chevrole_cars %>% distinct() %>% nrow()

duplicate_rows <- chevrole_cars %>% 
  janitor::get_dupes()


view(dfSummary(chevrole_cars))