
library(tidyverse)
library(tidytext)
library(widyr)
library(readxl)

# convert the descriptions to a tidy format
words <- chevrole_cars %>% 
  select(description) %>% 
  unnest_tokens(word, description)

# count the frequency of each word
word_counts <- words %>% 
  count(word, sort = TRUE)

# display the top 20 most frequent words
head(word_counts, 50) %>% 
  print(n = 40)


result <- car_data_01 %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, description) %>%
  #count(word, sort = TRUE, name = "n") %>%
  
  pairwise_count(word,  line, sort = TRUE) %>%
  filter(n > 10)


pairwise_count(dat, letter, group)