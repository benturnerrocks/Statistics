
library(tidyverse)
library(stringr)
library(purrr)
library(dplyr)
library(lubridate)

# Creating names for columns
streamingServices <- c("amazon_prime", "netflix","hbo_max","paramount_plus","hulu","disney_plus","crunchyroll", "rakuten", "dark_matter")
streamingServiceNames <- c("Amazon Prime", "Netflix","HBO Max","Paramount+","Hulu","Disney+","Crunchyroll", "Rakuten", "Dark Matter")

# Creating URLs
titleURLs <- str_c("Data/", streamingServices, "/titles.csv")
creditURLs <- str_c("Data/", streamingServices, "/credits.csv")

# Adding names to dfs
listOfTitleDFs <- map(titleURLs, read_csv)
names(listOfTitleDFs) <- streamingServiceNames
listOfCreditDFs <- map(creditURLs, read_csv)
names(listOfCreditDFs) <- streamingServiceNames

# Binding list of dfs into master DFs
masterTitles <- bind_rows(listOfTitleDFs,.id = "streaming_service")
masterCredits <- bind_rows(listOfCreditDFs,.id = "streaming_service")

# Data cleaning before join
# Get rid of streaming service from titles
masterCredits <- masterCredits %>%
  select(-streaming_service,-character) %>% 
  filter(role == "DIRECTOR")

masterTitles <- 
  masterTitles %>%
  mutate(release_year_date = as.Date(release_year %>% as.character(),format="%Y"))

# Left join
masterDF <- left_join(masterCredits, masterTitles, by=c("id"))

write_csv(masterDF, "merged_data.csv")

# Boxplot displaying the average imdb scores of content on each streaming service
masterDF %>% 
  select(title, streaming_service, imdb_score) %>% 
  remove_missing() %>%
  group_by(streaming_service) %>%
  ggplot() +
  geom_boxplot(aes(x = streaming_service, y = imdb_score)) + 
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) 

# A table for context for the previous boxplot displaying the total number of titles
# under each streaming service
table(masterDF$streaming_service)

# table summarizing the top 10 directors who have directed the most movies/shows
masterDF %>%
  group_by(name) %>%
  summarize(numMovies = n()) %>%
  slice_max(order_by = numMovies, n = 10)

#take into account votes
masterDF %>%
  group_by(release_year) %>%
  dplyr::summarize(averageRating = mean(imdb_score,na.rm=T) - mean(tmdb_score,na.rm=T)) %>%
  ggplot(aes(x=release_year, y=averageRating)) +
  geom_point()

# Line graph displaying total number of movies and shows across all platforms throughout
# all release years
masterDF %>% 
  group_by(type, release_year) %>% 
  summarize(total = n()) %>%
  ggplot(aes(x = release_year, y = total, color = type)) +
    geom_line()
  