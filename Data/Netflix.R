install.packages("tidyverse")
library(tidyverse)

nd <- read_csv("netflix_titles.csv")

as_tibble(nd)

nd <- subset(nd, select = -c(description,cast, date_added, director) )

nd <- nd %>%
  rename(runtime = duration) %>%
  rename(year=release_year)

nd <- mutate(nd,platform = "Netflix")

nd <- separate(nd, runtime, sep = " ",
         into = c("runtime", "excess"))

nd <- subset(nd, select = -c(excess) )

nd <- replace_na(nd,list(rating="N/A"))

nd <- replace_na(nd,list(country="N/A"))

nd <- separate(nd, country, sep = ",",
               into = c("country", "excess"))

nd <- subset(nd, select = -c(excess) )

nd <- subset(nd, select = -c(listed_in) )

# Check for duplicates
n_distinct(nd$title)

imdb <- read_csv("imdb_ratings.csv")

nd <- nd %>%
  rename(age = rating)

imdb <- subset(imdb, select = -c(primaryTitle) )

imdb <- imdb %>%
  rename(title = originalTitle)

nd <- inner_join(nd,imdb,by="title")

nd <- subset(nd, select = -c(runtime) )

nd <- nd %>% 
  distinct(title, .keep_all= TRUE)

nd <- nd %>%
  rename(runtime = runtimeMinutes) %>%
  rename(imdb_id = tconst)  %>%
  rename(rating = averageRating) 

nd <- subset(nd, select = -c(country) )

nd <- subset(nd, select = -c(show_id) )

nd <- separate(nd, genres, sep = ",",
               into = c("genre", "excess"))

nd <- subset(nd, select = -c(excess) )

nd <- nd[, c(6, 2, 1, 3, 4,7,8,9,5)]

nd <- nd %>%
  mutate(runtime=replace(runtime, runtime== "\\N", "N/A"))  %>%
  mutate(genre=replace(genre, genre== "\\N", "N/A")) 

write.csv(nd,'Netflix_cleaned.csv')

nt <- read_csv("Netflix_cleaned.csv")

ap <- read_csv("Amazon_cleaned.csv")

dp <- read_csv("Disney_cleaned.csv")

ap <- ap %>%
  rename(imdb_id = `IMDb id`) %>%
  rename(title = Name)  %>%
  rename(year = Year) %>%
  rename(age = Age) %>%
  rename(runtime = `Running Time`) %>%
  rename(rating = `IMDb Rating`)  %>%
  rename(genre = genres)

dp <- dp %>%
  rename(rating = averageRating) %>%
  rename(age = rated)

nt <- subset(nt, select = -c(1) )

ap <- replace_na(ap,list(runtime="N/A"))

glimpse(dp)

dp$runtime <- as.character(dp$runtime)

nt <- replace_na(nt,list(runtime="N/A"))

ap$rating <- as.character(ap$rating)

nt$rating <- as.character(nt$rating)

ap <- ap %>%
  mutate(genre=replace(genre, genre== "\\N", "N/A")) 

ott <- bind_rows(nt,ap,dp)

write.csv(ott,'OTT_cleaned.csv')

View(ott)

View(nd)

View(nt)

View(ap)

View(dp)

View(imdb)

#sum(is.na(ott))

glimpse(nd)

