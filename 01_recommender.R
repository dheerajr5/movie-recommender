# Ref
# https://www.youtube.com/watch?v=4rVDNx8osG0
# https://files.grouplens.org/datasets/movielens/ml-latest-small.zip
# https://files.grouplens.org/datasets/movielens/ml-latest-small-README.html

suppressPackageStartupMessages({
    library(tidyverse)
    library(qdapTools)
    library(recommenderlab)
})


movies <- read.csv('movies.csv')
ratings <- read.csv('ratings.csv')
tags <- read.csv('tags.csv')

movies_clean <- movies %>% 
    cbind(mtabulate(str_split(movies$genres,
                        pattern = '\\|'))) %>%
    select(-title, -genres, -`(no genres listed)`) 

ratings <- ratings %>% group_by(movieId) %>% 
    summarise(rating = mean(rating, na.rm = TRUE),
              .groups = 'drop') 
movies_rating <- movies_clean %>% 
    inner_join(ratings, by = 'movieId') %>% 
    select(-rating) %>% 
    column_to_rownames(var = 'movieId') %>% 
    as.matrix() %>% as('binaryRatingMatrix')

genres <- movies_rating %>% colnames() %>% as_tibble()

tags_sel <- tags %>% 
    filter(!(tag %in% c('sci-fi', 'horror', 'romance', 
                        'comdey', 'action', 'funny', 'BD-R'))) %>% 
    group_by(tag) %>% tally() %>% slice_max(n, n = 15)

genres_sel <- movies %>% group_by(genres) %>% tally() %>% slice_max(n, n = 15)

tags_valid <- tags %>% select(-userId, -timestamp) %>% 
    filter(tag %in% tags_sel$tag) %>% 
    group_by(movieId) %>% 
    mutate(tag = paste0(unique(tag), collapse = ',')) %>% 
    unique()

movies_full <- movies %>% 
    inner_join(ratings, by = 'movieId') %>% 
    left_join(tags_valid, by = 'movieId')

recom <- Recommender(movies_rating, method = 'IBCF',
                     param = list(k = 5))

genre_ch <- c('Action', 'Adventure', 'Comedy')

genres_ch_mat <- genres %>% mutate(genre = as.numeric(value %in% genre_ch)) %>% 
    pivot_wider(names_from = value, values_from = genre) %>% 
    as.matrix() %>% as('binaryRatingMatrix')

pred <- predict(recom, genres_ch_mat)

# fav_genre <- getList(pred) %>% as.character()
# fav_rating <- getRatings(pred) %>% as.numeric()

fav_genre <- pred@itemLabels[pred@items[["0"]][1]] # get top 1 label
fav_rating <- pred@ratings[["0"]] %>% max() # get max rating


tags_ch <- 'superhero'

matches <- movies_full %>% 
    filter(str_detect(movies_full$genres, fav_genre) == TRUE,
           str_detect(movies_full$tag, tags_ch) == TRUE) %>% 
    mutate(match = fav_rating * rating) %>% 
    arrange(desc(match)) %>% glimpse()
    
