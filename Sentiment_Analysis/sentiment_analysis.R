library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(rlang)
library(ggplot2)
library(tibble)
library(stringr)
library(rtweet)
library(sentimentr)
library(lubridate)

#IMPORTING LEXICONS CODE--------------------------------------------------------
  #Loading in sentiment lexicon
  afinn_lexicon <- get_sentiments("afinn")
  
  #Importing valence lexicon
  valence_lexicon <- as.data.frame(lexicon::hash_valence_shifters)
  valence_lexicon <- valence_lexicon %>% mutate(y = as.numeric(y))
  
  polarity_lexicon = read_csv(file="lexicons/custom_lexicon.csv")
  #polarity_lexicon = read_csv(file="/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/sentiment-functions/lexicon/afinn/afinn_lexicon.csv")
  polarity_lexicon = polarity_lexicon %>% select(-lexicon)
  colnames(polarity_lexicon) = c("word", "value")
  colnames(valence_lexicon) = c("word", "value")
  valence_lexicon = valence_lexicon %>% anti_join(polarity_lexicon, by="word")



#TWITTER CODE-------------------------------------------------------------------
#Authenticate via access token
defaultW <- getOption("warn") 
options(warn = -1) 
token_csv <- read.table("/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/token/twitter_token.csv", header = TRUE, sep = ",")
options(warn = defaultW)

twitter_token <- function(token_csv)
{
  token <- create_token(
    app = as.character(token_csv$app),
    consumer_key = as.character(token_csv$consumer_key),
    consumer_secret = as.character(token_csv$consumer_secret),
    access_token = as.character(token_csv$access_token),
    access_secret = as.character(token_csv$access_secret))

  return(token)
}

token <- twitter_token(token_csv)
identical(token, get_token())

#CREATING RANDOM TWEETS DATAFRAME-----------------------------------------------
create_ID_variable <- function(df){
  df$ID <- seq.int(nrow(df)) #gets ID variable for unnested_text dataframe
  return(df)
}

#Pulling lat, lon, census_tract from location data and adding radius for search_tweet
reduce_location_csv_width <- function(locations_csv){
  locations <- church_and_school_locations %>% 
    select(lat, lon, census_tract) %>% 
    add_column(radius = "5mi")
}

#formats location data for search_tweet function and creates new df
format_locations <- function(locations_csv){
  locations <- reduce_location_csv_width(locations_csv) 
  locations <- create_ID_variable(locations)
  #pulls lat/lon from csv file
  for(i in locations){
    lon <- locations$lon
    lat <- locations$lat
    census_tract <- locations$census_tract
    radius <- locations$radius
    coords <- paste0(lat, ",", lon, collapse = ";")
    break
  }
  #formats lat, lon, radius
  coords <- strsplit(coords, ";")
  for (i in coords) {
    coords_formatted <- paste0(coords[[1]], ",",radius) #radius for search tweet
  }
  coords_formatted <- data.frame(coords_formatted)
  coords_formatted <- create_ID_variable(coords_formatted)
  locations <- merge(locations, coords_formatted, by = "ID")
  locations <- locations %>% select(-ID)
  return(locations)
}

#pastes formatted lat/lon/radius into search_tweet function
search_tweet <- function(formatted_df) {
  single_search_df <- data.frame()
  every_search_df <- data.frame()
  formatted_coordinate_df <- data.frame(formatted_locations$coords_formatted)
  for (i in formatted_coordinate_df$formatted_locations.coords_formatted) {
    print(i)
    try ({
      single_search_df <- search_tweets("lang:en", geocode = i, n = 100, retryonratelimit = FALSE) 
      single_search_df <- single_search_df %>% #adds coords_formatted column to search_tweet column
        add_column(coords_formatted = i)
      single_search_df <- left_join(single_search_df, formatted_df, by = "coords_formatted") #left joins location data on coord_formatted column
      every_search_df <- rbind(single_search_df, every_search_df) #combines this search with the previous searches
    })
    
  }
  every_search_df <- every_search_df[!duplicated(every_search_df$screen_name), ] #removes duplicates due to large radius
  every_search_df <- clean_text(every_search_df)
  every_search_df <- reduce_tweets_df_width(every_search_df)
  return(every_search_df)
}

#ROSS LEXICON FUNCTIONS---------------------------------------------------------
#Ross' afinn score function
score_sentiment_afinn = function(content)
{
  content = tolower(content)
  content = gsub("[^[:alnum:] ]", "", content)
  content_tibble <- strsplit(content, " ")[[1]] %>% tibble()
  colnames(content_tibble) = c("word")
  sentiment_tibble = get_sentiments("afinn")
  results_tibble = inner_join(content_tibble, sentiment_tibble, by=c("word"))
  avg_sentiment = mean(results_tibble$value, na.rm = TRUE)
  if (avg_sentiment %>% is.nan())
  {
    return (NA)
  }
  else
  {
    return (avg_sentiment)
  }
}

#Ross' valence score function
score_domain_specific_sentiment = function(content, score_type="valence")
{
  content = tolower(content)
  if (score_type == "valence")
  {
    sentiment_df = content %>% sentimentr::sentiment_by(polarity_dt = polarity_lexicon %>% as_key(),
                                                        valence_shifters_dt = valence_lexicon %>% as_key(comparison = NULL))
    sentiment_score_for_content = sentiment_df$ave_sentiment[1]
    return(sentiment_score_for_content)
  }
  if (score_type == "no valence")
  {
    content_as_vector = strsplit(content, "\\s+")[[1]] %>% str_remove_all("[[:punct:]]")
    words_in_content = content_as_vector %>% length()
    positive_words = polarity_lexicon %>% filter(y == 1)
    positive_words = positive_words$x
    positive_word_count = positive_words %>% intersect(content_as_vector) %>% length()
    negative_words = polarity_lexicon %>% filter(y == -1)
    negative_words = negative_words$x
    negative_word_count = negative_words %>% intersect(content_as_vector) %>% length()
    sentiment_score_for_content = (positive_word_count - negative_word_count) / words_in_content
    return(sentiment_score_for_content)
  }
  if (score_type == "both")
  {
    sentiment_df = content %>% sentimentr::sentiment_by(polarity_dt = polarity_lexicon %>% as_key(),
                                                        valence_shifters_dt = valence_lexicon %>% as_key())
    sentiment_score_for_content = sentiment_df$ave_sentiment[1]
    if (sentiment_score_for_content == 0)
    {
      content_as_vector = strsplit(content, "\\s+")[[1]] %>% str_remove_all("[[:punct:]]")
      words_in_content = content_as_vector %>% length()
      positive_words = polarity_lexicon %>% filter(y == 1)
      positive_words = positive_words$x
      positive_word_count = count_occurences_of_words(content_as_vector, positive_words)
      negative_words = polarity_lexicon %>% filter(y == -1)
      negative_words = negative_words$x
      negative_word_count = count_occurences_of_words(content_as_vector, negative_words)
      sentiment_score_for_content = (positive_word_count - negative_word_count) / words_in_content
    }
    return(sentiment_score_for_content)
  }
}

#ROSS LEXICON DATAFRAME---------------------------------------------------------
# simple_tweets <- tweets %>% select(screen_name, text)
# simple_tweets$afinn_sentiment = unlist(lapply(simple_tweets$text, score_sentiment_afinn)) # different bounds than nrc and bing
# simple_tweets$custom_sentiment = unlist(lapply(simple_tweets$text, score_domain_specific_sentiment))
# simple_tweets <- simple_tweets %>% na_if (0) # -1 to 1
# View(simple_tweets)

#CLEANING TWITTER TEXT----------------------------------------------------------
clean_text <- function(text) {
  #text column should be named 'text' in dataframe
  text$text = tolower(text$text)
  text$text = gsub("[^[:alnum:] ]", "", text$text)
  return(text)
}

#LEXICON -----------------------------------------------------------------------
reduce_tweets_df_width <- function(tweets) {
  tweets <- tweets %>%
    select(screen_name, created_at, census_tract, text, lat, lon, census_tract, radius)
}

lexicon_function <- function(tweets_df, lexicon) {
  copy_tweets <- tweets_df %>% select(screen_name, text)
  tweet_text <- tweets_df %>% select(screen_name, text)
  all_words_name = paste0("all_" , enexpr(lexicon), "_words")
  all_values_name = paste0("all_", enexpr(lexicon), "_values")
  
  copy_tweets <- copy_tweets %>%
    unnest_tokens(word, text) %>% #Unnesting the words from the twitter text 
    inner_join(lexicon) %>% #Joining words from tweets with afinn lexicon and getting score for word
    group_by(screen_name) %>% #%>% #sorting the values by name
    mutate(x1 = paste0(word, collapse = ", "),#concatenating values from words
           x2 = paste0(value, collapse = ", ")) %>% #concatenating values from lexicon scores
    select(-word, -value) #removing redundant columns
  
  copy_tweets[[all_words_name]] <- with(copy_tweets, x1)
  copy_tweets[[all_values_name]] <- with(copy_tweets, x2)
  
  copy_tweets <- copy_tweets %>% select(-x1, -x2)
  copy_tweets <- copy_tweets[!duplicated(copy_tweets$screen_name), ]
  
  tweets_df <- left_join(tweets_df, copy_tweets, by = "screen_name")
  #eliminating duplicate values
}


#FINAL SCORES-------------------------------------------------------------------
score <- function(tweets_df, lexicon) {
  tweets_df <- create_ID_variable(tweets_df)
  
  numbers <- strsplit(tweets_df[[paste0("all_", enexpr(lexicon), "_values")]], ", ") #puts data into usable format
  
  mean_list <- c()
  for (i in numbers) { #iterates through vector of numbers
    count = 0
    number_list <- c()
    len <- length(numbers[i])
    for (j in i) { #iterates through numbers within vector
      count = count + 1
      j <- as.numeric(j) #converts from string to numeric
      number_list <- c(number_list, j) #puts numbers from words in tweet into a list
      if (len == count){
        mean_num <- mean(number_list) #creates mean from list
        mean_list <- c(mean_list, mean_num) #adds mean (in order) to new list
        break
      }
    }
  }
  score = paste0(enexpr(lexicon), "_scores")
  #Converts the list of mean score values to dataframe
  sentiment_score <- data.frame(score = matrix(unlist(mean_list), nrow=length(mean_list), byrow=TRUE)) 
  sentiment_score$ID <- seq.int(nrow(sentiment_score)) #adds ID variable for data merging
  
  #Merges the sentiment_score and unnested_text dfs into final df
  tweets_df <- merge(sentiment_score, tweets_df)
  #correctly names score column
  tweets_df[[paste0(enexpr(lexicon), "_scores")]] <- with(tweets_df, score)
  tweets_df <- tweets_df %>% select(-score)
  
  #rounds final scores
  tweets_df[[paste0(enexpr(lexicon), "_scores")]] <- round(tweets_df[[paste0(enexpr(lexicon), "_scores")]], 2)
  return(tweets_df)
}

#SAVE CSV-----------------------------------------------------------------------
save_csv <- function(df) {
  #save to csv
  write_csv(df, paste0("output/", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"), append = TRUE)
}



#ANALYSES-----------------------------------------------------------------------
#test <- tweets

avg_daily_sentiment <- function(tweets_df, lexicon) {
  
  daily_sentiment <- data.frame() %>% 
    add_column(date = NA,
               average_sentiment = NA)
  
  
  current_date = format(Sys.time(), "%Y-%m-%d")
  
  tweets_df <- tweets_df %>%
    mutate(created_at = round_date(created_at, unit = "day"))
  
  tweets_df <- tweets_df %>%
    filter(created_at == as.Date(current_date))
  
  head(tweets_df)

  tweets_df = filter(tweets_df, created_at == current_date)
  
  
  daily_mean <- mean(tweets_df[[paste0(enexpr(lexicon), "_scores")]], na.rm = TRUE)
  print(daily_mean)
  
  new_row = c(current_date, daily_mean)
  
  daily_sentiment <- rbind(daily_sentiment, new_row)
  
  return(daily_sentiment)
}

#daily_sentiment <- avg_daily_sentiment(test, afinn_lexicon)

#view(daily_sentiment)








