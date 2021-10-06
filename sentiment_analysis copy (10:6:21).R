library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(rtweet)
library(sentimentr)

#IMPORTING LEXICONS CODE--------------------------------------------------------
  #Loading in sentiment lexicon
  afinn_lexicon <- get_sentiments("afinn")
  
  #Importing valence lexicon
  valence_lexicon <- as.data.frame(lexicon::hash_valence_shifters)
  valence_lexicon <- valence_lexicon %>% mutate(y = as.numeric(y))
  
  polarity_lexicon = read_csv(file="/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/sentiment-functions/lexicon/custom/custom_lexicon.csv")
  #polarity_lexicon = read_csv(file="/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/sentiment-functions/lexicon/afinn/afinn_lexicon.csv")
  polarity_lexicon = polarity_lexicon %>% select(-lexicon)
  colnames(polarity_lexicon) = c("word", "value")
  colnames(valence_lexicon) = c("word", "value")
  valence_lexicon = valence_lexicon %>% anti_join(polarity_lexicon, by="word")



#TWITTER CODE-------------------------------------------------------------------
#Authenticate via access token
token <- create_token(
  app = "query_twitter_app",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#Check to see if the token is loaded
identical(token, get_token())

#Streaming the tweets
#stream the tweets from each location and pull the average sentiment from each day/location

create_ID_variable <- function(df){
  df$ID <- seq.int(nrow(df)) #gets ID variable for unnested_text dataframe
  return(df)
}

#CREATING RANDOM TWEETS DATAFRAME-----------------------------------------------
#Formatting Coordinates from csv file and running search_tweet with formatted coordinates
formatting_location_df <- function(locations_csv){
  location_df <- church_and_school_locations %>% 
    select(lat, lon, census_tract) %>% 
    add_column(radius = "5mi")
}


formatting_coordinates <- function(locations_csv){
  location_df <- formatting_location_df(locations_csv) 
  location_df <- create_ID_variable(location_df)
  #pulls lat/lon from csv file
  for(i in location_df){
    lon <- location_df$lon
    lat <- location_df$lat
    census_tract <- location_df$census_tract
    radius <- location_df$radius
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
  location_df <- merge(location_df, coords_formatted, by = "ID")
  location_df <- location_df %>% select(-ID)
  return(location_df)
}

#pastes formatted lat/lon/radius into search_tweet function
search_tweet <- function(formatted_df) {
  single_search_df <- data.frame()
  every_search_df <- data.frame()
  formatted_coordinate_df <- data.frame(formatted_locations$coords_formatted)
  n = 200
  for (i in formatted_coordinate_df$formatted_locations.coords_formatted) {
    print(i)
    try ({
      single_search_df <- search_tweets("lang:en", geocode = i, n = n) #add try error loop in case we exceed rate limit
      single_search_df <- single_search_df %>% #adds coords_formatted column to search_tweet column
        add_column(coords_formatted = i)
      single_search_df <- left_join(single_search_df, formatted_df, by = "coords_formatted") #left joins location data on coord_formatted column
      every_search_df <- rbind(single_search_df, every_search_df) #combines this search with the previous searches
    })
    
  }
  every_search_df <- every_search_df[!duplicated(every_search_df$screen_name), ] #removes duplicates due to large radius
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
# simple_random_tweets <- random_tweets %>% select(screen_name, text)
# simple_random_tweets$afinn_sentiment = unlist(lapply(simple_random_tweets$text, score_sentiment_afinn)) # different bounds than nrc and bing
# simple_random_tweets$custom_sentiment = unlist(lapply(simple_random_tweets$text, score_domain_specific_sentiment))
# simple_random_tweets <- simple_random_tweets %>% na_if (0) # -1 to 1
# View(simple_random_tweets)

#CLEANING TWITTER TEXT----------------------------------------------------------
cleaning_text <- function(uncleaned_text) {
  #text column should be named 'text' in dataframe
  uncleaned_text$text = tolower(uncleaned_text$text)
  uncleaned_text$text = gsub("[^[:alnum:] ]", "", uncleaned_text$text)
  return(uncleaned_text)
}

#AFINN LEXICON DATAFRAME--------------------------------------------------------
afinn_function <- function(tweets) {
  cleaning_text(random_tweets)
  afinn <- tweets %>%
    select(screen_name, created_at, census_tract, text, lat, lon, census_tract, radius) %>%
    unnest_tokens(word, text) %>% #Unnesting the words from the twitter text and creating new df
    inner_join(afinn_lexicon) %>% #Joining words from tweets with afinn lexicon and getting score for word
    group_by(screen_name) %>% #sorting the values by name
    mutate(all_afinn_words = paste0(word, collapse = ", "),#concatenating values from words
           all_afinn_values = paste0(value, collapse = ", ")) %>% #concatenating values from lexicon scores
    select(-word, -value) #removing redundant columns
   #eliminating duplicate values
}


#CUSTOM LEXICON DATAFRAME-------------------------------------------------------
custom_function <- function(tweets) {
  unnested_text_custom <- tweets %>% 
    select(screen_name, text) %>%
    unnest_tokens(word, text) %>% 
    inner_join(polarity_lexicon, by = "word") %>% 
    group_by(screen_name) %>% 
    mutate(all_custom_words = paste0(word, collapse = ", "),
           all_custom_values = paste0(value, collapse = ", ")) %>% 
    select(-word, -value) 
}


#FINAL AFINN SCORE--------------------------------------------------------------
lexicon_score <- function(lexicon_values) {
  unnested_text_afinn <- create_ID_variable(unnested_text_afinn)
  numbers <- strsplit(lexicon_values$all_afinn_values, ", ") #puts data into usable format
  
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
        #print(mean_num)
        break
      }
    }
  }
  #Converts the list of mean score values to dataframe
  sentiment_score <- data.frame(score = matrix(unlist(mean_list), nrow=length(mean_list), byrow=TRUE)) 
  sentiment_score$ID <- seq.int(nrow(sentiment_score)) #adds ID variable for data merging
  #Merges the sentiment_score and unnested_text dfs into final df
  merged_scores <- merge(sentiment_score, unnested_text_afinn)
  return(merged_scores)
}

#CONFIGURING FINAL DATAFRAME----------------------------------------------------
clean_and_mutate_final_df <- function(df){
  #Merging unnested_text_custom with final scores dataframe
  df <- merge(df, unnested_text_custom, by = "screen_name")
  
  df <- df %>%  mutate(afinn_score = round(df$score, 2)) %>% #rounding it to make it easier to look at
    select(-score, -ID)
  
  #Adding the scores for the custom sentiment to the final scores from afinn lexicon
  df$custom_score <- round(df$custom_score, 2)
  
  #removing duplicate values due to overlay from screen_name and created_at
  df <- df[!duplicated(df$screen_name), ]
}


#FINAL SCORE DF CSV-------------------------------------------------------------
#Writes final_score dataframe to csv file and saves to desktop
#write.csv(final_scores, paste0("/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/sentiment_analysis_output/", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))

#CORRELATION PLOT OF LEXICON SCORES---------------------------------------------
# (cor(final_scores$afinn_score, final_scores$custom_score, use = "pairwise.complete.obs"))
# ggplot(final_scores, aes(x = afinn_score, y = custom_score)) +
#   geom_point()
# 
# final_scores <- final_scores %>% filter(final_scores$afinn_score != 0,
#                                         final_scores$custom_score == 0)
  

#TO-DO:
#Add ggplot so we can visually determine dates and locations of tweets
#add list of valence words that were found similar to all_afinn_words in final_scores

