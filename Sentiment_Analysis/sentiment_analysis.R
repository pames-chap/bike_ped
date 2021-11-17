library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(topicmodels)
library(tm)
library(SnowballC)
library(dplyr)
library(rlang)
library(ggplot2)
library(tibble)
library(stringr)
library(rtweet)
library(sentimentr)
library(lubridate)
library(rJava)
library(qdap)

#IMPORTING LEXICONS CODE--------------------------------------------------------
  #Loading in sentiment lexicon
  afinn_lexicon <- get_sentiments("afinn")
  #bing_lexicon <- get_sentiments("bing")
  #nrc_lexicon <- get_sentiments("nrc")
  
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

#CLEANING TWITTER TEXT--------------------------------------------------------
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
save_csv <- function(tweets_df) {
  #save to csv
  defaultW <- getOption("warn") 
  options(warn = -1) 
  write.table(tweets_df, file = paste0("output/", format(Sys.time(), "%Y-%m-%d"), ".csv"), sep = ",", append = TRUE, quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  options(warn = defaultW)
}



#ANALYSES-----------------------------------------------------------------------
average_daily_sentiment_csv <- function(tweets_df) {
  
  daily_sentiment <- data.frame() %>% 
    add_column(date = NA,
               average_afinn_sentiment = NA,
               average_polarity_sentiment = NA)
  
  current_date = format(Sys.time(), "%Y-%m-%d")
  
  average_afinn_sentiment <- round(mean(tweets_df$afinn_lexicon_scores, na.rm = TRUE), 3)
  average_polarity_sentiment <- round(mean(tweets_df$polarity_lexicon_scores, na.rm = TRUE), 3)
  
  new_row = c(current_date, average_afinn_sentiment, average_polarity_sentiment)
  
  
  daily_sentiment[nrow(daily_sentiment)+1,] <- new_row
  
  defaultW <- getOption("warn") 
  options(warn = -1) 
  write.table(daily_sentiment, file = "output/average_daily_sentiments.csv", sep = ",", append = TRUE, quote = FALSE,
              col.names = FALSE, row.names = FALSE)
  options(warn = defaultW)
  
}

test <- tweets

clean_terms <- function(terms) {
  #text column should be named 'text' in dataframe
  terms = tolower(terms)
  terms = gsub("[^[:alnum:] ]", "", terms)
  return(terms)
}

clean_stop_words_LDA <- function(input_text){
  
  Corpus <- Corpus(VectorSource(input_text)) 
  DTM <- DocumentTermMatrix(Corpus)
  
  # convert the document term matrix to a tidytext corpus
  DTM_tidy <- tidy(DTM)
  # I'm going to add my own custom stop words that I don't think will be
  # very informative in hotel reviews
  #custom_stop_words <- tibble(word = c())
  
  # remove stopwords
  DTM_tidy_cleaned <- DTM_tidy %>% # take our tidy dtm and...
    anti_join(stop_words, by = c("term" = "word"))# %>% # remove English stopwords and...
    #anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords
  
  
  DTM_tidy_cleaned <- DTM_tidy_cleaned %>% 
    mutate(stem = wordStem(term))
  
  # reconstruct cleaned documents (so that each word shows up the correct number of times)
  # reconstruct our documents
  cleaned_documents <- DTM_tidy_cleaned %>%
    group_by(document) %>% 
    mutate(terms = toString(rep(stem, count))) %>%
    select(document, terms) %>%
    unique()

}

cleaned_input_text <- clean_stop_words_LDA(test$text)

# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{ 
  
  #cleaned_input_text <- clean_stop_words_LDA(input_text)
  
  Corpus <- Corpus(VectorSource(cleaned_input_text$terms)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta)  # arrange words in descending informativeness
   
  top_terms$term <- clean_terms(top_terms$term)
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }

  else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# plot top ten terms in the hotel reviews by topic
top_terms_by_topic_LDA(cleaned_input_text$terms, number_of_topics = 3) #NEED TO DETERMINE CORRECT NUMBER OF TOPICS


#inner joining cleaned text with cenus_tract
cleaned_input_text$document <- as.integer(cleaned_input_text$document)
cleaned_text_w_census_tract <- inner_join(cleaned_input_text, test, by = c("document" = "ID"))
cleaned_text_w_census_tract$census_tract <- as.character(cleaned_text_w_census_tract$census_tract)
cleaned_text_w_census_tract <- as.data.frame(cleaned_text_w_census_tract)

top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()

  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(5) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}

top_terms_by_topic_tfidf(text_df = cleaned_text_w_census_tract, # dataframe
                         text_column = terms, # column with text
                         group_column = census_tract, # column with topic label
                         plot = T) # return a plot


#Use tf-idf to and group by census tract
  #only inlcude census tracts with over 5 terms
  #try to remove ties from top_n function
  #use usernames from dataframe to remove them form tf-idf
  #normalize x-axis on tf-idf

#use diabetes to determine the topics (do certain census tracts have with higher diabetes rates have different sentiment scores)
#try to see if there's anything related to food deserts in different census tracts
#look at different key words for groups (vaccines, boosters, etc. )

#building daily average sentiment scores
test <- tweets

average_census_tract_sentiment <- function(tweets_df){
  tweets_df <- tweets_df %>% group_by(census_tract)
  
  census_tract_sentiment <- tweets_df %>%
    group_by(census_tract) %>%
    summarise_at(vars(afinn_lexicon_scores, polarity_lexicon_scores), list(name = mean), na.rm = TRUE)
  
  census_tract_sentiment$date <- format(Sys.time(), "%Y-%m-%d")
  
  view(census_tract_sentiment)
}


avg_census_tract <- average_census_tract_sentiment(test)
view(avg_census_tract)

