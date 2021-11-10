source("sentiment_analysis.R")

church_and_school_locations <- read.csv("datasets/census_tracts_with_lat_lon.csv", header = TRUE)
census_demographic_data <- read.csv("datasets/census_tract_demographic_data.csv", header = TRUE)

#BUILDING TWEETS DF-------------------------------------------------------------
formatted_locations <- format_locations(church_and_school_locations) #formats church and school locations into usable format for search_tweet and saves location data to df

tweets <- search_tweet(formatted_locations) #runs search_tweet from the formatted_locations df and raves it to random_tweets df

tweets <- lexicon_function(tweets, afinn_lexicon) #unnests twitter text and joins AFINN lexicon, separates words by commas and joins values for each word in a vector

tweets <- lexicon_function(tweets, polarity_lexicon) #unnests twitter text and joins CUSTOM lexicon, separates words by commas and joins values for each word in a vector

tweets <- score(tweets, afinn_lexicon) #scores afinn tweets

tweets <- score(tweets, polarity_lexicon) #scores polarity tweets

save_csv(tweets) #appends tweets to csv file
average_daily_sentiment_csv(tweets) #saves daily average



#TWEETS ANALYSIS----------------------------------------------------------------
tweets_demographic <- left_join(tweets, census_demographic_data, by = "census_tract")





#To-do
#You could start analyses on the health outcomes, demographic info, census tract information etc to see if there are any correlations
#Possibly study happiness on different levels of the week to see if there is any correlations 
  #could also relate to the sunday phenomena