source("/Users/pearsonames/Desktop/sentiment_analysis.R")

church_and_school_locations <- read.csv("/Users/pearsonames/Desktop/IRAD Digital Neighborhoods/datasets/churches_public_schools.csv", header = TRUE)

formatted_locations <- formatting_coordinates(church_and_school_locations) #formats church and school locations into usable format for search_tweet and saves location data to df

random_tweets <- search_tweet(formatted_locations) #runs search_tweet from the formatted_locations df and raves it to random_tweets df

unnested_text_afinn <- afinn_function(random_tweets) #unnests twitter text and joins AFINN lexicon, separates words by commas and joins values for each word in a vector

unnested_text_custom <- custom_function(random_tweets) #unnests twitter text and joins CUSTOM lexicon, separates words by commas and joins values for each word in a vector

scored_tweets <- lexicon_score(unnested_text_afinn) #scores the afinn values from each tweet

scored_tweets$custom_score = unlist(lapply(scored_tweets$all_afinn_words, score_domain_specific_sentiment)) #Adding the scores for the custom sentiment to the final scores from afinn lexicon

scored_tweets <- clean_and_mutate_final_df(scored_tweets) #cleans, rounds and removes duplicates again from final df

view(scored_tweets)



