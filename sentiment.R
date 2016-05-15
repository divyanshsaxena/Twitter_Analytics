#Installing and Loading required packages
install.packages('devtools')
library(devtools)
install_github('okugami79/sentiment140', force = TRUE)
install.packages('plyr')
install.packages('ggplot2')
install.packages('twitteR')
install.packages('wordcloud')
install.packages('tm')
install.packages('RColorBrewer')

#Creating a function to analyse the sentiments of a tweet
sentiment_twitter <- function(searchterm,i)
{
  #Loading all packages
  library(twitteR)
  library(wordcloud)
  library(tm)
  library(RColorBrewer)
  library(sentiment)
  library(plyr)
  library(ggplot2)
  
  #Getting the access keys from twitter application
  consumer_key <- 'XpWA4TpYQnuiOpe4OxiQYybaU'
  consumer_secret <- 'SyWkTILXBLbskApeAZ1PAhpkwGvELn8eMnsZTpIpX628rCImhg'
  access_token<-'3299049666-x4R0zlZmaS1xTQ8ZEKhCJYWiczUHQv6v7ZV8gYe'
  access_secret <- 'LLR5DvJlDJRysT3IxzONdqMy3V1Z3NwsA53tgtOBYG4zK'
  
  #Authorizing the user
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
  #Getting tweets where i is number of tweets
  tweet_to_score <- searchTwitter(searchterm, n=i, lang="en", resultType="recent")
  tweet_text <- sapply(tweet_to_score, function(x) x$getText())
  
  #Classifying the tweets to different sentiments
  tweet_clean <- clean_text(tweet_text)
  emotion_class <- classify_emotion(tweet_clean, algorithm = "bayes", prior = 1)
  emotion <- emotion_class[,7]
  emotion[is.na(emotion)] = "unknown"
  polarity_class <- classify_polarity(tweet_clean, algorithm = "bayes")
  polarity = polarity_class[,4]
  sent_df = data.frame(text=tweet_clean, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
  sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  head(sent_df, n=5)
  ggplot(sent_df, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    labs(x="emotion categories", y="number of tweets") +
    ggtitle("Sentiment Analysis of Twitter") +
    theme(plot.title = element_text(size=12, face="bold"))
}

#Cleaning the tweets
clean_text = function(x)
{
  x = gsub("rt", "", x) # remove rt
  x = gsub("RT", "", x) # remove RT
  x = gsub("@\\w+", "", x) # remove at
  x = gsub("[[:punct:]]", "", x) # remove punctuation
  x = gsub("[[:digit:]]", "", x) # remove numbers
  x = gsub("http\\w+", "", x)  # remove links http
  x = gsub("[ |\t]{2,}", "", x) # remove tabs
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  try.error = function(z)
  {
    y = NA
    try_error = tryCatch(tolower(z), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(z)
    return(y)
  }
  x = sapply(x, try.error)
  return(x)
}


sentiment_twitter("IPL2016", 2000)
