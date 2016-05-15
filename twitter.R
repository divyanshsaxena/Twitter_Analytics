#Install and load the twitter package
install.packages('twitteR')
library(twitteR)

#After generating the keys from  twitter application page, store them in following variables
consumer_key <- 'XpWA4TpYQnuiOpe4OxiQYybaU'
consumer_secret <- 'SyWkTILXBLbskApeAZ1PAhpkwGvELn8eMnsZTpIpX628rCImhg'
access_token <- '3299049666-x4R0zlZmaS1xTQ8ZEKhCJYWiczUHQv6v7ZV8gYe'
access_secret <- 'LLR5DvJlDJRysT3IxzONdqMy3V1Z3NwsA53tgtOBYG4zK'

#Establishing a connection with twitter
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

#Fetching tweets on ODD_EVEN rule 
odd_even <- searchTwitter('ODD-EVEN',lang = 'en',n=100, resultType = 'recent')
#Here, n is the number of tweets to be fetched
#First parameter value is the string that is matched with the hash tags

#Cleaning the tweets
odd_even.text <- sapply(odd_even, function(x) x$getText())
tweets_corpus <- Corpus(VectorSource(odd_even.text)) #Create Corpus
tweets_corpus <- tm_map(tweets_corpus, PlainTextDocument) #Convert Corpus to Plain Text Document
tweets_corpus <- tm_map(tweets_corpus, removePunctuation) #Remove Punctuation
tweets_corpus <- tm_map(tweets_corpus, removeWords, c('IPL', '2016', stopwords('english'))) #Remove StopWords
tweets_corpus <- tm_map(tweets_corpus, stemDocument) #Perform Stemming of Words
tweet_words <- clean_text(odd_even.text)
#The above code gives the tweet in a clean format that can be used for analytics

#Generating the word cloud from the tweets
#Installing and Loading the required packages
install.packages('tm')
install.packages('wordcloud')
install.packages('RColorBrewer')
library('tm')
library(wordcloud)
library(RColorBrewer)

#Word Cloud Code
wordcloud(tweet_words, max.words = 5000, random.order = FALSE, colors=brewer.pal(8, "Dark2")) #Plot WordCloud