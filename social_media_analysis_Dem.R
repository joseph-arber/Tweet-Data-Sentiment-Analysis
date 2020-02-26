#Social Media Analysis: Twitter Data 
# Aim: To understand how twitter has percieved the democratic primary elections so far. Specifically the performance of Sanders and Buttigieg 

#Useful packages
library(tidyr)
library(tidyverse)
library(ggplot2)
#Load the rtweet library
library(rtweet)
library(tm)

#These packages will be useful for later on in the analysis:
library(SentimentAnalysis)
library(syuzhet)
library(wordcloud)
library(igraph)


#Authorize twitter API to extract tweet data 
#We are going to extract coronavirus tweet data
tweets_nevada <- search_tweets("#nevadadebate", n = 1000, include_rts = TRUE, lang = "en") #search_tweets function allows us to query tweet data

#Take a look at the output:
head(tweets_nevada,4)
tail(tweets_nevada)

#Column names
names(tweets_nevada)

#Extract tweets from a specific user...
gt_donald <- get_timeline("@realDonaldTrump", n = 3200)

#Take a look at the ouput:
head(gt_donald,4)
tail(gt_donald)

#Tweets are returned a JSON objects. The twitter JSON is converted into an R dataframe, of which its components can be used to analyse particular phenomena. Attributes and values converted to column names and values

#Tweeting about Bernie Sanders
tweets_df <- search_tweets("#sanders")

#View the columns (variable names):
names(tweets_df)

#Exploring components, #screen_name to understand user interest, #followers_count to compare social media inuence and #retweet_count and text to identify popular tweets
# Create a table of users and tweet counts for the topic, #Brexit
sc_name <- table(tweets_df$screen_name)
head(sc_name)

# Sort the table in descending order of tweet counts
sc_name_sort <- sort(sc_name, decreasing = TRUE)
# View top 6 users and tweet frequencies
head(sc_name_sort)
names(sc_name_sort)#Look for unusual twitter handles or suspicous handles (possible bots?)


#Get geographical location tweet data
#Topics trending in London
gt_city <- get_trends("Newyork")

#View the first 6 columns
head(gt_city[,1:6])

#Aggregate the trends and tweet volumes
trend_df <- gt_city %>%
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume))

#Sort data frame on descending order of tweet volumes and print header
trend_df_sort <- arrange(trend_df, desc(tweet_vol))
head(trend_df_sort,10)


#Twitter data over time:
#Extract tweets on #walmart and exclude retweets
buttigieg_twts <- search_tweets("#buttigieg", n = 18000, include_rts = FALSE)

#View the output
head(buttigieg_twts)

#Time-series plot. To do this we use the "ts_plot" function
ts_plot(buttigieg_twts, by = "hours", color = "blue") #There is a spike in the time-series which can be explained by the most recent democrat debate which involved Buttigieg

#Compared to days:
ts_plot(buttigieg_twts, by = "days", color = "blue")

#Create a time series object for Tweets about Buttigieg at hourly intervals
buttigieg_ts <- ts_data(buttigieg_twts, by ='hour')

#Rename the two columns in the time series object
names(buttigieg_ts) <- c("time", "buttigieg_n")

#View the output
head(buttigieg_ts)

#Tweets about Sanders over time-series
sanders_twts <- search_tweets("#sanders", n = 18000, include_rts = FALSE)

#Create a time series object for Sanders at hourly intervals
sanders_ts <- ts_data(sanders_twts, by ='hour')

#Rename the two columns in the time series object
names(sanders_ts) <- c("time", "sanders_n")

#View the output
head(sanders_ts)

library(ggplot2)
#install.packages("reshape")
library(reshape)

#Merge the two time series objects and retain "time" column
merged_df <- merge(buttigieg_ts, sanders_ts, by = "time", all = TRUE)
head(merged_df)

#Stack the tweet frequency columns
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")

#View the output
head(melt_df)

#Visualizing tweets through time series analysis provides good insights on interest level
#Plot frequency of tweets on Sanders and Buttigieg
ggplot(data = melt_df, aes(x = time, y = value, col = variable))+
  geom_line(lwd = 0.8)


#Tweets about Warren over time-series
warren_twts <- search_tweets("#warren", n = 18000, include_rts = FALSE)

#Create a time series object for Warren at hourly intervals
warren_ts <- ts_data(warren_twts, by ='hour')

#Rename the two columns in the time series object
names(warren_ts) <- c("time", "warren_n")

#View the output
head(warren_ts)

#Merge the two time series objects and retain "time" column
merged2_df <- merge(buttigieg_ts, warren_ts, by = "time", all = TRUE)
head(merged2_df)

#Stack the tweet frequency columns
melt2_df <- melt(merged2_df, na.rm = TRUE, id.vars = "time")

#View the output
head(melt2_df)

#How do Warren and Buttigieg compare?
ggplot(data = melt2_df, aes(x = time, y = value, col = variable))+
  geom_line(lwd = 0.8) #Warren spiked at a similar point to Buttigieg but by much less.


#Text sentiment and network analysis using twitter data:

#It is important process tweet text and prepare a clean text corpus for analysis:
#install.packages("qdapRegex")
#load in the qdap library:
library(qdapRegex)

#Extract tweet text from the pre-loaded dataset
twt_txt <- buttigieg_twts$text
head(twt_txt)

#Remove URLs from the tweet text and view the output
twt_txt_url <- rm_twitter_url(twt_txt)
head(twt_txt_url)

#Replace special characters, punctuation, & numbers with spaces
twt_txt_chrs  <- gsub("[^A-Za-z]"," " , twt_txt_url)

#View text after replacing special characters, punctuation, & numbers
head(twt_txt_chrs)

twt_gsub <- twt_txt_chrs
#We can see that URLs have been removed and special characters, punctuation, & numbers have been replaced with additional spaces in the text

#Convert text in "twt_gsub" dataset to a text corpus and view output
twt_gsub <- twt_txt_chrs

#install.packages("tm")
library(tm)

twt_corpus <- twt_gsub %>% 
  VectorSource() %>% 
  Corpus() 
head(twt_corpus$content)

#Convert the corpus to lowercase
twt_corpus_lwr <- tm_map(twt_corpus, tolower) 

#View the corpus after converting to lowercase
head(twt_corpus_lwr$content) #Most of the tweets discuss the moderate politics of Buttigieg when compared to Sanders

#install.packages("qdap", INSTALL_opts = "--no-multiarch")

#Removing custom stop words:
library(RColorBrewer)
library(wordcloud)
#Extract term frequencies for top 60 words and view output
termfreq  <- wordcloud(twt_corpus, max.words = 60, 
                       random.order = FALSE, colors = brewer.pal(8, "Dark2"))


#Creating a document term matrix (DTM) from the tweet corpus above
dtm_democrat <- DocumentTermMatrix(twt_corpus_lwr)
dtm_democrat

#Lets find the sum of word counts in each document
rowTotals <- apply(dtm_democrat, 1, sum)
head(rowTotals)

#We then need to select rows with a row total greater than zero
dtm_dem_new <- dtm_democrat[rowTotals > 0, ]
dtm_dem_new

#install.packages("topicmodels")
library(topicmodels)

#Create a topic model with 5 topics
topicmodl_5 <- LDA(dtm_democrat, k = 5)

#Select and view the top 10 terms in the topic model
top_10terms <- terms(topicmodl_5,10)
top_10terms 

#Create a topic model with 4 topics
topicmodl_4 <- LDA(dtm_democrat, k = 4)

#Select and view the top 6 terms in the topic model
top_6terms <- terms(topicmodl_4, 6)
top_6terms 


#Sentiment Analysis 
library(syuzhet)

twts_buttigieg <- search_tweets("buttigieg", n = 5000,
                                lang = "en", include_rts = FALSE)

sa.value <- get_nrc_sentiment(twts_buttigieg$text)
view(sa.value)

#sum of the sentiment scores, extracts sum of SA as input
score <- colSums(sa.value[,])
#Convert to a data frame
score_df <- data.frame(score)
score_df

#Convert the row names into a 'sentiment' column:
sa.score <- cbind(sentiment = row.names(score_df),
                  score_df, row.names = NULL)
print(sa.score)

#Sentiment analysis score suggests that Buttigieg is viewed in a positive light on twitter. There are a number of words also associcated with trust...

#SA plot
ggplot(data = sa.score, aes(x = sentiment, y = score,fill = sentiment)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) #The plot visualizes the varying sentiments towards Buttigieg


#Network Analysis and Geolocation coordinates 
library(rtweet)
# Extract 18000 tweets on "#politics"
pol <- search_tweets("#buttigieg", n = 18000)

#Extract geolocation data
pol_coord <- lat_lng(pol)

head(pol_coord)

#Omit the NA values:
pol_geo <- na.omit(pol_coord[,c("lat", "lng")])
head(pol_geo)

#Load in the map function:
library(maps)
#Plot longitude and latitude values of tweets on US state map
map(database = "state", fill = TRUE, col = "light yellow")

#US map showing geo-location of #Buttigieg related tweets
with(pol_geo, points(lng, lat, pch = 20, cex = 1, col ='blue'))

#Plot longitude and latitude values of tweets on the world map
map(database = "world", fill = TRUE, col = "light yellow")
with(pol_geo, points(lng, lat, pch = 20, cex = 1, col = 'blue')) 

#We can see that Buttigieg and Sanders are trending across the US states:

