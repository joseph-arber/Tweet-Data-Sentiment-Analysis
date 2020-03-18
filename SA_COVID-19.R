rm(list = ls())

#Useful packages
library(tidyr)
library(tidyverse)
#Load the rtweet library
library(rtweet)
library(tm)
#These packages will be useful for later on in the analysis:
library(SentimentAnalysis)
library(syuzhet)
library(wordcloud)

library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
install.packages("tidytext")

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(knitr) #Create nicely formatted output tables
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams


#Authorize twitter API to extract tweet data 
#We are going to extract Saudi Arabia tweet data
tweets_COVID <- search_tweets("#COVID19", n = 1000, include_rts = TRUE, lang = "en") #search_tweetsfunction allows us to query tweet data

head(tweets_COVID)
names(tweets_COVID)

#We need to build a corpus:
library(tm)
corpus<-iconv(tweets_COVID$text, to = "utf-8-mac")

corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#We need to clean this data:
corpus<-tm_map(corpus, tolower)
inspect(corpus[1:5]) 

corpus<- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus<- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset<-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

cleanset<-tm_map(corpus, removeWords, c('covid', 'coronavirus', 'and','the'))


removeURL <- function(x) gsub('http[[:alnum:]]*', '',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

#Term document matrix to turn this data into structured data:
tdm <- TermDocumentMatrix(cleanset)
tdm 
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

#Library word cloud
library(wordcloud)
library(RColorBrewer)

#Word frequency
word_frequency <- sort(colSums(as.matrix(tdm)),
                       decreasing = TRUE)
df_frequency <- data.frame(word = names(word_frequency),
                           freq = word_frequency)
head(df_frequency)

#Simple word cloud
wordcloud(w$word,
          w$freq)

#Alternative way to produce word cloud
sort(rowSums(tdm), decreasing = TRUE)


set.seed(222)
wordcloud(words = names(tdm),
          freq = w,
          max.words = 150)



wordcloud(w$word,
          w$freq,
          max.words = 10, min.freq = 1,
          random.order = FALSE,
          family = "Helvatica", font = 3)

library(wordcloud2)


#Letter cloud:
letterCloud(w,
            word = "SA",
            size = 1)


library(topicmodels)
#Create a topic model with 5 topics
topicmodl_5 <- LDA(tdm, k = 5)

#Select and view the top 10 terms in the topic model
top_10terms <- terms(topicmodl_5,10)
top_10terms 

#Create a topic model with 4 topics
topicmodl_4 <- LDA(tdm, k = 4)

#Select and view the top 6 terms in the topic model
top_6terms <- terms(topicmodl_4, 6)
top_6terms 

#Sentiment Analysis:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)


tweets <- iconv(tweets_Saudi, to = 'utf-8-mac')

#Obtain sentiment scores:
sa.value <- get_nrc_sentiment(tweets)
#Provide score for every emotion:
view(sa.value)

get_nrc_sentiment('negative')

barplot(colSums(sa.value),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Saudi Arabia')




#load in the qdap library:
library(qdapRegex)

#Extract tweet text from the pre-loaded dataset
twt_txt <- tweets_COVID$text
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
head(twt_gsub)

#install.packages("tm")
library(tm)

twt_corpus <- twt_gsub %>% 
  VectorSource() %>% 
  Corpus() 
head(twt_corpus$content)

#Convert the corpus to lowercase
twt_corpus_lwr <- tm_map(twt_corpus, tolower) 

#View the corpus after converting to lowercase
head(twt_corpus_lwr$content)

twt_corpus<-tm_map(twt_corpus, removeWords, stopwords('english'))
inspect(twt_corpus[1:5])

twt_corpus<-tm_map(twt_corpus, removeWords, c('the', 'this', 'can'))

#install.packages("qdap", INSTALL_opts = "--no-multiarch")

#Removing custom stop words:
library(RColorBrewer)
library(wordcloud)
#Extract term frequencies for top 60 words and view output
termfreq  <- wordcloud(twt_corpus, max.words = 60, 
                       random.order = FALSE, colors = brewer.pal(8, "Dark2"))
termfreq


#Creating a document term matrix (DTM) from the tweet corpus above
dtm_COVID <- DocumentTermMatrix(twt_corpus)
dtm_COVID

#Lets find the sum of word counts in each document
rowTotals <- apply(dtm_COVID, 1, sum)
head(rowTotals)

#We then need to select rows with a row total greater than zero
dtm_COV_new <- dtm_COVID[rowTotals > 0, ]
dtm_COV_new

#install.packages("topicmodels")
library(topicmodels)

#Create a topic model with 5 topics
topicmodl_5 <- LDA(dtm_COV_new, k = 5)

#Select and view the top 10 terms in the topic model
top_10terms <- terms(topicmodl_5,10)
top_10terms 

#Create a topic model with 4 topics
topicmodl_4 <- LDA(dtm_COV_new, k = 4)

#Select and view the top 6 terms in the topic model
top_6terms <- terms(topicmodl_4, 6)
top_6terms 


#Sentiment Analysis 

library(syuzhet)


sa.value <- get_nrc_sentiment(tweets_COVID$text)
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

#SA plot
ggplot(data = sa.score, aes(x = sentiment, y = score,fill = sentiment)) + geom_bar(stat = "identity") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


sc_name <- table(tweets_COVID$screen_name)
head(sc_name)

# Sort the table in descending order of tweet counts
sc_name_sort <- sort(sc_name, decreasing = TRUE)
# View top 6 users and tweet frequencies
head(sc_name_sort)
names(sc_name_sort)#Look for unusual twitter handles or suspicous handles (possible bots?)

# Create a data frame of tweet text and retweet counts
rtwt <- tweets_COVID[,c("retweet_count","text")]
# Sort data frame based on descending order of retweet counts
rtwt_sort <- arrange(rtwt, desc(retweet_count))

#install.packages("data.table")
library(data.table)
#Unique function removes duplicate tweets from df
rtwt_unique <- unique(rtwt_sort, by = "text")
rtwt_unique
#Most popular tweets:
names(rtwt_unique)
head(rtwt_unique)


tweets_COV19<-search_tweets("#COVID19", n = 1000, include_rts = TRUE, lang = "en")


# Clean the data
text <- str_c(tweets_COV19$text, collapse = "")

library(qdapRegex)

# continue cleaning the text
text <- text %>%
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))                   # Final cleanup of other small changes

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

head(textCorpus)
# build wordcloud 
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud



