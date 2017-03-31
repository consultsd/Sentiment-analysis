#Links referred to and Credits
#https://www.analyticsvidhya.com/blog/2017/03/measuring-audience-sentiments-about-movies-using-twitter-and-text-analytics/
#https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/
#https://themepacific.com/how-to-generate-api-key-consumer-token-access-key-for-twitter-oauth/994/


#Sentiment analysis using Twitter data

install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")

# Set API Keys
api_key <- "DDDDDDDDDDDDDD"
api_secret <- "DDDDDDDDDDDDDD"
access_token <- "DDDDDDDDDDDDDD"
access_token_secret <- "DDDDDDDDDDDDDD"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Grab latest tweets
tweets_baahu <- searchTwitter('#Baahubali2', n=20000)

# convert tweets to a data frame
tweets.df <- twListToDF(tweets_baahu)

write.csv(tweets.df, file = "C:/Users/Sharath P Dandamudi/Desktop/baahubali.csv")

#Loading the data
library(readr)
baahubali = read_csv("C:/Users/Sharath P Dandamudi/Desktop/baahubali.csv")


#loading libraries
library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library('DT')

#extracting relevant data
r1 = as.character(baahubali$text)

#removing Retweets
r1_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",r1)

#let's clean html links
r1_txt<-gsub("http[^[:blank:]]+","",r1_txt)

#let's remove people names
r1_txt<-gsub("@\\w+","",r1_txt)

#Data Preprocessing
set.seed(100)
sample = sample(r1, (length(r1_txt)))
corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, c(stopwords('english'),'bahubali2','baahubali2','bahubali','baahubali'))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corpus = tm_map(corpus, removeSpecialChars)

corpus <- tm_map(corpus, PlainTextDocument)

#Initiating library RWeka
library(RWeka)

#Creating 1 and 2 grams and creating a sparse matrix
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
dtm_up = DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))

freq_up <- colSums(as.matrix(dtm_up))


#Calculating Sentiments
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

DT::datatable(sent_pos_up)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"))

DT::datatable(sent_neg_up)

plot.new()
set.seed(100)
wordcloud(sent_neg_up$text,sent_neg_up$freq, min.freq=10,colors=brewer.pal(6,"Dark2"))


#Approach 2 - using the 'syuzhet' package
text = as.character(baahubali$text) 

##removing Retweets
some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
##let's clean html links
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
##let's remove people names
some_txt<-gsub("@\\w+","",some_txt)
##let's remove punctuations
some_txt<-gsub("[[:punct:]]"," ",some_txt)
##let's remove number (alphanumeric)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)

#visual
library(ggplot2) # Data visualization
library(syuzhet)
mysentiment<-get_nrc_sentiment((some_txt))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) + 1000
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional violence", ylab = "Score", main = "Twitter sentiment for Baahubali 2", sub = "", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(mysentiment)