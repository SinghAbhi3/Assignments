#Loading required packages

#install.packages('devtools')
library(devtools)
#install.packages("twitteR")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")
#install.packages("httr")
library(httr)
#install.packages("base64enc")
library(base64enc)
#install.packages("httpuv")
library(httpuv)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("tm")
library(tm)
#install.packages("syuzhet")
library(syuzhet)
#install.packages("ggplot2")
library(ggplot2)

setup_twitter_oauth("9rMPg0fvNUG27YtnJlfkchJ9q", 
                              "mqh7mniVvQpBnVA42qe2GwYjuFBw2ajt10tYHDNIg9c54DQ0PP", 
                              "2263177111-avYpSgiShOJnTRRLiWNN1u7TV0rOQIjFZZK5B8Z",  
                              "C0IUNKTFDpCaw2Ts4Yd8sZK2YWRZ7PeAV6kCih6UqeEXM")  
Tweets <- userTimeline('SonuSood', n = 1000,includeRts = F, )
View(Tweets)
TweetsDF <- twListToDF(Tweets)
View(TweetsDF)

TweetsDF$text <- as.character(TweetsDF$text)
TweetsDF$text <- iconv(enc2utf8(TweetsDF$text), sub = "byte")
TweetsDF$text <- iconv(TweetsDF$text, to = "utf-8", sub = "")
TweetsDF$text <- gsub("@\\w*"," USER",   TweetsDF$text)
TweetsDF$text  <- gsub("http[[:alnum:][:punct:]]*"," ",   tolower(TweetsDF$text ))
TweetsDF$text  <- gsub("www[[:alnum:][:punct:]]*"," ",   tolower(TweetsDF$text ))
TweetsDF$text<-gsub("\\&\\w*;","", TweetsDF$text)
TweetsDF$text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', TweetsDF$text)
TweetsDF$text <- gsub("[^a-zA-Z0-9 ]","",TweetsDF$text)

twittercorpus <- Corpus(VectorSource(TweetsDF$text))
twittercorpus <- tm_map(twittercorpus, stemDocument)
twittercorpus <- tm_map(twittercorpus, removePunctuation)
twittercorpus <- tm_map(twittercorpus, removeWords, stopwords(kind = "en"))
twittercorpus <- tm_map(twittercorpus, stripWhitespace)

View(twittercorpus)

tdm <- TermDocumentMatrix(twittercorpus)
View(inspect(tdm[1:30, 1:50]))

tdm_2 <- removeSparseTerms(tdm, 0.99)

dim(tdm_2)

m <- as.matrix(tdm_2)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud(twittercorpus, scale = c(5, 0.5),max.words = 150,random.order = FALSE, rot.per = 0.35,
          use.r.layout = FALSE, colors = brewer.pal(6,"Dark2"))
emotions <- get_nrc_sentiment(TweetsDF$text)
head(emotions)

emotions <- get_nrc_sentiment(TweetsDF$text)
head(emotions)

ss <- as.data.frame(colSums(emotions[,1:8]))
aa <- t(ss)
rownames(aa) <- "Column Sum"
barplot(aa, xlab = "emotion", ylab = "Count", main = "Emotion Count")

tweet.text <- cbind(TweetsDF$text, emotions)
View(tweet.text)

View(tweet.text[,c(11:10)])

Totals <- data.frame(colSums(tweet.text[,c(11:10)]))
names(Totals) <- "score"

Totals <- cbind("emotions" = rownames(Totals), Totals)
rownames(Totals) <- NULL

ggplot(Totals, aes(x = emotions, y = score)) +
  geom_bar(aes(fill = emotions), stat = "identity", position = "dodge", width = 1) +
  xlab("sentiment") + ylab("sentiment Scores") + ggtitle("Sentiment Scores for All Tweets")

corpus <- TweetsDF$text

aa <- VCorpus(VectorSource(corpus))
aa <- tm_map(aa,content_transformer(tolower))
aa <- tm_map(aa, removePunctuation)
aa <- tm_map(aa, stripWhitespace)
aa <- tm_map(aa, removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
aa <- tm_map(aa, content_transformer(removeURL))

bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram <- TermDocumentMatrix(aa, control = list(tokenize = bigramTokenizer))
View(as.matrix(tdm.bigram))

freq <- sort(rowSums(as.matrix(tdm.bigram)), decreasing = TRUE)
freq.df <- data.frame(word=names(freq), freq = freq)
View(freq.df)


trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram <- TermDocumentMatrix(aa, control = list(tokenize = trigramTokenizer))
View(as.matrix(tdm.trigram))

freq1 <- sort(rowSums(as.matrix(tdm.trigram)), decreasing = TRUE)
freq.df2 <- data.frame(word=names(freq1), freq = freq1)
View(freq.df2)
