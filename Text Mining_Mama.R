#Loading required packages

library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
install.packages('wordcloud2')
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(devtools)

#IMDB Reviews: Mama

aurl <- "https://www.imdb.com/title/tt2023587/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
#550

setwd("C:\\Users\\singh\\Documents\\IMDB_Mama")
write.table(IMDB_reviews,"Mama.txt",row.names = F)
Mama <- read.delim('Mama.txt')
str(Mama)
View(Mama)

# Build Corpus and DTM/TDM
corpus <- Mama[-1,]
head(corpus)
class(corpus)
corpus <- as.factor(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)

inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  
w <- subset(w, w>= 50)
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

#Sentiment Analysis
IMDB_reviews <- read.delim('Mama.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)
#character

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
get_nrc_sentiment('splendid')
get_nrc_sentiment('no words')

#Barplot

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        Mama')
