#AMAzon - Samsung Galaxy S20 reviews

#Loading required packages
library(tm)
library(scales)
library(syuzhet)
library(rvest)
library(XML)
library(magrittr)
library(wordcloud2)
library(wordcloud)
library(lubridate)
library(dplyr)
library(tidytext)
library(RWeka)
library(rJava)

scrape_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.in/Samsung-Galaxy-Storage-Additional-Exchange/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) 
  
  # Review Title
  doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text() -> review_title
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text
  
  # Number of stars in review
  doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         page = page_num) %>% return()
}

ASIN <- 'B08444S68Q'

a <- scrape_amazon(ASIN,1)
b <- scrape_amazon(ASIN,2)
c <- scrape_amazon(ASIN,3)
d <- scrape_amazon(ASIN,4)

cc <- rbind(a,b,c,d)
View(as.data.frame(cc))


corpus <- cc$review_text
head(corpus)

corpus <- VCorpus(VectorSource(corpus))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
corpus <- tm_map(corpus, content_transformer(removeURL))

View(corpus)

S20_tdm <- TermDocumentMatrix(corpus)
S20_matrix <- as.matrix(S20_tdm)
View(S20_matrix)

S20_rowsum <- rowSums(S20_matrix)
View(S20_rowsum)

S20_tdm_sparse <- removeSparseTerms(S20_tdm, 0.99)

dim(S20_tdm_sparse)

m <- as.matrix(S20_tdm_sparse)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
View(d)


wordcloud(corpus, scale = c(5, 0.5),max.words = 150,random.order = FALSE, rot.per = 0.35,
          use.r.layout = FALSE, colors = brewer.pal(6,"Dark2"))


gg <- as.character(corpus)
emotions <- get_nrc_sentiment(gg)
head(emotions)

ss <- as.data.frame(colSums(emotions[,1:8]))
aa <- t(ss)
rownames(aa) <- "Column Sum"
barplot(aa, xlab = "emotion", ylab = "Count", main = "Emotion Count")

PN <- as.data.frame(colSums(emotions[,-c(1:8)]))
T_PN <- t(PN)
View(T_PN)
rownames(T_PN) <- "Column Sum"
barplot(T_PN, xlab = "emotion", ylab = "Count", main = "Emotion Count")




corpus <- cc$review_text

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

break_oo <- as.data.frame(strsplit(cc$review_star, " "))
View(break_oo)
colnames(break_oo) <- NULL
ratings <- t(break_oo[1,])
ratings <- as.numeric(ratings)

mean(ratings)
hist(ratings, col = 'blue')
