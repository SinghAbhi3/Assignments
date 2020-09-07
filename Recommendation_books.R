#Setting working directory

setwd('C:/Users/singh/Documents/Recommendation System')

#Loading required libraried

library(recommenderlab)
library(tidyverse)
library(Matrix)
library(kableExtra)
library(gridExtra)

books_rating <- read.csv(file.choose(), header =T)
books_rating$book_ratings <- as.factor(books_rating$book_ratings)
books_rating$book_id <- as.factor(books_rating$book_id)


bmatrix <- as(book_rating, "realRatingMatrix")


#item similiarity
sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")

# users who rated at least 100 books and books rated at least 100 times
bmatrix <- bmatrix[rowCounts(bmatrix) > 150, colCounts(bmatrix) > 300]
bmatrix

tbl_ratings <- as.data.frame(table(as.vector(bmatrix@data)))
tbl_ratings


tbl_ratings <- tbl_ratings[-1,] 
ggplot(tbl_ratings, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle("Distribution of Book Ratings"

rated_count <- colCounts(bmatrix)
read_book <- data.frame(
book_id = names(rated_count),
read = rated_count
)


top_books <- 
  inner_join(read_book, book_titles, by = "book_id") %>% 
  arrange(desc(read)) %>% 
  select(-book_id) %>% 
  head(10) %>% 
  ggplot(aes(x = title, y = read)) + geom_bar(stat = "identity", fill = "lightblue") + geom_text(aes(label=read), vjust=-0.3, size=3.5) + ggtitle("Top 10 Rated Books") +  coord_flip()


avg_book_ratings <- data.frame("avg_rating" = colMeans(bmatrix)) %>% 
  ggplot(aes(x = avg_rating)) + 
  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Distribution of Average Ratings for Books")
avg_book_ratings

#Item based Prediction

Imodel <- Recommender(data = books_train, method = "IBCF")
Imodel

Ipredict <- predict(Imodel, newdata = books_test, n = 5) %>% list()
Ipredict

item_recc_books <- function(i){
  p <- Ipredict[[1]]@items[[i]]
  p <- data.frame("guess" = as.factor(p))
  p <- inner_join(p, book_titles, by = c("guess" = "book_id")) %>% select(title)
  r <- data.frame("name" = as.factor(i))
  r <- inner_join(r, book_titles, by = c("name" = "book_id")) %>% select(title)
  print(paste("Books similar to --", r))
  return(as.list(p))
}
item_recc_books(5); item_recc_books(200); item_recc_books(18)

"Umodel <- Recommender(data = books_train, method = ""UBCF"")
Umodel"

Upredict <- predict(Umodel, newdata = books_test, n = 5) %>% list()
Upredict

# function created to display recommended similar books to users
user_recc_books <- function(u){
  p <- Upredict[[1]]@items[[u]]
  p <- data.frame("guess" = as.factor(p))
  p <- inner_join(p, book_titles, by = c("guess" = "book_id")) %>% select(title)
  r <- data.frame("name" = as.factor(u))
  r <- inner_join(r, book_titles, by = c("name" = "book_id")) %>% select(title)
  print(paste("Books similar to --", r, "-- based on similar users"))
  return(as.list(p))
}
user_recc_books(5); user_recc_books(200); user_recc_books(18)


                                                          
                                                                                                  
                                                                                                  