#Setting working directory
setwd('C:\\Users\\singh\\Documents\\SPAM-HAM-NaiveBayes')


#Reading file
spam_ham <- read.csv('sms_raw_NB.csv')
str(spam_ham)
spam_ham$type <- as.factor(spam_ham$type)

install.packages("gmodels")
library(gmodels)
library(e1071)
library(caret)
library(MASS)
library(dplyr)
library(caTools)

library(tm)
#install.packages('SnowballC')
library(SnowballC)

table(spam_ham$type)
# ham spam 
#4812  747 

sms_corpous<-VCorpus(VectorSource(spam_ham$text))
sms_corpous
class(sms_corpous)

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(sms_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
class(corpus_clean)

sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)


# creating training and test datasets
sms_raw_train <- spam_ham[1:4169, ]
sms_raw_test  <- spam_ham[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]


## check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
#ham      spam 
#0.8647158 0.1352842 

prop.table(table(sms_raw_test$type))
#     ham      spam 
#0.8683453 0.1316547 

# indicator features for frequent words
# if the word has been referred to 5 times or more
sms_dict<-findFreqTerms(sms_dtm, 5)

#Applying this particular dictionary of words to training and testing data.
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

temp <- as.data.frame(as.matrix(sms_train))
View(temp)
dim(sms_train)
dim(sms_test)

## Create a custom function to show that if a specific word as been used more than once.
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}


# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##Evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))


confusionMatrix(sms_test_pred,sms_raw_test$type)
#Accuracy : 0.977 


