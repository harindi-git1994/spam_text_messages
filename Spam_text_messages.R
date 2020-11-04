sms_data <- read.csv("C:\\Users\\yhari\\OneDrive\\Documents\\4. Github\\spam.csv")
class(sms_data)
str(sms_data)

sms_data$type <- factor(sms_data$type)
str(sms_data)
table(sms_data$type)

library(tm)

# Prepare corpus for the text data 
sms_corpous <- Corpus(VectorSource(sms_data$text))
sms_corpous <- tm_map(sms_corpous,function(x) iconv(enc2utf8(x),sub = 'byte'))

# Cleaning data (removing unwanted symbols)
corpus_clean <- tm_map(sms_corpous,tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)

# Do not run the plain Text Document
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
as.character(corpus_clean)

# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
sms_dtm <- DocumentTermMatrix(corpus_clean) 
class(sms_dtm)
sms_dtm

# if code at 25 shows any error run the code at line 24 first and proceed
as.character(sms_dtm)

# creating training and test datasets
sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5558, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5558, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5558]
  
# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

sms_dtm_train

# indicator features for frequent words
sms_dict <- findFreqTerms(sms_dtm_train, 20)
sms_dict
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_train
sms_test 

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

convert_counts

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)


##  Training a model on the data ----
library(e1071)
m1 <- naiveBayes(sms_train, sms_raw_train$type)
m1

View(sms_train)


##  Evaluating model performance ----
pv <- predict(m1, sms_test)
pv
library(gmodels)
CrossTable(sms_raw_test$type,pv)
(1197+146)/1389


# Accuracy 
mean(pv == sms_raw_test$type)
# 82.086 % 

1323/1389

