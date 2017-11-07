# read the Sentiment data from txt file from http://archive.ics.uci.edu/ml/datasets/Sentiment+Labelled+Sentences

sentiment_raw <- read.table("amazon_cells_labelled.txt", 
               sep="\t",
               fill=TRUE
               #col.names=c("review", "sentiment") #review=text of the review Sentiment "0" for negative "1" positive 
)

#lets start cleaning up the data
str(sentiment_raw)
library(tm)
sentiment_corpus <- VCorpus(VectorSource(sentiment_raw$review))
print(sentiment_corpus)

# clean up the corpus using tm_map()
sentiment_corpus_clean <- tm_map(sentiment_corpus, content_transformer(tolower))

# show the difference between sms_corpus and corpus_clean
as.character(sentiment_corpus[[1]])
as.character(sentiment_corpus_clean[[1]])

sentiment_corpus_clean <- tm_map(sentiment_corpus_clean, removeNumbers) # remove numbers
sentiment_corpus_clean <- tm_map(sentiment_corpus_clean, removeWords, stopwords()) # remove stop words
sentiment_corpus_clean <- tm_map(sentiment_corpus_clean, removePunctuation) # remove punctuation
sentiment_corpus_clean <- tm_map(sentiment_corpus_clean, stripWhitespace) # eliminate unneeded whitespace


sentiment_dtm_train <- sms_dtm[1:4169, ]
sentiment_dtm_test  <- sms_dtm[4170:5559, ]

# also save the labels
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# check that the proportion of spam is similar
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

library(wordcloud)
wordcloud(sentiment_corpus_clean, min.freq = 50, random.order = FALSE)

# subset the training data into review and sentiment groups
review <- subset(sentiment_raw, type == "review")
sentiment  <- subset(sentiment_raw, type == "sentiment")
