library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

rm(list=ls())

pmspeech <- readLines("pmgstspeech.txt")
pm_corpus <- Corpus(VectorSource(pmspeech))


#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
pm_corpus <- tm_map(pm_corpus, toSpace, "/")
pm_corpus <- tm_map(pm_corpus, toSpace, "@")
pm_corpus <- tm_map(pm_corpus, toSpace, "\\|")

pm_corpus <- tm_map(pm_corpus,PlainTextDocument)

# Convert the text to lower case
pm_corpus <- tm_map(pm_corpus, content_transformer(tolower))

# Remove numbers
pm_corpus <- tm_map(pm_corpus, removeNumbers)

# Remove english common stopwords
pm_corpus <- tm_map(pm_corpus, removeWords, stopwords("english"))

# Remove punctuations
pm_corpus <- tm_map(pm_corpus, removePunctuation)

# Eliminate extra white spaces
pm_corpus <- tm_map(pm_corpus, stripWhitespace)

# Text stemming - which reduces words to their root form
pm_corpus <- tm_map(pm_corpus, stemDocument)

# Build a term-document matrix
pmcorpus_dtm <- TermDocumentMatrix(pm_corpus)
pmcorpus_sparse <- removeSparseTerms(pmcorpus_dtm,0.99)
dtm_m <- as.matrix(pmcorpus_sparse)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)

# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 3, names.arg = dtm_d[1:10,]$word,
        col ="lightblue", main ="Top 10 most frequent words in speech",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word,freq = dtm_d$freq,random.order=FALSE, rot.per=0.35,
          scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(pmcorpus_dtm, terms = c("corrupt","tax"), corlimit = 0.25)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(pmspeech, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(pmspeech, method="bing")
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(pmspeech, method="afinn")
head(afinn_vector)
summary(afinn_vector)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(pmspeech)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:175]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:10,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
