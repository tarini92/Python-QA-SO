rm(list = ls())

### load text mining libraries
library(tm)
library(dplyr)
library(readr)
library(lubridate)
library(SnowballC)
library(quanteda)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(tictoc)
library(topicmodels)
library(lda)

setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

questions <- read_csv('Questions.csv')
#answers <- read_csv('Answers.csv')

### filtering questions and answers of year 2016
questions_2016 <- filter(questions, year(CreationDate) == "2016")
questions_2016_text <- select(questions_2016, Title, Body)

#answers_2016 <- filter(answers, year(CreationDate) == "2016")
#answers_2016_text <- select(answers_2016, Body)

questions_2016_text$Title <- iconv(questions_2016_text$Title, to='ASCII', sub='byte')
questions_2016_text$Body <- iconv(questions_2016_text$Body, to='ASCII', sub='byte')

### Removing all html data frames from the dataset
questions_2016_text$Body <- gsub("<[^>]+>", "", questions_2016_text$Body)
questions_2016_text$Title <- gsub("[[:punct:]]", " ", questions_2016_text$Title)
questions_2016_text$Body <- gsub("[[:punct:]]", " ", questions_2016_text$Body)

### Forming a corpus from merged data frame, treating each row as a single document (done with DataframSource())
corpus_questions_2016 <- Corpus(DataframeSource(questions_2016_text))

### Inspect the corpus
#corpus_questions_2016
#writeLines(as.character(corpus_questions_2016[[1]]))

### Removing the numbers from the corpus
corpus_questions_2016 <- tm_map(corpus_questions_2016, removeNumbers)
writeLines(as.character(corpus_questions_2016[[1]]))

### Removing stop words using standard list of tm library
corpus_questions_2016 <- tm_map(corpus_questions_2016, removeWords, stopwords("english"))
writeLines(as.character(corpus_questions_2016[[1]]))

corpus_questions_2016 <- tm_map(corpus_questions_2016, content_transformer(tolower))
writeLines(as.character(corpus_questions_2016[[10]]))

### stripping white space
corpus_questions_2016 <- tm_map(corpus_questions_2016, stripWhitespace)
writeLines(as.character(corpus_questions_2016[[1]]))

### Stemming the document
corpus_questions_2016 <- tm_map(corpus_questions_2016, stemDocument)
writeLines(as.character(corpus_questions_2016[[10]]))

### Document Term Matrix 
dtmr_questions_2016 <-DocumentTermMatrix(corpus_questions_2016, control=list(wordLengths=c(5, 12), bounds = list(global = c(100,10000))))
dtmr_questions_2016

write(colnames(dtmr_questions_2016), file='colnames.txt')

### Calculate cumulative frequencies of words across documents and sort
freq <- colSums(as.matrix(dtmr_questions_2016))

length(freq)

ordr <- order(freq, decreasing=TRUE)

freq[head(ordr)]
freq[tail(ordr)]

### Find list of words occurring 2000 times in the entire corpus
### Result is ordered alphabetically not by frequency
findFreqTerms(dtmr_questions_2016, lowfreq=2000)

### Find correlation between differet words i.e. co-occurrence of two words where third parameter
### number between 0 and 1 that serves as a lower bound for  the strength of correlation
### between the  search and result terms
findAssocs(dtmr_questions_2016, "webdriv", 0.6)
findAssocs(dtmr_questions_2016, "wrapper", 0.6)
findAssocs(dtmr_questions_2016, "virtualenv", 0.5)
findAssocs(dtmr_questions_2016, "tensorflow", 0.5)

### Find top 50 terms and their associations - TRY THIS

### Graph Visualizations- term Occurrence Histogram
wf_questions_2016 <- data.frame(term = names(freq), occurrences=freq)
p <- ggplot(subset(wf_questions_2016, freq > 10000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))
p

### Constructing a word cloud
# setting the same seed each time ensures consistent look across clouds
set.seed(42)
# Limit words by specifying minimum frequency
wordcloud(names(freq), freq, min.freq = 1000, colors=brewer.pal(8, "Dark2"))


### To Term's Frequency Inverse Document Frequency (tf-idf) matrix
### Convert the dtm into one row per token(term), per document (tidy) dataset
tidy_dtm_questions_2016 <- tidy(dtmr_questions_2016)
tidy_dtm_questions_2016

### Computing the tfidf of questions_2016
tfidf_questions_2016 <- bind_tf_idf(tidy_dtm_questions_2016, term, document, count)
#tfidf_questions_2016

### Arrange tfidf table in decreasing order of tf_idf to get the important words
tfidf_questions_2016 %>% arrange(desc(tf_idf))

### Writing sorted frquencies matrix into a csv file
write.csv(freq[ordr], "word_freq.csv")



