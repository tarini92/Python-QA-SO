rm(list = ls())

### load text mining libraries
library(tm)
library(dplyr)
library(readr)
library(lubridate)
library(quanteda)
library(tidytext)
library(tictoc)
library(topicmodels)
library(lda)
library(qdap)
library(RTextTools)
library(stringi)
library(LDAvis)
library(servr)

setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

questions <- read_csv('Questions.csv')

### filtering questions and answers of year 2008
questions_2008 <- filter(questions, year(CreationDate) == "2008")

### Merging Title and Body columns together into a single column
questions_2008$Text <- paste(questions_2008$Title, " ", questions_2008$Body)
#questions_2008$Text

### Converting into ASCII data
questions_2008$Text <- iconv(questions_2008$Text, to='ASCII', sub='byte')

### Removing all html tags, special characters, control characters, single and double quotes from the dataset
questions_2008$Text <- gsub("<[^>]+>", "", questions_2008$Text)
questions_2008$Text <- gsub("[[:punct:]]", " ", questions_2008$Text)
questions_2008$Text <- gsub("[[:cntrl:]]", " ", questions_2008$Text)
questions_2008$Text <- gsub("'", " ", questions_2008$Text)
questions_2008$Text <- tolower(questions_2008$Text)
questions_2008$Text <- gsub("[[:space:]]", " ", questions_2008$Text)
questions_2008$Text <- gsub("^[[:space:]]+", " ", questions_2008$Text)
questions_2008$Text <- gsub("[[:space:]]+$", " ", questions_2008$Text)
questions_2008$Text <- gsub("[[:digit:]]+", " ", questions_2008$Text)

class(questions_2008$Text)

### Forming a corpus from merged data frame, treating each row as a single document (done with DataframSource())
corpus_questions_2008 <- VCorpus(VectorSource(questions_2008$Text))
writeLines(as.character(corpus_questions_2008[[5]]))

### Removing stop words using standard list of tm library
corpus_questions_2008 <- tm_map(corpus_questions_2008, removeWords, stopwords("english"))
writeLines(as.character(corpus_questions_2008[[5]]))

### Stemming the document
corpus_questions_2008 <- tm_map(corpus_questions_2008, stemDocument)
writeLines(as.character(corpus_questions_2008[[5]]))

### Document Term Matrix 
dtm_questions_2008 <-DocumentTermMatrix(corpus_questions_2008, control=list(wordLengths=c(4, 12), bounds = list(global = c(100, 1500))))
dtm_questions_2008

### Set parameters for Gibbs sampling
# Gibbs sampling works by performing a random walk in such a way that reflects the characteristics
# of a desired distribution. Because the starting point of the walk is chosen at random, 
# it is necessary to discard the first few steps of the walk 
# (as these do not correctly reflect the properties of distribution). 
# This is referred to as the burn-in period. We set the burn-in parameter to  4000. 
# Following the burn-in period, we perform 2000 iterations, taking every 500th  iteration for further use.
# The reason we do this is to avoid correlations between samples. We use 5 different starting points (nstart=5) 
# – that is, five independent runs. Each starting point requires a seed integer (this also ensures reproducibility), 
# so we have provided 5 random integers in my seed list. Finally we’ve set best to TRUE (actually a default setting), 
# which instructs the algorithm to return results of the run with the highest posterior probability.

burnin <- 1000
iter <- 500
thin <- 105
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE

### Number of topics
k <- 40 

### Remove sparsity from the DTM to apply lda
non_sparse_dtm <- removeSparseTerms(dtm_questions_2008, 0.99)
rowTotals <- apply(non_sparse_dtm , 1, sum)
#class(rowTotals)
final_dtm_questions_2008 <- non_sparse_dtm[rowTotals>0, ]
final_dtm_questions_2008

#Run LDA using Gibbs sampling
tic()
ldaOut <- LDA(final_dtm_questions_2008 ,k , method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
toc()

### Write out results
# Docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics
write.csv(ldaOut.topics,file=paste("LDAGibbs", k, "DocsToTopics.csv"))

# Top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms
write.csv(ldaOut.terms, file=paste("LDAGibbs", k, "TopicsToTerms.csv"))

# Probabilities associated with each topic assignment
topicProb <- as.data.frame(ldaOut@gamma)
topicProb
write.csv(topicProb, file=paste("LDAGibbs", k, "TopicProbabilities.csv"))

# Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(final_dtm_questions_2008), function(x) sort(topicProb[x, ])[k]/sort(topicProb[x, ])[k-1])
write.csv(topic1ToTopic2, file=paste("LDAGibbs", k, "Topics1toTopics 2.csv"))

# Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(final_dtm_questions_2008), function(x) sort(topicProb[x, ])[k-1]/sort(topicProb[x, ])[k-2])
write.csv(topic2ToTopic3, file=paste("LDAGibbs", k, "Topics2ToTopics3.csv"))

### LDA VISUALIZATIONS
# Theta <- Document To Topic Distribution (matrix)
# Phi <- Topic To Term Distribution (matrix)
# vocab <- number of tokens in each document (matrix)
# doc.length <- number of tokens in each document
# term.frequency <- frequency of each term in the vocabulary
topicmodels2LDAvis <- function(x, ...)
{
  # Determine the posterior probabilities of the topics for each document
  # and of the terms for each topic for a fitted topic model.
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments   ### Gives simple triplet representation (sparse matrix) of lda output
  LDAvis::createJSON(phi = post[["terms"]], theta = post[["topics"]], vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE))
}

LDAvis::serVis(topicmodels2LDAvis(ldaOut))
