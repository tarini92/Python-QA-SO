rm(list = ls())

# Load libraries
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
library(openNLP)
library(NLP)

# Similarity function of TEXTRANK
simil_textRank <- function(x, y)
{
  # b is the intersection of common words, k and p are the length of two sentences
  b <- Reduce(intersect, list(x, y))
  c <- length(b) - 1
  m <- 0
  n <- 0
  k <- length(x)
  p <- length(y)
  
  for(i in 1:k)
  {
    if(x[i] != "")
    {
      m <- m + 1
    }
  }
  
  for(j in 1:p)
  {
    if(y[j] != "")
    {
      n <- n + 1
    }
  }
  
  func <- c / (log(m) + log(n))
  return(func)
}

setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

questions <- read_csv('Questions.csv')
questions$CreationDate <- as.Date(questions$CreationDate)

### filtering questions and answers of year 2008
questions_2008 <- filter(questions, year(CreationDate) == "2008" & month(CreationDate) == "8")

### Merging Title and Body columns together into a single column
questions_2008$Text <- paste(questions_2008$Title, " ", questions_2008$Body )
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

#class(questions_2008$Text)

### Forming a corpus from merged data frame, treating each row as a single document (done with DataframSource())
corpus_questions_2008 <- VCorpus(VectorSource(questions_2008$Text))
writeLines(as.character(corpus_questions_2008[[5]]))

### Removing stop words using standard list of tm library
corpus_questions_2008 <- tm_map(corpus_questions_2008, removeWords, stopwords("english"))
writeLines(as.character(corpus_questions_2008[[5]]))

### Stemming the document
corpus_questions_2008 <- tm_map(corpus_questions_2008, stemDocument)
writeLines(as.character(corpus_questions_2008[[5]]))

j <- length(questions_2008$Text)

corp_list <- list()
for(i in 1:j)
{
    corp_list[[length(corp_list) + 1]] <- corpus_questions_2008[[i]]
}

p <- length(questions_2008$Text)
m <- matrix(NA, nrow = p, ncol = p)
simil_trank <- as.data.frame(m)

tic()
for(i in 1:p)
{
  for(j in 1:p)
  {
    simil_trank[i, j] = simil_textRank(corp_list[[i]], corp_list[[j]])
  }
}
toc()

# Calculate PAGERANK algorithm  
M <- t(simil_trank / rowSums(simil_trank))
n <- nrow(M)
U <- matrix(data = rep(1/n, n^2), nrow = n, ncol = n)
beta <- 0.85
A <- beta*M + (1-beta)*U
A[is.na(A)] <- 0
A[is.infinite(A)] <- 0
e <- eigen(A)
v <- e$vec[, 1]
v <- as.numeric(v)/sum(as.numeric(v))

# Find N most highest value location in a vector and its value
highest <- order(v, decreasing = TRUE)[1:10]
v[highest]

# Create list from highest eigenvalue documents
summ_list <- list()
m <- length(highest)

for(i in 1:m)
{
  x <- highest[i]
  summ_list[[length(summ_list) + 1]] <- questions_2008$Text[x]
}

# Writing the results
d <- lapply(summ_list, write.table, file = "summary_textRank.txt", append = TRUE)
