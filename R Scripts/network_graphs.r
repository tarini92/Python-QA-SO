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


# Network Graphs Visualization using R and Gephi software
# to visualize similarity-based relationships of documents within a corpus
# Cosine similarity between the documents as the angle between the vectors of them in unique word space

setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

questions <- read_csv('Questions.csv')
questions$CreationDate <- as.Date(questions$CreationDate)

### filtering questions and answers of year 2008 and month of August
questions_2008 <- filter(questions, year(CreationDate) == "2008" & month(CreationDate) == "8")

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
dtm_questions_2008 <-DocumentTermMatrix(corpus_questions_2008, control=list(wordLengths=c(4, 14)))
dtm_questions_2008

# Convert dtm into a matrix
dtm_mat <- as.matrix(dtm_questions_2008)

# Write to a csv file
write.csv(dtm_mat, file = 'dtm_Questions_2008_matrix.csv')

# compute cosine similarity between document vectors
# converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x)
{
     as.dist(x %*% t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

cs <- cosineSim(dtm_mat)

# adjacency matrix: set entries below a certain threshold to 0.
# We choose half the magnitude of the largest element of the matrix
# as the cutoff. This is an arbitrary choice

cs[cs < max(cs)/2 ] <- 0

cs <- round(cs, 3)

write.csv(as.matrix(cs), file = "AdjacencyMatrix.csv")

### Visualization using GEPHI - open source, JAVA based network analysis and visualization tool
# The nodes with the highest connectivity are indicated via node size and colour  (purple for high, green for low) 
# and strength of similarity is indicated by edge thickness.
