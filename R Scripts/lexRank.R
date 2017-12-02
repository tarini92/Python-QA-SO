library(readr)
library(dplyr)
library(lubridate)

setwd("/Users/tarinichandra/Desktop/ML-course")

questions <- read_csv("Questions.csv")
questions_2008<- filter(questions, year(CreationDate)=="2008" & month(CreationDate)=="8")
#dim(questions_2008)

#questions_2009 <- filter(questions, year(CreationDate)=="2009")
#dim(questions_2009)
#questions_2010 <- filter(questions, year(CreationDate)=="2010")
#dim(questions_2010)
#questions_2011 <- filter(questions, year(CreationDate)=="2011")
#questions_2012 <- filter(questions, year(CreationDate)=="2012")
#questions_2013 <- filter(questions, year(CreationDate)=="2013")
#questions_2014 <- filter(questions, year(CreationDate)=="2014")
#questions_2015 <- filter(questions, year(CreationDate)=="2015")

library(tm)
library(textreg)
######Replacing the html tags in the CSV file##############
questions_2008$Title <- gsub("<[^>]+>","",questions_2008$Title)
questions_2008$Body <- gsub("<[^>]+>","",questions_2008$Body)
#answers_2008$Body <- gsub("<[^>]+>","",answers_2008$Body)
#View(questions_2016)
questions_2008 <- select(questions_2008,Title,Body)
questions_2008$Text <-paste(questions_2008$Title," ",questions_2008$Body)
questions_2008$Text <- gsub("[[:punct:]]", " ", questions_2008$Text) ##Replaces all punctuations and signs with " "

questions_2016$Text <- iconv(questions_2016$Text, to = "ASCII", sub = "byte")
corpus_questions_2008 <- VCorpus(VectorSource(questions_2008$Text))

corpus_2008_char <- convert.tm.to.character(corpus_questions_2008)


####Calculating LexRank for text


library(lexRankr)

lexScores <- lexRank(corpus_2008_char, docId = "create", threshold = 0.2, n = 10,
        returnTies = TRUE, usePageRank = TRUE, damping = 0.85,
        continuous = FALSE, sentencesAsDocs = FALSE, removePunc = TRUE,
        removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
        rmStopWords = TRUE, Verbose = TRUE)

write.csv(as.matrix(lexScores),"lexScore.csv")

####Network graphs


corpus_questions_2008 <- tm_map(corpus_questions_2008,content_transformer(tolower)) #Converts the corpus to lowercase 


corpus_questions_2008<- tm_map(corpus_questions_2008, removeNumbers) #Strip digits (std transformation, so no need for content_transformer)


corpus_questions_2008<- tm_map(corpus_questions_2008, removeWords, stopwords("english")) #remove stopwords using the standard list in tm


corpus_questions_2008 <- tm_map(corpus_questions_2008, stripWhitespace) #Strip whitespace (cosmetic?)

library(SnowballC)

corpus_questions_2008 <- tm_map(corpus_questions_2008,stemDocument) #Stem document, i.e. converts all forms of words into their root forms

dtm <- DocumentTermMatrix(corpus_questions_2008, control = list(wordLengths = c(3,15) ))

m <- as.matrix(dtm)

write.csv(m,"dtm.csv")

#filekey <- cbind(rownames(m),filenames)
#write.csv(filekey,"filekey.csv")

#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
write.csv(as.matrix(cs),file="cosineSim.csv"
          )
#adjacency matrix: set entries below a certain threshold to 0.
#We choose half the magnitude of the largest element of the matrix
#as the cutoff. This is an arbitrary choice
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)
A <- as.matrix(cs)
B <- A/rowSums(A)
write.csv(as.matrix(cs),file="AdjacencyMatrix.csv")


                                                          