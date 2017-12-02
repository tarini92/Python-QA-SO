rm(list = ls())

### load text mining libraries
library(tm)
library(dplyr)
library(readr)
library(lubridate)
library(tictoc)

setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

questions <- read_csv('Questions.csv')

### filtering questions and answers of year 2008
questions_2008 <- filter(questions, year(CreationDate) == "2008")

### Merging Title and Body columns together into a single column
questions_2008$Text <- paste(questions_2008$Title, " ", questions_2008$Body)

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


### Forming a corpus from merged data frame, treating each row as a single document
corpus_questions_2008 <- VCorpus(VectorSource(questions_2008$Text))
writeLines(as.character(corpus_questions_2008[[5]]))

### Removing stop words using standard list of tm library
corpus_questions_2008 <- tm_map(corpus_questions_2008, removeWords, stopwords("english"))
writeLines(as.character(corpus_questions_2008[[1]]))

### Removing custom stopwords from the corpus
myStopwords <- c("can", "say","one","way","use",
                 "also","however","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","ve","re",
                 "bbbb", "self", "mean", "print", "front", "thanks", "cccc", "init",
                 "find", "full", "guess", "using")

corpus_questions_2008 <- tm_map(corpus_questions_2008, removeWords, myStopwords)

### Stemming the document
corpus_questions_2008 <- tm_map(corpus_questions_2008, stemDocument)
writeLines(as.character(corpus_questions_2008[[5]]))

### Document Term Matrix 
dtm_questions_2008 <-DocumentTermMatrix(corpus_questions_2008, control=list(wordLengths=c(5, 10), bounds = list(global = c(1,1000))))
dtm_questions_2008

### Remove sparsity from the DTM 
non_sparse_dtm <- removeSparseTerms(dtm_questions_2008, 0.999)
non_sparse_dtm


### Storing the dtm as a matrix
mat <- as.matrix(non_sparse_dtm)

write.csv(mat, file="dtm_questions_2008.csv")

### Compute distance between document vectors
tic()
d <- dist(mat)
toc()

### Run HIERARCHICAL CLUSTERING using Ward's method
tic()
groups <- hclust(d, method="ward.D")
toc()

#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

### Cut dendogram into trees
rect.hclust(groups, 5)

### Run KMEANS algorithm, with k clusters and 50 starting configurations
k <- 4
tic()
kFit <- kmeans(d, k, nstart = 10)
toc()

### Kmeans - find the optimum number of clusters (elbow method)
# look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:10
tic()
for(i in 2:10) 
      wss[i] <- sum(kmeans(d, centers = i, nstart= 10)$withinss)
toc()

plot(2:10, wss[2:10], type="b", xlab="Number of clusters", ylab="Within groups sum of squares")
