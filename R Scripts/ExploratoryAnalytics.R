library(tm)
library(readr)
library(dplyr)
library(lubridate)
library(SnowballC)

questions <- read_csv("Questions.csv")
tags <- read_csv("Tags.csv")
answers <- read_csv("Answers.csv")

questions_2008<-left_join(questions,tags,by="Id")
questions_2008<- filter(questions_2008, year(CreationDate)=="2008")


answers_2008 <- filter(answers, year(CreationDate)=="2008")

#questions_answers_2008 <- left_join(questions_2008,answers_2008, by.x="Id", by.y="ParentId")

######Replacing the html tags in the CSV file##############
questions_2008$Title <- gsub("<[^>]+>","",questions_2008$Title)
questions_2008$Body <- gsub("<[^>]+>","",questions_2008$Body)

pop_tags_2008<-questions_2008 %>%
  select(Id,Tag) %>%
  count(Tag,sort=TRUE) %>%   
  mutate(freq=paste0(round(100*n/sum(n),2),"%")) 

View(pop_tags_2008)

distrib_tags_2008<-questions_2008 %>%
  select(Id,Tag) %>%
  group_by(Id) %>%
  summarize (n_tags=n()) %>%
  arrange(desc(n_tags))

View(distrib_tags_2008)

questions_2008_python <- filter(questions_2008, Tag=="python" )
#length(questions_2008_python)

sum(!is.na(questions_2008$ClosedDate))/nrow(questions_2008)

####Score for each tag
score_tags_2008<-questions_2008 %>%
  select(Score,Tag) %>%
  group_by(Tag) %>%
  summarize(score_avg=mean(Score),n_questions=n())%>%
  arrange(desc(n_questions))

View(score_tags_2008)

top_5_questions_2008 <- questions_2008 %>%
  filter(Tag=="python") %>%
  arrange(desc(Score)) %>%
  head(5)

names(answers_2008)[names(answers_2008) == 'Id'] <- 'AnswerId'
questions_answers_2008 <- merge(questions_2008, answers_2008, by.x = "Id", by.y = "ParentId", all.x = TRUE)


top_5_users_2008 <- questions_2008 %>%
  select(OwnerUserId,Tag) %>%
  group_by(OwnerUserId) %>%
  summarize(n_questions=n()) %>%
  arrange(desc(n_questions))

View(top_5_users_2008)

###Top people who have the most number of questions in python tag
top_5_answers_2008 <- questions_answers_2008 %>%
  filter(Tag=="python") %>%
  select(OwnerUserId.y,AnswerId) %>%
  group_by(OwnerUserId.y) %>%
  summarize(n_answers=n()) %>%
  arrange(desc(n_answers))

View(top_5_answers_2008)



###Top people arranged by answer scores
top_5_questions_2008 <- questions_answers_2008 %>%
  filter(Tag=="python") %>%
  select(OwnerUserId.y,Score.y) %>%
  group_by(OwnerUserId.y) %>%
  summarize(total_score=sum(Score.y)) %>%
  arrange(desc(total_score))
View(top_5_questions_2008)  
  



questions_2008 <- select(questions_2008,Title,Body)
questions_2008$Text <-paste(questions_2008$Title," ",questions_2008$Body)
questions_2008$Text <- gsub("[[:punct:]]", " ", questions_2008$Text) ##Replaces all punctuations and signs with " "

questions_2008$Text <- iconv(questions_2008$Text, to = "ASCII", sub = "byte")
corp1 <- VCorpus(VectorSource(questions_2008$Text))


corp1 <- tm_map(corp1,content_transformer(tolower)) #Converts the corpus to lowercase 


corp1 <- tm_map(corp1, removeNumbers) #Strip digits (std transformation, so no need for content_transformer)


corp1<- tm_map(corp1, removeWords, stopwords("english")) #remove stopwords using the standard list in tm


corp1 <- tm_map(corp1, stripWhitespace) #Strip whitespace (cosmetic?)


corp1 <- tm_map(corp1,stemDocument) #Stem document, i.e. converts all forms of words into their root forms


