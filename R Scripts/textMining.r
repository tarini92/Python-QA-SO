rm(list = ls())

### Will help in answering popularity of tags, most enquired topics
library(readr)
library(dplyr)
library(lubridate)

## Changing the path to working directory
setwd('/home/akanksha/Desktop/Sem III/ML/Course Project/Dataset/pythonquestions')

## Reading the files
questions <- read_csv('Questions.csv')
answers <- read_csv('Answers.csv')
tags <- read_csv('Tags.csv')

## Filtering out the questions of 2008 year
questions_2008 <- filter(questions, year(CreationDate) == "2008")
answers_2008 <- filter(answers, year(CreationDate) == "2008")
merged_df_ques <- left_join(questions_2008, tags, by = "Id")
# change the column name of answers_2008 data frame to avoid duplication while merging
names(answers_2008)[names(answers_2008) == 'Id'] <- 'AnswerId'
merged_df_ans <- merge(merged_df_ques, answers_2008, by.x = "Id", by.y = "ParentId", all.x = TRUE)

## Count the popularity of each tag
## MOST ENQUIRED TAGS/TOPICS
pop_tags_2008 <- merged_df_ques %>% select(Id,Tag) %>% count(Tag,sort=TRUE) %>% mutate(freq=paste0(round(100*n/sum(n),2),"%"))

## MOST VOTED QUESTIONS/ANSWERS/TAGS
# Which tags tend to have higher/lower score?
summary(questions_2008$Score)

# Calculate average score for each tag
# Tags ordered on the basis of most questions asked
score_tags_2008_ordered_questions <- merged_df_ques %>% select(Score, Tag) %>% group_by(Tag) %>% summarize(score_avg = mean(Score), n_questions = n()) %>% arrange(desc(n_questions))

# Tags ordered on the basis of decreasing average score
score_tags_2008_ordered_score <- merged_df_ques %>% select(Score, Tag) %>% group_by(Tag) %>% summarize(score_avg = mean(Score), n_questions = n()) %>% arrange(desc(score_avg))

# From the average score values, there seem to be a higher tendency to upvote/downvote questions

# Top 10 scored 'python' questions
merged_df_ques %>% filter(Tag == 'python') %>% arrange(desc(Score)) %>% select(Title) %>% head(10)

# Popular users 
# number of users answered for each tag arranged in descending order
pop_users_tags <- merged_df_ans %>% select(OwnerUserId.y, Tag) %>% group_by(Tag) %>% summarize(n_users = n()) %>% arrange(desc(n_users))

# popular users who answered most questions
pop_users_ans <- merged_df_ans %>% filter(Tag == 'python') %>% select(OwnerUserId.y, AnswerId) %>% group_by(OwnerUserId.y) %>% summarize(n_ans = n()) %>% arrange(desc(n_ans))

# popular users who achieved highest scores
pop_users_score <- merged_df_ans %>% filter(Tag == 'python') %>% select(OwnerUserId.y, Score.y) %>% group_by(OwnerUserId.y) %>% summarize(score_avg = sum(Score.y)) %>% arrange(desc(score_avg))
