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

View(questions)
View(answers)
View(tags)

### summarize questions dataset
questions$CreationDate

## Filtering out the questions of 2016 year
questions_2016 <- filter(questions, year(CreationDate) == "2016")
merged_df <- left_join(questions_2016, tags, by="Id")

View(merged_df)

## Count the popularity of each tag
pop_tags_2016<-merged_df %>% select(Id,Tag) %>% count(Tag,sort=TRUE) %>% mutate(freq=paste0(round(100*n/sum(n),2),"%"))
View(pop_tags_2016)

