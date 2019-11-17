# required libraries
library(tidyverse)

# generate some fake survey data
set.seed(69)

id <- 1:2000
gender <- sample(c("Male","Female"),2000,replace=TRUE)
age <- sample(c("18-24","25-34","35-54","55-64","65+"),2000,replace=TRUE,prob=c(0.15,0.20,0.3,0.15,0.2))
province <- sample(c("BC","AB","ON","QC"),2000,replace=TRUE,prob=c(0.2,0.2,0.35,0.25))
product <- sample(c("Bag of Glass","Bass-O-Matic","Happy Fun Ball","Little Chocolate Donuts"),2000,replace=TRUE,prob=c(0.1,0.5,0.2,0.2))
Q1 <- sample(c("Very satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied","Somewhat dissatisfied","Very dissatisfied"),2000, replace=TRUE)
Q2 <- sample(c("Very satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied","Somewhat dissatisfied","Very dissatisfied"),2000, replace=TRUE)
Q3 <- sample(c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree"),2000, replace=TRUE)
Q4 <- sample(c("Extremely helpful","Very helpful","Somewhat helpful","Not so helpful","Not at all helpful"),2000, replace=TRUE)
Q5 <- sample(0:10,2000,replace=TRUE)

df <- data.frame(id,gender,age,province,product,Q1,Q2,Q3,Q4,Q5)

# OR load some data from csv
df <- read.csv('My_Survey_Results.csv')

# tidy data
df_tidy <- gather(df, question, response, 6:dim(df)[2])

# summarize data by question
df_summary <- group_by(df_tidy, question, response) %>%
             summarise(n = n()) %>%
             mutate(perc = n / sum(n)) %>%
             arrange(desc(n), .by_group = TRUE)

# filter one question
df_Q4 <- filter(df_summary, question == 'Q4')

# summarize a question by province (cross tab)
df_summary <- filter(df_tidy, question == 'Q1') %>%
              group_by(province, response) %>%
              summarise(n = n()) %>%
              spread(key = province, value = n, fill = 0)

# summarize a question by province (cross tab)
df_summary <- filter(df_tidy, question == 'Q1') %>%
              group_by(province, response) %>%
              summarise(n = n()) %>%
              mutate(perc = n / sum(n)) %>%
              select(-n) %>%
              spread(key = province, value = perc, fill = 0)

# summarize Q5 results
df_summary <- filter(df_tidy, question == 'Q5') %>%
              group_by(response) %>%
              summarise(n = n()) %>%
              mutate(perc = n / sum(n)) %>%
              arrange(desc(as.numeric(response)))

# calculate promoters, detractors and NPS
promoters <- sum(df_summary$perc[df_summary$response %in% c(9,10)])
detractors <- sum(df_summary$perc[df_summary$response %in% c(0:6)])
nps <- promoters - detractors

