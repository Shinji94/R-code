#' ---
#' author: 32026312
#' title: Lab # 5
#' date: 30th Oct 2017
#' ---
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)

#question 1
file = 'Australia_severe_storms_1975-2015 (1).csv'
setwd('C:\\Users\\Hasee\\Desktop\\r programming\\datasets')
storm = read.csv(file)
head(storm)
storm$All.comments = str_c(storm[,9],storm[,10],storm[,11],storm[,12],storm[,13],storm[,14])
storm1 = storm[,-7:-14]

head(storm1)

print(sapply(storm1,class))

#question 2


expr <- "\\b[fF]lash flood"
storm1$flash_flood <- str_detect(storm1$All.comments, expr)
storm1$flash_flood

storm1$year = year(dmy_hm(storm1$Date.Time))

tempdata = storm1[,8:9]

tempdata1 = as.data.frame(table(tempdata))
tempdata2 = filter(tempdata1,tempdata1$flash_flood == 'TRUE')
tempdata2

p = ggplot(tempdata2) + 
  geom_line(aes(x = as.numeric(year)+1974,y =Freq))

p
#question 3 
expr <- "([0-9]km/h|[0-9][kK]nots|[0-9]KNOTS|[0-9]KTS|[0-9]kts|[0-9]kt)"
x  <- str_extract(storm1$All.comments, expr)
table(x)
