#' ---
#' author: 32026312
#' title: Lab # 5
#' date: 30th Oct 2017
#' ---
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)

#question 11
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
expr <- "\\d+(.*)km/h|\\d+(.*)kmh"
expr2<- "\\d+\\s*[kK]nots*|KNOTS|[kK]ts|[kK]t"

km  <- str_extract(storm1$All.comments, regex(expr,ignore_case = T))
knots  <- str_extract(storm1$All.comments, expr2)
summary(table(km))
summary(table(knots))
table(km)
#got the data with 'km/h'  and "knots" unit
km[is.na(km)]<-0  
knots[is.na(knots)]<-0      

#now extrct the value from both 'km' and 'knots'
numexpr = "\\d+"
num_km  <- str_extract(km, numexpr)
num_knots  <- str_extract(knots, numexpr)
table(num_km)
table(num_knots)
num_knots = as.numeric(num_knots)


#round the km/h unit
y =as.numeric(num_km)*1.852
for (i in 1:length(y)){
  y[i] = round(y[i],0)
}

#creat a new vector to store the value
exp_data = storm1[6]
head(exp_data)
exp_data$wind_spead = y
#substitute the km_data in to windspead_value
for (i in 1:length(exp_data)){
  if(exp_data$wind_spead[i] == 0){
    exp_data$wind_spead[i] = num_knots[i]
  } 
}

head(exp_data)
dim(exp_data)
#delete the windspead which is obivously unreasonable
exp_data$wind_spead[exp_data$wind_spead==0]<- NA
exp_data$wind_spead[exp_data$wind_spead>400]<- NA
exp_data<-na.omit(exp_data)#delete the NA 

boxplot(as.numeric(exp_data$wind_spead)~exp_data$State)
