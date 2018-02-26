#' ---
#' author: 32026312
#' title: Lab 4
#' date: 6th Oct 2017
#' ---

#1
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
#setwd('C:\\Users\\Hasee\\Desktop\\r programming\\datasets')
#2
data = read.csv('Australia_severe_storms_1975-2015.csv', stringsAsFactors = FALSE)
dim(data)

#3
data = data[,-9:-15]
new_data = filter(data,data$Database != "Waterspout")
head(new_data)

#4
y = list(OlsonNames())[[1]][345:366]
district = c(y[3],y[13],y[14],y[16],y[17],y[19],y[20],y[21])
timezone = c()
for (i in 1:14417){
  timezone[i] = switch(new_data$State[i], 'Broken Hill'  = district[1],NT = district[2],NSW = district[3],QLD = district[4],
         SA = district[5],TAS = district[6],VIC = district[7], WA = district[8])
   
}
#i do some prepoceess to the csv.file,change all the 'brokenhill' relevent place into 'BROKENHILL'
new_data$Timezone = timezone
for (i in 1:14417){
  if(new_data$Nearest.Town[i]=="BROKENHILL")
    new_data$Timezone[i]<-"Australia/Broken_Hill"
}
head(new_data)



#5
UCT="00"
new_data<-mutate(new_data,UCT)
for (i in 1:nrow(new_data)){
  local_t<-dmy_hm(as.character(new_data$Date.Time[i]),tz=as.character(new_data$Timezone[i]))
  new_data$UCT[i]<-as.character(with_tz(local_t,tz="UCT"))
}
head(new_data)

#6

month = month(new_data$UCT[])
year = year(new_data$UCT[])
head(year)
new_data$month = month
new_data$year = year
head(new_data)

#7
dim(new_data)

forty_yr<-group_by(new_data,month,Database,add = FALSE)
forty_Sum <- summarise(forty_yr,
                       count=n()
) 
forty_Sum
dim(forty_Sum)

ggplot(forty_Sum)+
  geom_line(aes(x = month,y = forty_Sum$count,group = as.factor(Database),colour = as.factor(Database),
                linetype = as.factor(Database)),
                alpha = 0.5)
