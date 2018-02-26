#' ---
#' title: Lab 4 assessment
#' author: Tom Palmer
#' date: 21st October 2017
#' output: pdf_document
#' ---

library(tidyverse)
# library(dplyr)
# library(ggplot2)
library(lubridate)


# 1. 
setwd("H:\\all\\teaching\\Math550 Stats in Practice\\R\\MATH550\\R_course\\Lab4\\Coursework")

# ------------------------------------------------------------------------------
# 2. Load
# ------------------------------------------------------------------------------
storm <- read.csv("Australia_severe_storms_1975-2015.csv",
                  stringsAsFactors=FALSE)

print("Dimensions of StormEvents data set are:")
print(dim(storm))

# ------------------------------------------------------------------------------
# 3. Clean
# ---------------------------------------------------------

storm <- select(storm, Event.ID, Database, Date.Time, 
                Nearest.town, State, Latitude, Longitude)


#storm$Date.Time<-dmy_hm(storm$Date.Time)


storm <- storm[storm$Database != "Waterspout", ]
print(dim(storm))
print(head(storm))

#----------------------------------------------------------
# 4. time zone allocation
#----------------------------------------------------------

# OlsonNames() classifications

state_allocate <- function(state.in="NSW",town.in="Sydney") 
{
  if(state.in=="QLD") timezone="Australia/Queensland"
  if(state.in=="VIC") timezone="Australia/Victoria"
  if(state.in=="SA") timezone="Australia/South"
  if(state.in=="WA") timezone="Australia/West"
  if(state.in=="TAS") timezone="Australia/Tasmania"
  if(state.in=="NT") timezone="Australia/North"
  if(state.in=="ACT") timezone="Australia/ACT"
  if(state.in=="NSW") timezone="Australia/NSW"
  if((state.in=="NSW") & ((town.in=="Broken Hill") | (town.in=="BROKEN HILL") | 
                      (town.in=="BROKEN HILL AREA") | 
                      (town.in=="MILDURA/BROKEN HILL") | 
                      (town.in=="BROKEN HILL AIRPORT"))) timezone="Australia/Broken_Hill"
 return(timezone)
}

# Well done to those who allocated all Broken Hill categories 
# Marks awarded for Broken Hill or BROKEN HILL
#
#
# Using strings which we learn about in lab 5 there are 
# easier ways to extract all occurrences 


storm$time.zone <- character(14417)
for(i in 1:14417) { 
  storm$time.zone[i] <- state_allocate(storm$State[i], storm$Nearest.town[i])
}

table(storm$time.zone)

#----------------------------------------------------------
# 5. UTC time
#----------------------------------------------------------

for(i in 1:nrow(storm)){
  storm$UTC.Time[i] <- with_tz(dmy_hm(storm$Date.Time[i], tz=storm$time.zone[i]), "UTC")
}
print(head(storm))

#
# Alternative approach:
# Some people used the following approach using vectorisation
# which is particularly fast.
#

storm$UTC.Time <- dmy_hm("01/01/1970 00:00", tz="UTC")

for(timezone in unique(storm$time.zone)){
  storm[storm$time.zone == timezone, "UTC.Time"] <- 
    dmy_hm(storm[storm$time.zone == timezone, "Date.Time"], tz=timezone)
}
print(head(storm))


#----------------------------------------------------------
# 6. Month and year
#-----------------------------------------------------------

storm$month <- month(storm$UTC.Time)
storm$year <- year(storm$UTC.Time)
head(storm)


#---------------------------------------------------------
# 7. Events
#---------------------------------------------------------

table(storm$Database)

Events <- c("Hail", "Lighting", "Rain", "Tornado", "Wind")

Events_count <- data.frame(Month=seq(1,12))
Events_count

Events_count$Hail <- NA
for(i in 1:12) {
  Events_count$Hail[i] <- sum(storm$month==i & storm$Database=="Hail")
}

Events_count$Lighting <- NA
for(i in 1:12) {
  Events_count$Lighting[i] <- sum(storm$month==i & storm$Database=="Lighting")
}

Events_count$Rain <- NA
for(i in 1:12) {
  Events_count$Rain[i] <- sum(storm$month==i & storm$Database=="Rain")
}

Events_count$Tornado <- NA
for(i in 1:12) {
  Events_count$Tornado[i] <- sum(storm$month==i & storm$Database=="Tornado")
}

Events_count$Wind <- NA
for(i in 1:12) {
  Events_count$Wind[i] <- sum(storm$month==i & storm$Database=="Wind")
}

Events_count

q7plot <- ggplot(Events_count) +
   geom_line(aes(x=Month, y=Wind), colour="red") +
   geom_line(aes(x=Month, y=Rain), colour="blue") +
   geom_line(aes(x=Month, y=Hail), colour="green") +
   geom_line(aes(x=Month, y=Tornado), colour="purple") +
   geom_line(aes(x=Month, y=Lighting), colour="yellow") +
  labs(x="Month", y="Event count", title="Storm events by month")
q7plot
