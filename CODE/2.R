#' ---
#' author: 32026312
#' title: Lab 2 
#' date: 9th Oct 2017
#' ---


library(dplyr)
library(ggplot2)
filename = "accidents2014.csv"
data = read.csv(file = filename)

names(data)# to check if the names of variables mathch
nrow(data)
dim(data)

header = names(data)
header
class(header)
new_header = header[c(-5,-6,-9,-10,-11)]
new_data = select(data,new_header)
head(new_data)
dim(new_data)
new_header
new_data = filter(new_data,new_data[,11] ==  9)
new_data = filter(new_data,new_data[,6] != 1 )
dim(new_data)
class(new_data)

new_data2 = mutate(new_data, distance_of_accident = ((Grid.Ref..Easting - 429967)^2                                                     +(Grid.Ref..Northing - 434260)^2)^(1/2))
new_data3 = arrange(new_data2,distance_of_accident)
tail(new_data3)
new_header
qplot(new_data3[,10],binwidth = 10,xlab = 'Casualty Age',ylab = 'NO. of Casualty')

