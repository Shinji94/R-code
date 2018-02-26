#'---
#' author: Tom Palmer
#' title: Lab 2 coursework - Leeds accident data
#' date: 8th October 2017
#'---

#
# 1.
#
# Read in data.
#

setwd("H:\\all\\teaching\\Math550 Stats in Practice\\R\\MATH550\\R_course\\Lab2\\Coursework")

#install.packages("dplyr")
#install.packages("ggplot")
# or
#install.packages("tidyverse") # tidyverse: ggplot2, dplyr, tidyr, readr, purrr, tibble
library(dplyr)
library(ggplot2)

accidents <- read.csv("accidents2014.csv")
print(dim(accidents))

#
# 2.
#

# Select the required varibles
acc1 <- select(accidents, Reference.Number,
             Grid.Ref..Easting, Grid.Ref..Northing, 
             Number.of.Vehicles, Number.of.Casualties,
             X1st.Road.Class, Casualty.Class,
             Casualty.Severity, Sex.of.Casualty, 
             Age.of.Casualty, Type.of.Vehicle)

# Keep cars and non-motorway accidents.
acc2 <- filter(acc1, Type.of.Vehicle==9, X1st.Road.Class!=1)
print(dim(acc2))

#
# 3.
#

#Leeds_Easting <- 429967
#Leeds_Northing <- 434260

#
# Function for computing the distance from the centre of Leeds.
#
Leeds_dist <- function(Easting, Northing){
  Leeds_Easting <- 429967
  Leeds_Northing <- 434260
  distance <- sqrt((Easting - Leeds_Easting)^2 + 
                   (Northing - Leeds_Northing)^2)
  return(distance)
}

#
# Compute the distance from the centre of Leeds.
#
acc3 <- mutate(acc2, distance=Leeds_dist(Grid.Ref..Easting,
                                      Grid.Ref..Northing))

#
# Rearrange the accidents by distance.
#
acc4 <- arrange(acc3, distance)

# output
print(tail(acc4))

#
# 4.
#

# Producing the plot
Age <- ggplot(acc4) +
     geom_histogram(aes(x=Age.of.Casualty), binwidth=10) +
     labs(x="Casualty age", y="No. of casualties")
Age

# Saving it as a file.
ggsave(Age, file="Age.png")

# Making sure the bins do not go below zero

Age2 <- ggplot(acc4) +
  geom_histogram(aes(x=Age.of.Casualty), breaks=seq(0, 100, 10)) +
  labs(x="Casualty age", y="No. of casualties")
Age2 
