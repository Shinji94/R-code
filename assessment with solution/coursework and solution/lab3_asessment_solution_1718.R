#' ---
#' author: Tom Palmer
#' title: Lab 3 - coursework solutions
#' date: 13th October 2017
#' output: pdf_document
#' ---

#
# Lotka-Volterra
#

#install.packages("tidyverse")
#library(tidyverse) 
# or, 
#install.packages(ggplot2)
#install.packages(dplyr)
library(ggplot2)
library(dplyr)

#
# 1.
# 

# Set the parameters
alpha <- 0.05
beta <- 0.0001
gamma <- 0.02

# Set the initial conditions
R <- 30
F <- 40
weeks <- 104

# Run the deterministic model.
for (t in 1:(weeks - 1)) {
   R[t+1] <- R[t] + alpha*R[t] - beta*R[t]*F[t]
   F[t+1] <- F[t] + beta*R[t]*F[t] - gamma*F[t]
 }
print(c(R[t+1], F[t+1]))

#
# 2.
#

# Set the initial conditions
SR <- 30
SF <- 40
weeks <- 104

# Set the random seed
#
# This is done so the answer should be the same each time 
# as the same random numbers are used.
set.seed(60854)

# Simulate the data
# Note that rabbits eaten (=EAT) = foxes born
for (t in 1:(weeks - 1)) {
  EAT <- rbinom(1, (SR[t]*SF[t]), beta)
  SR[t+1] <- SR[t] + rbinom(1, SR[t], alpha) - EAT
  SF[t+1] <- SF[t] + EAT - rbinom(1, SF[t], gamma)
}
print(c(SR[t+1], SF[t+1]))

# alternative solution
set.seed(60854)
SR2 <- 30
SF2 <- 40
for (t in 1:(weeks - 1)) {
  EAT2 <- rbinom(1, (SR2[t]*SF2[t]), beta)
  SF2[t+1] <- SF2[t] + EAT2 - rbinom(1, SF2[t], gamma)
  SR2[t+1] <- SR2[t] + rbinom(1, SR2[t], alpha) - EAT2
}
print(c(SR2[t+1], SF2[t+1]))

# alternative solution - but less good
set.seed(60854)
SR3 <- 30
SF3 <- 40
for (t in 1:(weeks - 1)) {
  SR3[t+1] <- SR3[t] + rbinom(1, SR3[t], alpha) - rbinom(1, (SR3[t]*SF3[t]), beta)
  SF3[t+1] <- SF3[t] + rbinom(1, (SR3[t]*SF3[t]), beta) - rbinom(1, SF3[t], gamma)
}
print(c(SR3[t+1], SF3[t+1]))


#
# 3.
#

# Create long data frame
LV <- data.frame(time=rep(1:104,4),
                group=c(rep("Rabbits", 104), rep("Foxes", 104),
                        rep("Sto_Rabbits", 104), rep("Sto_Foxes", 104)),
                size=c(R, F, SR, SF))

# Generate plot
foxes_rabbits_plot <- 
ggplot(LV, aes(x=time, y=size, colour=group)) +
  geom_point() +
  geom_line() +
  labs(x="Time period (weeks)", y="Population levels",
       colour="Group") +
  theme_bw() +
  theme(legend.position="top") + 
  scale_colour_grey()

foxes_rabbits_plot

# Save plot to file.
ggsave(foxes_rabbits_plot, file="foxes_rabbits_YYYYYYYY.png", width=5, height=4)
