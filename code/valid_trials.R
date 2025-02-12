#---------------------------------------------------------------
# Author: Damian Beck
# Date: June 2024
# Based on r version 4.3.2
#---------------------------------------------------------------
##bimodal prior integration in VR-Tennis 
##according to Bayesian decision theory

#calculate missing trials and backswing hits

# Packages ----
#---------------------------------------------------------------
#install.packages("car", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("nlme", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("sjPlot", dependencies = TRUE)
#install.packages("broom.mixed", dependencies = TRUE)
#install.packages("psych", dependencies = TRUE)
#install.packages("MASS", dependencies = TRUE)

library(car)
library(ggplot2)
library(nlme)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(psych)
library(MASS)

# Import data ----
#---------------------------------------------------------------
data_all_day1 <- read.csv("data/output_day1.csv")
data_all_day2 <- read.csv("data/output_day2.csv")
data_all_day3 <- read.csv("data/output_day3.csv") 
data_all_control <- read.csv("data/output_control.csv") 

# filter out warm up trials
data_all_day1 <- data_all_day1 %>% filter(trial_number > 96)
data_all_control <- data_all_control %>% filter(trial_number > 96)

#Merge data from day 2 and day 3
data_all <- rbind(data_all_day1, data_all_day2, data_all_day3, data_all_control)

# overall number of non valid trials
# each day 24 subjects * 480 trials = 288 trials = 11520 trials (without warm-up)
# No hit trials = 4 * 11520 - length(data_all)
number_of_no_hits <- 4*11520 - dim(data_all)[1]
percentage_of_no_hits <- number_of_no_hits / 4/11520 * 100
percentage_of_no_hits #percentage of no hits is 11.88

# exclude data for backswing true
number_of_backswing_hits <- data_all %>% filter(backswing == "True")
dim(number_of_backswing_hits)[1]
percentage_of_backswing_hits <- dim(number_of_backswing_hits)[1] / 4/11520 * 100
percentage_of_backswing_hits #percentage of backswing hits is 2.20%

#slow
number_of_backswing_hits_slow <- number_of_backswing_hits %>% filter(condition == "slow")
dim(number_of_backswing_hits_slow)[1]
percentage_of_backswing_hits_slow <- dim(number_of_backswing_hits_slow)[1] / 4*3/11520 * 100
percentage_of_backswing_hits_slow #percentage of backswing hits is 0.40%


#moderate
number_of_backswing_hits_moderate <- number_of_backswing_hits %>% filter(condition == "moderate")
dim(number_of_backswing_hits_moderate)[1]
percentage_of_backswing_hits_moderate <- dim(number_of_backswing_hits_moderate)[1] / 4*3/11520 * 100
percentage_of_backswing_hits_moderate #percentage of backswing hits is 1.24%


#fast
number_of_backswing_hits_fast <- number_of_backswing_hits %>% filter(condition == "fast")
dim(number_of_backswing_hits_fast)[1]
percentage_of_backswing_hits_fast <- dim(number_of_backswing_hits_fast)[1] / 4*3/11520 * 100
percentage_of_backswing_hits_fast #percentage of backswing hits is 4.95%

