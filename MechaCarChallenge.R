# Load in the MechaCar Data and all required packages
library(jsonlite)
library(fs)
library(tidyverse)
library(dplyr)
# MPG Analysis 
names <- c("vehicle_length", "vehicle_weight", "spoiler_angle", "ground_clearance", 
           "AWD", "mpg")
mecha_car <- read.csv("MechaCar_mpg.csv", stringsAsFactors=F, col.names = names)
# Determine if there is a linear relationship between the various variables and
# the resulting mpg using multiple variable linear regression
mecha_car %>% rename(vehicle)
# Create two tables for drivetrain options (AWD, and non-AWD)
mecha_car_awd <- subset(mecha_car, AWD ==1)
mecha_car_nonawd <- subset(mecha_car, AWD ==0)
# Performing multiple variable regression for both tables 
## AWD Table 
awd_coeff = lm(mpg ~ vehicle_length+ vehicle_weight+ spoiler_angle + ground_clearance, data=mecha_car_awd)
summary(lm(mpg ~ vehicle_length+ vehicle_weight+ spoiler_angle + ground_clearance
           , data=mecha_car_awd))
## NON-AWD Table 
nonawd_coeff = lm(mpg ~ vehicle_length+ vehicle_weight+ spoiler_angle + ground_clearance, data=mecha_car_nonawd)
summary(lm(mpg ~ vehicle_length+ vehicle_weight+ spoiler_angle + ground_clearance
           , data=mecha_car_nonawd))
# Comparing the two models to determine which has higher MPG
values <- c(mean(mecha_car$vehicle_length),mean(mecha_car$vehicle_weight),mean(mecha_car$spoiler_angle),
            mean(mecha_car$ground_clearance))
awd_mpg = awd_coeff$coefficients["vehicle_length"]*values[1]+awd_coeff$coefficients["vehicle_weight"]*values[2]+
  awd_coeff$coefficients["spoiler_angle"]*values[3]+awd_coeff$coefficients["ground_clearance"]*values[4]+
  awd_coeff$coefficients["(Intercept)"]
nonawd_mpg = nonawd_coeff$coefficients["vehicle_length"]*values[1]+nonawd_coeff$coefficients["vehicle_weight"]*values[2]+
  nonawd_coeff$coefficients["spoiler_angle"]*values[3]+nonawd_coeff$coefficients["ground_clearance"]*values[4]+
  nonawd_coeff$coefficients["(Intercept)"]
# Suspension Coil ANalysis 
# Load into the suspension data
susp <- read.csv("Suspension_Coil.csv")
susp_summary <- susp %>% summarize(Mean=mean(PSI), Median=median(PSI),Standard_Deviation=sd(PSI), Variance=var(PSI))
# Suspension Coil T-test 
susp_values <- susp$PSI
pop_mean <- 1500
t.test(susp_values, mu=pop_mean)