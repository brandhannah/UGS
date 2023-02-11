## Overview
ls()
rm("apollo_ztest","moveold", "quicktexregapollo_old")

## Reading in Data

database <- read.csv("data/UGS_data_wide.csv")

## Rearranging

database <- database %>% arrange(id)

## Mean Centering Interaction Term "Health Condition" and Cleaning

database <- database %>% drop_na(HealthCondition) %>% mutate(HealthMC = HealthCondition - mean(HealthCondition))

summary(database$HealthMC)


## Mean Centering Interaction Term "Present Income" and Cleaning

database <- database %>% arrange(id) %>% drop_na(IncomePresent) %>% mutate(IncomeMC = IncomePresent - mean(IncomePresent)) 


summary(database$IncomeMC)

## Cleaning + Mean Centering for Model 3 

database <- database %>% arrange(id) %>% drop_na(Kleingarten)  
database <- database %>% arrange(id) %>% drop_na(Balcony) 
database <- database %>% arrange(id) %>% drop_na(Garden) 
database <- database %>% arrange(id) %>% drop_na(GreenBackyard) 


## Overview

View(database)

names(database)

table(database$choice)

summary(database)


## Selected summaries

summary(database$Miete2)

summary(database$Naturnähe1)