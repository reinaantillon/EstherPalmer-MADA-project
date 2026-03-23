###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels) #for modeling

#path to data
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","processed-data","processed_merged_data.rds")
data_location2 <- here::here("data","processed-data","processed_enviro_data.rds")
#load data. 
mydata <- readRDS(data_location1)
envirodata <- readRDS(data_location2)


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using complexity as an outcome and temp as a predictor
#Trying this as a baby one just bc I could see they're likely correlated

lmfit1 <- lm(complexity ~ temp, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  


############################
#### Correlation
#I know that generalized linear mixed models (glmms) assume none of your variables are correlated
#Given that I have 6 different measures of temperature in this study, I assume my variables are correlated
#(and that's not counting other things that might be correlated with temp like radiation)

cor(mydata$temp, mydata$max.temp)
#obviously correlated
#However I'd like a matrix of this especially one I can publish
#I will need to remove all non-numeric variables
#I am going to use the processed_enviro_data.rds since I then wont have to remove CSS data

df1 <- envirodata %>% select(-Day, -Date, - Weekday)
#This removes the non numeric variables that shouldn't have a correlation
cor_matrix <- round(cor(df1, use = "complete.obs"), digits = 3)
cor_matrix
#This I think ignores NA's
#This makes a matrix of correlations rounded to 3 digits!
#No idea what is signifigant tho or how to turn this into a pretty figure

#In other models I have had many hours long meetings discussing we used .75 as a cutoff for correlation
#I think I may stick to that here

#Of the 6 temperature variables (water temp, max air temp, min air temp, 2/4/8in soil temp)
#I will keep water temp (temp) since it's the temperature of the water where the sal is at time of sampling

df2 <- df1 %>% select(-max.temp, -min.temp, -twoinST, -fourinST, -eightinST)

cor_matrix2 <- round(cor(df2, use = "complete.obs"), digits = 3)
cor_matrix2
#This is much easier to read and could be turned into a figure

#I will need to remove: cond or TDS, radiation or ET
#I will remove ET from my model bc I don't think how much water evaporates from plants is physiologically relevent to salmonella
#This means I'm keeping radiation
#I will remove conductivity bc vibes
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(cor_matrix2, file = table_file1)
#I think this saves it as a table

lm_fit <- linear_reg() %>% 
  fit(complexity ~ TDS + pH + temp + depth + width + rel.humid + wind.speed + radiation + rain + turbidity + flow_avg,
  data = mydata)
tidy(lm_fit)