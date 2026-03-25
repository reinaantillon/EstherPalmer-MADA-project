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
library(randomForest) #for randomforest modeling
library(caret) #modeling
library(ranger) #random forest modeling but different
library(corrplot) #to make a correlation plot of my variables

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
#the default is the pearson correlation which is used for quantitative continuous variables with a linear relationship
#I'd say that applies here

cor_plot1 <- corrplot::corrplot(cor(df1, use = "complete.obs"), method = "number", type = "upper")
cor_plot1

#In other models I have had many hours long meetings discussing we used .75 as a cutoff for correlation
#I think I may stick to that here

#Of the 6 temperature variables (water temp, max air temp, min air temp, 2/4/8in soil temp)
#I will keep water temp (temp) since it's the temperature of the water where the sal is at time of sampling

df2 <- df1 %>% select(-max.temp, -min.temp, -twoinST, -fourinST, -eightinST)

cor_matrix2 <- round(cor(df2, use = "complete.obs"), digits = 3)
cor_matrix2
cor_plot2 <- corrplot::corrplot(cor(df2, use = "complete.obs"), method = "number", type = "upper")
cor_plot2
#This is much easier to read and could be turned into a figure

#I will need to remove: cond or TDS, radiation or ET
#I will remove ET from my model bc I don't think how much water evaporates from plants is physiologically relevent to salmonella
#This means I'm keeping radiation
#I will remove conductivity bc vibes, will find a more scientific method later
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(cor_matrix2, file = table_file1)
#I think this saves it as a table

df3 <- df2 %>% select(-ET, -COND)
cor_plot3 <- corrplot::corrplot(cor(df3, use = "complete.obs"), method = "number", type = "upper")
cor_plot3

df4 <- mydata %>% select(-ET, -COND, -Day, -Date, - Weekday, -max.temp, -min.temp, -twoinST, -fourinST, -eightinST)
df4 <- df4 %>% select(-Anat, -AquaInve, -BrazI, -Brae, -Infa, -MontII, -MuenI, -Mues, -Rubi, -Typm, -Gamn, -GiveI, 
  -NewpII, -MissII, -MontI, -Hart, -Agbe, -Hada, -Mine, -Oran, -SaitII, -KisrI, -MbanI, -Luci, -BertBuda, -MuenII)
#note to self: rename saintpaul later so there's II instead of ll

df4$complexity <- as.integer(df4$complexity)
set.seed(222)
ind <- sample(2, nrow(df4), replace = TRUE, prob = c(.8, .2))
train <- df4[ind==1,]
test <- df4[ind==2,]
#So from what I can tell, I assigned 80% of the data to train, and 20% to test

lm_fit <- linear_reg() %>% set_engine("glm") %>%
  fit(complexity ~ TDS + pH + temp + depth + width + rel.humid + wind.speed + radiation + rain + turbidity + flow_avg,
  data = train)
tidy(lm_fit)
#This is interesting but far from perfect
#Should maybe run a random forest whatever to select variables for use
#This also doesn't show a good relationship between temp and complexity despite my dotplots showing one
lm_train_pred <- predict(lm_fit, train) %>% 
  bind_cols(train %>% select(complexity))

lm_train_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_test_pred <- predict(lm_fit, test) %>% 
  bind_cols(test %>% select(complexity))

lm_test_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)
#better rmse on the test than the training which is interesting

#ok so for now I will need to split into test and training data
#I do actually have a second dataset that I can use from this same site with all the same variables just sampling weekly instead of daily
#and I plan to use that second data set to train my model
#however I don't know where exactly that is and I don't want to go searching I also don't know if it's fully finished
#will ask undergrads later
#so for now split into training and test data, redo better later

############################
#### Random Forest attempt

#I don't know that this is going well, may try something else?
#I know previous lab members have used this to narrow down the amount of variables in their data


#rf <- randomForest(complexity~., data=train, na.action = na.roughfix)
#print(rf)
#na.roughfix estimates missing values, which I'm hoping will lead to less nas in the prediction

#pred1 <- predict(rf, train)
#there's a lot of NA's
#it did not like that complexity and pred1 are not factors
#It further does not like that they don't match
#confusionMatrix(as.factor(pred1), as.factor(train$complexity))

#ok I found a different package lets try this maybe it will hate me less

rf_mod <- rand_forest(trees = 1000) %>% set_engine("ranger") %>% set_mode("regression")
rf_fit <- rf_mod %>% fit(complexity~., data = train)
rf_fit

rf_train_pred <- predict(rf_fit, train) %>% 
  bind_cols(train %>% select(complexity))
#unlike my last random forest this is predicting less NA's which is good

rf_train_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)

rf_test_pred <- predict(rf_fit, test) %>%
  bind_cols(test %>% select(complexity))

rf_test_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)
#This still looks like a better rmse than the linear model?
#have no idea what my acutal model looks like though

#I think that while this random forest model is maybe a better fit than my linear model,
#since I can't seem to learn the final formula, it may be better to stick with my linear model
#since I do need to know what my predictors are

############################
#### Serovar correlation

#I would like to know if the presense of any serovar correlates with each other