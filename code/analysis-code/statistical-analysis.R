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
#library(randomForest) #for randomforest modeling
library(caret) #modeling
library(ranger) #random forest modeling but different
library(corrplot) #to make a correlation plot of my variables
library(vip) #for seeing random forest results

#path to data
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","processed-data","processed_merged_data.rds")
data_location2 <- here::here("data","processed-data","processed_enviro_data.rds")
data_location3 <- here::here("data","processed-data","processed_CSS_data.rds")
#load data. 
mydata <- readRDS(data_location1)
envirodata <- readRDS(data_location2)
CSSdata <- readRDS(data_location3)


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

df4$complexity <- as.integer(df4$complexity)
df5 <- CSSdata %>% select(-complexity) # Marco Reina, I added this line here

rngseed <- 1234
set.seed(rngseed)

train <- df4
test <- df5 
#I will train on the daily, test on the weekly

lm_mod <- linear_reg()

folds <- vfold_cv(train, v = 10)
folds

#This is the full model with all predictors, I will also test models with various combinations of predictors
lm_wf1 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ TDS + pH + temp + depth + width + rel.humid + wind.speed + radiation + rain + turbidity + flow_avg)
lm_fit1 <- lm_wf1 %>% fit(train)
tidy(lm_fit1)
lm_train_pred1 <- predict(lm_fit1, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred1 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_fit_cv1 <- lm_wf1 %>% 
  fit_resamples(folds, control = control_resamples(save_pred = TRUE, save_workflow = TRUE, extract = I))
lm_fit_cv1
collect_metrics(lm_fit_cv1)

lm_pred1 <- collect_predictions(lm_fit_cv1)

lm_p1 <- lm_pred1 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p1

#removing radiation
lm_wf2 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ TDS + pH + temp + depth + width + wind.speed + rel.humid + rain + turbidity + flow_avg)
lm_fit_cv2 <- lm_wf2 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE, save_workflow = TRUE))
lm_fit_cv2
collect_metrics(lm_fit_cv2)

lm_fit2 <- lm_wf2 %>% fit(train)
tidy(lm_fit2)
lm_train_pred2 <- predict(lm_fit2, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred2 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred2 <- collect_predictions(lm_fit_cv2)

lm_p2 <- lm_pred2 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p2
#This has a lower rmse

#removing rel.humid and radiation
lm_wf3 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ TDS + pH + temp + depth + width + wind.speed + rain + turbidity + flow_avg)
lm_fit_cv3 <- lm_wf3 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv3
collect_metrics(lm_fit_cv3)

lm_fit3 <- lm_wf3 %>% fit(train)
tidy(lm_fit3)
lm_train_pred3 <- predict(lm_fit3, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred3 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred3 <- collect_predictions(lm_fit_cv3)

lm_p3 <- lm_pred3 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p3

#removing rel.humid, radiation, and depth
lm_wf4 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ TDS + pH + temp + width + wind.speed + rain + turbidity + flow_avg)
lm_fit_cv4 <- lm_wf4 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv4
collect_metrics(lm_fit_cv4)

lm_fit4 <- lm_wf4 %>% fit(train)
tidy(lm_fit4)
lm_train_pred4 <- predict(lm_fit4, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred4 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred4 <- collect_predictions(lm_fit_cv4)

lm_p4 <- lm_pred4 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p4

#removing rel.humid, radiation, depth, and width
lm_wf5 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ TDS + pH + temp + wind.speed + rain + turbidity + flow_avg)
lm_fit_cv5 <- lm_wf5 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv5
collect_metrics(lm_fit_cv5)

lm_fit5 <- lm_wf5 %>% fit(train)
tidy(lm_fit5)
lm_train_pred5 <- predict(lm_fit5, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred5 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred5 <- collect_predictions(lm_fit_cv5)

lm_p5 <- lm_pred5 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p5

#removing rel.humid, radiation, depth, width, and TDS
lm_wf6 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ pH + temp + wind.speed + rain + turbidity + flow_avg)
lm_fit_cv6 <- lm_wf6 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv6
collect_metrics(lm_fit_cv6)

lm_fit6 <- lm_wf6 %>% fit(train)
tidy(lm_fit6)
lm_train_pred6 <- predict(lm_fit6, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred6 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred6 <- collect_predictions(lm_fit_cv6)

lm_p6 <- lm_pred6 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p6

#removing rel.humid, radiation, depth, width, TDS, and flow average
lm_wf7 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ pH + temp + wind.speed + rain + turbidity)
lm_fit_cv7 <- lm_wf7 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv7
collect_metrics(lm_fit_cv7)

lm_fit7 <- lm_wf7 %>% fit(train)
tidy(lm_fit7)
lm_train_pred7 <- predict(lm_fit7, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred7 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred7 <- collect_predictions(lm_fit_cv7)

lm_p7 <- lm_pred7 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p7

#removing rel.humid, radiation, depth, width, TDS, flow average, and temp
lm_wf8 <- workflow() %>% add_model(lm_mod) %>% 
  add_formula(complexity ~ pH + wind.speed + rain + turbidity)
lm_fit_cv8 <- lm_wf8 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
lm_fit_cv8
collect_metrics(lm_fit_cv8)

lm_fit8 <- lm_wf8 %>% fit(train)
tidy(lm_fit8)
lm_train_pred8 <- predict(lm_fit8, train) %>% bind_cols(train %>% select(complexity))

lm_train_pred8 %>% yardstick::rmse(truth = complexity, estimate = .pred)

lm_pred8 <- collect_predictions(lm_fit_cv8)

lm_p8 <- lm_pred8 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
lm_p8

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

rf_mod1 <- rand_forest(trees = 1000) %>% set_engine("ranger", seed = rngseed) %>% set_mode("regression")
rf_wf1 <- workflow() %>% add_model(rf_mod1) %>% add_formula(complexity ~ TDS + pH + temp + depth + width + rel.humid + wind.speed + radiation + rain + turbidity + flow_avg)
rf_fit_cv1 <- rf_wf1 %>% fit_resamples(folds, control = control_resamples(save_pred = TRUE))
rf_fit_cv1
collect_metrics(rf_fit_cv1)

rf_pred1 <- collect_predictions(rf_fit_cv1)

rf_p1 <- rf_pred1 %>% ggplot(aes(x=complexity, y=.pred)) + geom_point() + xlim(1, 11) + ylim(1, 11)
rf_p1

#rf_train_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)
############################################################
# Marco Reina rf_train_pred seems like it is not defined ###
############################################################

#rf1_tree <- rf_fit %>% extract_fit_parsnip() %>% vip::vip()
#rf1_tree


#rf_test_pred <- predict(rf_fit, test) %>%
  #bind_cols(test %>% select(complexity))

#rf_test_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)

#####################################################
# Marco Reina rf_fit seems like it is not defined ###
#####################################################

#This still looks like a better rmse than the linear model?
#have no idea what my acutal model looks like though

#I think that while this random forest model is maybe a better fit than my linear model,
#since I can't seem to learn the final formula, it may be better to stick with my linear model
#since I do need to know what my predictors are

############################
#### Serovar correlation

#I would like to know if the presense of any serovar correlates with each other
df5 <- CSSdata %>% select(-complexity)
df6 <- df5 %>% ungroup() %>% select(-Day)
#creating a data frame of only serovar data so that I can use it in a corrplot
cor_matrix3 <- round(cor(df6, use = "complete.obs"), digits = 3)
cor_matrix3

cor_plot4 <- corrplot::corrplot(cor(df6, use = "complete.obs"), method = "number", type = "upper")
cor_plot4
#I think it looks like the only real correlations seen are between serovars that do not occur frequently
#I will count the top 10 serovars and create a new corrplot

# df7 <- colSums(df6[c("Anat", "AquaInve")] >0)

df7 <- df6 


#set.seed(222)
#ind <- sample(2, nrow(df4), replace = TRUE, prob = c(.8, .2))
#train <- df4[ind==1,]
#test <- df4[ind==2,]
#So from what I can tell, I assigned 80% of the data to train, and 20% to test

#lm_fit <- linear_reg() %>% set_engine("glm") %>%
#  fit(complexity ~ TDS + pH + temp + depth + width + rel.humid + wind.speed + radiation + rain + turbidity + flow_avg,
#  data = train)
#tidy(lm_fit)
#This is interesting but far from perfect
#Should maybe run a random forest whatever to select variables for use
#This also doesn't show a good relationship between temp and complexity despite my dotplots showing one
#lm_train_pred <- predict(lm_fit, train) %>% 
#  bind_cols(train %>% select(complexity))

#lm_train_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)

#lm_test_pred <- predict(lm_fit, test) %>% 
#  bind_cols(test %>% select(complexity))

#lm_test_pred %>% yardstick::rmse(truth = complexity, estimate = .pred)
#better rmse on the test than the training which is interesting

#ok so for now I will need to split into test and training data
#I do actually have a second dataset that I can use from this same site with all the same variables just sampling weekly instead of daily
#and I plan to use that second data set to train my model
#however I don't know where exactly that is and I don't want to go searching I also don't know if it's fully finished
#will ask undergrads later
#so for now split into training and test data, redo better later