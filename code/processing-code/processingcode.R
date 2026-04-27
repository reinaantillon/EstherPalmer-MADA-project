###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed-data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html
###############################


## ---- packages --------
#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(ggplot2) #for figures
library(janitor) #for naming columns as rows

## ---- loaddata --------
#path to data
#note the use of the here() package and not absolute paths
enviro_data_location <- here::here("data","raw-data","Environmental_Data_Final.xlsx")
CSS_data_location <- here::here("data", "raw-data", "Daily_Sampling_Normalized_CSS_6mo.xlsx")
weekly_data_location <- here::here("data", "raw-data", "Weekly_CSS_and_Environmental_Data.xlsx")
#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
enviro_rawdata <- readxl::read_excel(enviro_data_location, sheet = "Sheet2")
#I am going to use the dataset that excludes weekends for this preliminary look, Sheet1 contains the data that has weekends
CSS_rawdata <- readxl::read_excel(CSS_data_location, col_names = FALSE)
#This will read in the CSS data
weekly_weather_rawdata <- readxl::read_excel(weekly_data_location, sheet = "Weather")
weekly_CSS_rawdata <- readxl::read_excel(weekly_data_location, sheet = "CSS", col_names = FALSE)
#My undergrads did not put it all in one sheet for me :/
#As such I will load each sheet separately and merge later

## ---- exploredata --------
#take a look at the data
dplyr::glimpse(enviro_rawdata)
#another way to look at the data
summary(enviro_rawdata)
#The summary seems to be reading some of the data as characters rather than values
#yet another way to get an idea of the data
head(enviro_rawdata)
#this is a nice way to look at data
skimr::skim(enviro_rawdata)
#look in the Codebook for a variable explanation
#print(codebook)

#fig1 <- CSS_rawdata %>% ggplot(aes(x=)) + geom_area()
#plot(fig1)


## ---- cleandata1 --------
d1 <- dplyr::select(enviro_rawdata, -DO, -SALT)
#removes dissolved oxygen from the data set since I belive the instrument stopped working
#the - means remove
#also removed salinity bc there was only one instance where it was not 0

## ---- cleandata2 --------
d1 <- d1 %>% dplyr::rename(
  flow1 = FLOW...10,
  flow2 = FLOW...11,
  flow3 = FLOW...12
)
#renames these columns bc idk what this is
flow <- subset(d1, select = c(flow1, flow2, flow3))
#creates a dataframe with just the 3 flow variables
#this is a much nicer method than my first attempts bc it keeps the variable names!
d1$flow_avg <- rowMeans(flow)
#this creates a variable in d1 that is the average of all the flow measurements (we took 3)
d2 <- select(d1, -flow1, -flow2, -flow3)
#now that we have the average we don't need the individual flow measurements

## ---- cleandata3 --------
d2$Day <- as.character(d2$Day)
#Makes this a character rather than a continuous varible
d2$COND <- na_if(d2$COND, c("pH METER BROKE"))
#turns the string into an NA
#in case it was not obvious, our pH meter broke so we had some NA's
d2$COND <- as.numeric(d2$COND)
#This makes conductivity a numeric variable

#renaming everything to make it easier
d3 <- d2 %>% rename(
  depth = `DEPTH (cm)`,
  max.temp = `Max Temp`,
  min.temp = `Min Temp`,
  rel.humid = `R.H (%)`,
  temp = `TEMP ©`,
  wind.speed = `Wind Speed (mph)`,
  turbidity = TURBIDITY,
  width = WIDTH,
  twoinST = `2 Inch Soil Temp`,
  fourinST = `4 Inch Soil Temp`,
  eightinST = `8 Inch Soil Temp`,
  radiation = `Total Radiation (MJ/m2)`,
  rain = `Rain (in)`,
  ET = `ET (in)`
)
d3$Weekday <- as.factor(d3$Weekday)
#this makes it so that it's a factor not a character

skim(d3)

## ---- cleandata4 --------
#there is one in depth that is 3in so I need to convert that to cm then make everything numbers
x <- 3*2.54
#math to covert 3in to cm
d3[17, 9] = "7.62"
#can't replace with the number since its still regarded as a character string so replaced with string
d3$depth <- as.numeric(d3$depth)

d3$wind.speed <- na_if(d3$wind.speed, c("NA"))
d3$wind.speed <- as.numeric(d3$wind.speed)
d3$turbidity <- na_if(d3$turbidity, c("NA"))
d3$turbidity <- as.numeric(d3$turbidity)
#looks like not all NA were marked as NA some were as characters
#made these numeric bc they are

skim(d3)

## ---- cleandata5 --------
#now the only thing left is to convert width from ft in format into a number of cm
w1 <- subset(d3, select = c(width))
w2 <- w1 %>% separate(width, c('feet', 'inches'), "'", convert = TRUE)
#this separates feet and inches into separate columns
w2$inches <- gsub('"', '', w2$inches)
#this removes the " on inches
w2$inches <- as.numeric(w2$inches)
w2$inches[is.na(w2$inches)] <- 0
#makes inches numeric, replaces NA with 0
w2[78, 2] = NA
#adds back in the one actual NA in this dataset

w3 <- w2%>% mutate(cm = (12*feet + inches)*2.54)
#creates a df with a column for cm

d3$width <- w3$cm
#replaces the column in the whole dataframe with the one in cm from w3

skim(d3)

## ---- cleandata6 --------
#It will be easier if the CSS data is transposed so I can match days from the CSS data to the days from weather data
d4 <- as.data.frame(CSS_rawdata)
d4 <- t(d4)
d4 <- as.data.frame(d4)
#without this last command I wasn't actually able to see anything in the matrix which concerned me
d5 <- d4 %>% janitor::row_to_names(row_number = 1)
#made row 1 (my serovar names) into the column!
d5 <- d5 %>% rename(
  Day = GeneID
)
#This renamed GeneID (an artifact of DeSeq2) to Day
#This will allow me to match CSS data to environmental data

d5$Day <- gsub("D", "", d5$Day)
#This removed the D in front of the numbers

d5$Day <- as.character(d5$Day)
#again so this matches the other sheet

skim(d5)

## ---- cleandata7 --------
#It looks like everything is a character, which is incorrect
d6 <- d5 %>% mutate_at(vars(Anat, AquaInve, BrazI, Brae, Infa, MontII, MuenI, Mues, Rubi, Typm, Gamn, GiveI, NewpII, MissII, MontI, Hart, Agbe, Hada, Mine, Oran, Luci, BertBuda), as.numeric)
d6 <- d6 %>% rename(
  KisrI = Kisrl,
  MbanI = Mbanl,
  SaitII = Saitll,
  MuenII = Muenll
)
#someone named a couple of the polyphyletic serovars using an L instead of roman numerals lol

d6 <- d6 %>% mutate_at(vars(KisrI, MbanI, MuenII, SaitII), as.numeric)

## ---- complexity --------
d7 <- d6 %>% rowwise(Day)
d7 <- d7 %>% mutate(complexity = sum(c_across(Anat:MuenII) >0, na.rm=TRUE))
#This counts all of the serovars in a sample (or # of instances where proportion >0)
#complexity is what I will be modeling for

## ---- joindata --------
#It will be helpful if these two data sets can go in one sheet
d8 <- merge(d3, d7, by = "Day")
d8$Day <- as.integer(d8$Day)


## ---- weeklydataweather --------
#So I want to train my models on the daily sampling data and test them on the weekly sampling data.
#note that I have more weekly CSS that still needs processing and will be added later
#Here I will clean my weekly sampling data in the same way I did the daily
#Looks like I need to transpose the CSS data and clean up some of the extra stuff
#For the weather I will need to clean up all the other variables the same
d9 <- select(weekly_weather_rawdata, -DO, -SALT)
d9 <- d9 %>% rename(
  flow1 = FLOW...9,
  flow2 = FLOW...10,
  flow3 = FLOW...11,
  depth = `DEPTH (cm)`,
  max.temp = `Max Temp`,
  min.temp = `Min Temp`,
  rel.humid = `R.H (%)`,
  temp = `TEMP ©`,
  wind.speed = `Wind Speed (mph)`,
  turbidity = TURBIDITY,
  width = WIDTH,
  twoinST = `2 Inch Soil Temp`,
  fourinST = `4 Inch Soil Temp`,
  eightinST = `8 Inch Soil Temp`,
  radiation = `Total Radiation (MJ/m2)`,
  rain = `Rain (in)`,
  ET = `ET (in)`
)
#rename columns, so it's easier to deal with them
flow_avg2 <- subset(d9, select = c(flow1, flow2, flow3))
#creates a dataframe with just the 3 flow variables
#this is a much nicer method than my first attempts bc it keeps the variable names!
d9$flow_avg <- rowMeans(flow_avg2)
#this creates a variable in d1 that is the average of all the flow measurements (we took 3)
d10 <- select(d9, -flow1, -flow2, -flow3, -`Average Flow`)
#now that we have the average we don't need the individual flow measurements
d10 <- d10 %>% select(-Complexity...26, -Complexity...2)
#This is complexity calculated from the CSS data, but I think it's better to remove it and recalculate it
#need to rename column 1 to Day, and change it from a number to whatever I made it in the other data sets
d10 <- d10 %>% rename(
  Day = '...1'
)
d10$Day <- as.character(d10$Day)
#still need to fix width
#there is one depth that is in inches
#windspeed is a string currently

wd1 <- subset(d10, select = c(width))
wd2 <- wd1 %>% separate(width, c('feet', 'inches'), "'", convert = TRUE)
#this separates feet and inches into separate columns
wd2$inches <- gsub('"', '', wd2$inches)
#this removes the " on inches
wd2$inches <- as.numeric(wd2$inches)
wd2$inches[is.na(wd2$inches)] <- 0
#makes inches numeric, replaces NA with 0
wd3 <- wd2%>% mutate(cm = (12*feet + inches)*2.54)
#creates a df with a column for cm
d10$width <- wd3$cm
#replaces the column in the whole dataframe with the one in cm from w3

d10$depth <- as.numeric(d10$depth)
d10[13, 6] = 1.2*2.54
#this still doesn't look right I have texted undergrads to confirm that they measured 1.2 inches
#either that or it could be 1.2 feet, or 1 foot 2 in
#will leave for now

d10$wind.speed <- as.numeric(d10$wind.speed)

#Now this all looks good!

## ---- weeklydataCSS --------
d11 <- as.data.frame(weekly_CSS_rawdata, headers = FALSE)
d11[1, 1] = "Day"
d11 <- t(d11)
d11 <- as.data.frame(d11)
#without this last command I wasn't actually able to see anything in the matrix which concerned me
d12 <- d11 %>% janitor::row_to_names(row_number = 1)

d12 <- d12 %>% select(-Sum)
d12 <- dplyr::filter(d12, Day != "Frequency")
d12$Day <- as.integer(d12$Day)
d12$Day <- as.character(d12$Day)

d12 <- d12 %>% rename(
  BrazI = `Brazil I`,
  MontII = `Montevideo II`,
  MuenI = `Muenchen I`,
  Rubi = Rubislaw,
  GiveI = `Give I`,
  NewpII = `Newport II`,
  Hart = Harford,
  CerrI = `Cerro I`,
  Schw = Schwarzengrund,
  DKPPR = DurbKokmPanaPomoReadII,
  Typm = Typhimurium
)

d12 <- d12 %>% mutate_at(vars(BrazI, MontII, MuenI, Rubi, Typm, GiveI, NewpII, Hart, CerrI, Schw, DKPPR), as.numeric)
d12$Complexity <- as.integer(d12$Complexity)

#Now this looks good! The only issue is that it seems like there are more CSS days than weather days.
#I have asked Dawson for the remaining weather days
#Once I have those I'll merge the CSS and weather data together



## ---- savedata --------
processed_enviro_data <- d3
processed_CSS_data <- d7
merged_data <- d8
# location to save file
save_data_location1 <- here::here("data","processed-data","processed_enviro_data.rds")
save_data_location2 <- here::here("data","processed-data","processed_CSS_data.rds")
save_data_location3 <- here::here("data","processed-data","processed_merged_data.rds")
saveRDS(processed_enviro_data, file = save_data_location1)
saveRDS(processed_CSS_data, file = save_data_location2)
saveRDS(merged_data, file = save_data_location3)

## ---- notes --------
