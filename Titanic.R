

## Workshop 1
## August 29,2017

## Titanic data set
## create a project or create a session
## go to Kaggle.com and download Titatic data set, just the test.csv and train.csv
## take good care where you store/save the files. You want them in the project/session that you created

## We have .csv files, let R read them so we can play with the data
## Base R
BaseTest <- read.csv("test.csv")
BaseTrain <- read.csv("train.csv")

## the assign sign is <-  (shortcut: alt+- , alt key and the minus sign together)

## the tidyverse way...very similar
##if you have not installed tidyverse yet please do so now
## remember you install once, but you still need to call/load it
install.packages("tidyverse") ##install
library(tidyverse) ##load
TidyTest <- read_csv("test.csv") ## read file
TidyTrain <- read_csv("train.csv")


##some useful functions
View(BaseTest) # Invokes a spreadsheet-style data viewer on a matrix-like R object.
View(TidyTest)


dim(BaseTest) # returns a vector with the number of rows in the first element, and the number of columns as the second element (the dimensions of the object)
nrow(BaseTest) # returns the number of rows
ncol(BaseTest) # returns the number of columns

head(BaseTest) # shows the first 6 rows
tail(BaseTest) # shows the last 6 rows

names(BaseTest) # returns the column names 
rownames(BaseTest) # returns the row names (notice that sometimes there isnt row names)

str(BaseTest) # structure of the object and information about the class, length and content of each column
summary(BaseTest) # summary statistics for each column




## Since we are not doing Machine Learning lets combine the 2 datasets
## problem is test and train data don't have the same variable count, test does not have Survived

##lets create a new variable in test called Survived using Base R syntax
BaseTestSurvived<- data.frame(Survived = rep("NA", nrow(BaseTest)), BaseTest[,])
## create a new Data frame called BaseTestSurvived, add a new variable Survived that will assign NA to it, a number of times = to the number of rows, then combine it with BaseTest. Notice the [,]



##Lets do it using R tidyverse
TidyTest <- TidyTest %>%
    mutate(Survived =NA) 
## %>% is called piping (shortcut: Ctrl + Shift + M if you have a PC or Cmd + Shift + M if you have a Mac)


## notice the order of the variables/columns
## Do u want the new Variable Survived to be in the front??
TidyTest <- TidyTest %>%
    mutate(Survived =NA) %>%
    select(Survived, everything())


## combine the 2 datasets:
## Base R
Base.Combined <- rbind(BaseTrain, BaseTestSurvived)


## Tidyverse:
Tidy.Combined <- bind_rows(TidyTrain,TidyTest)

## they looks similar
## but we could have done it all in one shot with tidyverse
Tidy.Combined <-  TidyTest %>%
    mutate(Survived =NA) %>%
    select(Survived, everything()) %>%
    bind_rows(TidyTrain)





##lets look at our data
## use str() function to look at the structure of dataset

str(Tidy.Combined)

str(Base.Combined)

##what is a factor?
## what is an integer, why not call it a number?
## should we change the class of the variables?


## Exploratory Analysis:


##lets look how many survived and how many died?
# Base R
table(Base.Combined$Survived)
## Pclass
table(Base.Combined$Pclass)

#Tidyverse
count(Tidy.Combined,Survived,Pclass) ##I can do it in one shot

#plot
ggplot(Tidy.Combined, aes(x = Pclass, fill = factor(Survived))) + ##notice we didn't specify y-axis because
    ## we are using bar-plot (bar plots counts how many passengers in each class)
    geom_bar() +  ## bar pplot
    xlab("Pclass") + ##label the x-axis
    ylab("Total Count") + ##label the y-axis
    labs(fill = "Survived")  ## label the different colors/fill

## if you dont want to see missing values NA
##  simply just plot the Train set (remember the Test set had all the NA's)
ggplot(TidyTrain, aes(x = Pclass, fill = factor(Survived))) + ##notice we don't specify y-axis because
    ## we are using bar-plot (bar plots counts how many passengers in each class)
    geom_bar() +  ## bar pplot
    xlab("Pclass") + ##label the x-axis
    ylab("Total Count") + ##label the y-axis
    labs(fill = "Survived")  ## label the different colors/fill



###########################################
## We didnt get a chance to talk about this:

##lets check if there is no duplicate names
## Base R

length(unique(as.character(Base.Combined$name)))

duplicated(Tidy.Combined$Name) ##this gives you TRUE FALSE
anyDuplicated(Tidy.Combined$Name)

##since its a False/True 
# First, get the duplicate names and store them as a vector
dup.names <- as.character(Tidy.Combined[which(duplicated(as.character(Tidy.Combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
Tidy.Combined[which(Tidy.Combined$Name %in% dup.names),]



##Tidyverse:
Tidy.Combined %>% 
    group_by(Name)%>%
    filter (n()>1)  ##give me any group of names that have more than one value

## n() the number of observations in the curent group


