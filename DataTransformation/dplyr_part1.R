## if font is small, please let me know

## to change font size please go to Tools > Global Options > Appearance >font size


## if this is your first time here please install tidyverse package
install.packages("tidyverse")

## load tidyverse
library(tidyverse)

## today will work with the "diamonds" dataset already part of R 

## Recap from last workshop
## functions to look at dataset (gives you an idea what the dataset looks like)
View(diamonds)
head(diamonds)
tail(diamonds)
str(diamonds)


## Recap from ggplot workshop
ggplot (data = diamonds, mapping = aes(x=cut, fill= cut)) +
    geom_bar()


## lets say we only want to look or work with only Ideal cut
## we need to subset the data to only look at cut == Ideal
## we use the function filter
diamonds %>% 
    filter(cut=='Ideal') ##remember to use == for equality and put string in ''

## if you want to create a new dataset with this subset you simply assign it a name
IdealDiamonds <- diamonds %>% 
    filter (cut=='Ideal')

## you can also filter by the values of one or more columns:
## give me only diamonds that have color =='E' and price > 350
## 2 ways to do this:

diamonds %>% 
    filter(color=='E' , price>350) 

diamonds %>% 
    filter(color=='E' & price>350)  ## I like this one better 



## give me only diamonds that have color =='E' or price > 350
diamonds %>% 
    filter(color=='E' | price>350)   ## remember the "or" syntax |


## give everything but dont include when cut == 'Fair' 
diamonds %>% 
    filter(cut!='Fair')    ## remember not equal !=


######################################################################3

## summarize:

## You can use all kind of functions to summarize the data:
## There are many other summary statistics you could consider such:
## sd(), min(), max(), median(), sum(), 
## n() (returns the length of vector), first() (returns first value in vector), 
## last() (returns last value in vector) and n_distinct() (number of distinct values in vector).

#calculating the mean of the column price
diamonds %>% 
    summarize(mean(price))


## but what if you want the mean price for each cut group
#Group by one column and then summarizing with those groups
diamonds %>% 
    group_by(cut) %>% 
    summarize(mean(price))
## notice the result in the console
## it gave you a tibble with the summary and it created a column called `mean(price)`

## you can name that column if you want to, call it mean_price
diamonds %>% 
    group_by(cut) %>% 
    summarize(mean_price=mean(price))
## so summarize is usually paired with group_by to summarize individual groups

## lets count how many were in each group (call it total)
diamonds %>% 
    group_by(cut) %>% 
    summarize(mean_price=mean(price),total=n()) 
## we used the n() to count the total number in each group of cut (how many diamonds in each cut)

######################################################################
## mutate:
## lets say we want to calculate the price per caret and you want to store
## this result in a new column/variable
diamonds2 <- diamonds %>% 
    mutate(price_per_carat = price / carat)

## lets see what is looks like
str(diamonds2)
##notice we now have  11 variables


########################################################################
## arrange: reorder the rows by one or more columns

#reorder by the column "cut"
diamonds %>% 
    arrange(cut)


## reorder by the column cut and price. 
## To order in descending way, put the column in desc()
diamonds %>% 
    arrange(cut,desc(price))  ## so first it arranges by cut and then by price in descending order



## Lets chain/pipe functions
## Select all diamonds with a fair cut and then calucate the mean price (name it mean_price)
## depending on the clarity
diamonds %>% 
    filter(cut=='Fair') %>% 
    group_by(clarity) %>% 
    summarise(mean_price=mean(price))


##############################################################################33

### Challenge  
## Take 5 minutes to do this on your own:

## Select all Diamnons with the color 'H' and 
## calculate the the number of diamonds, depending on the clarity???
## name the number of diamonds : total







## Answer

diamonds %>% 
    group_by(cut) %>% 
    summarize(total = n())




########################################################################33
## Challenge 2
## Take 5 minutes to do this on your own:

## Select all diamonds with a number of carats bigger than 3. 
## Calculate the mean, median and sum of price and the number of diamonds depending on the cut. 
## give your summary names:
## mean_price
## median_price
## sum_price
## total

## then order the data depending on the number of diamonds.







## Answer
diamonds %>% 
    filter(carat>3) %>%
    group_by(cut) %>% 
    summarize(mean_price = mean(price),
              median_price = median(price),
              sum_price = sum(price),
              total=n()) %>% 
    arrange(total)




## next workshop will revisit these functions again and add some more
##THANK YOU VANESSA