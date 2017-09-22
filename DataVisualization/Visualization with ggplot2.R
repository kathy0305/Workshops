### Data Visualization with ggplot2

####Recap

*Create a project or new script
*load tidyverse 
*Some useful functions
getwd()
setwd()
head()
tail()
str()
summary()
View()



### Visualization using ggplot2
library(tidyverse)

## We are going to be using the diamonds dataset that is already preloaded 
## as part of the tidyverse pkg
View(diamonds) ## opens a new spreadsheet-style viewer of the data

str(diamonds)  ## basic structure of the data
head(diamonds)  ## gives you the 6 rows of the date (6 rows is the default)
##if you want to change the number of rows you simply add another argumnet
head(diamonds, n=10) 
## or simply 
head(diamonds, 10)

tail(diamonds) # gives you the last 6 rows
summary(diamonds) ## gives you generic summary about the variables like mean, min max..etc
## note that summary function has different outputs depending on what kind of
## object it takes as an argument.



## with ggplot2 you begin every plot with the function ggplot()
## this creates a coordinate system that you can add layers to

## first argument of the function ggplot() is the data set
## what data do you want me to visualize
## try this :
ggplot(data = diamonds)
## it creates an empty graph, ok you want to use diamonds data set, but what else??

## will need to add another layer, maybe say what kind of graph?
## you do that by geom_ function
## a geom (geometric object) defines how do you want to display the data
## type geom_ in your console and see the options


## lets now try this
ggplot(data= diamonds)+
    geom_point()

## we get an error, we are missing aesthetics:
## Aesthetic mappings describe how variables in the data are mapped to visual properties
## (aesthetics) of geoms. Aesthetic mappings can be set in ggplot2 and in individual layers.

## Basically its asking what variables do you want me to use:
ggplot(data= diamonds)+
    geom_point(mapping = aes(x=carat, y=price))

## you can also add aesthetics  to the ggplot function
ggplot(data=diamonds, mapping=aes(x=carat, y=price))+
    geom_point()   ## I personally like this way better
                   

## lets try a diffrent geom
ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut))
## hey how come you didnt specify y=??
## because Bar charts and histograms bin your data, they already 'count' the points;
## how many are in each classification
## this is done automatically with the stat_count() (its done behind the scene)
## try this
ggplot(data= diamonds)+
    stat_count(mapping = aes(x=cut))

## you can flip the coordinates if you want
ggplot(data= diamonds)+
    stat_count(mapping = aes(x=cut))+
    coord_flip()

## a bit boring graph
## lets add some color

## thats is the beauty of ggplot2 you can 'add' stuff as you go
ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, color=cut))


## notice that it only added a border color
## fill is a much useful aesthetic

ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, fill=cut))

## it looks prettier, but it didnt add much value of what is going on

## lets add a new varible


ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, fill=clarity))


ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, fill=clarity))


## gives you a better idea , of the proportion of each clarity classification
## but its hard to see the exact proportion
## we can fix that by using the argument 'position'

?position  ## read the help section about position

## lets try position="fill"
ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, fill=clarity),
             position = "fill")

## nice to see the proportion, but we lost the count

## lets try position ="dodge"
ggplot(data= diamonds)+
    geom_bar(mapping = aes(x=cut, fill=clarity),
             position = "dodge")
## cut vs clarity is not telling us much
## lets change x
ggplot(data= diamonds, aes(x=price, fill=clarity)) +
    geom_histogram()
## note i changed few things, what???


## a bar chart was nice to begin with but I am still not getting 
## a good sense of the relation between all these variables
## lets go back to geom_point 

ggplot(data= diamonds, mapping= aes(x=carat,y=price))+
    geom_point()
    
## add color
ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + 
    geom_point()

## lets play around with the other variables
## change color argument to other variables
ggplot(diamonds, aes(x=carat, y=price, color=color)) + 
    geom_point()
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
    geom_point()

ggplot(diamonds, aes(x=carat, y=price, color=clarity, size=cut)) + #add size argument
    geom_point()


ggplot(diamonds, aes(x=carat, y=price, color=clarity, shape=cut)) + #add shape argument
    geom_point()
## Now every shape represents a different cut of the diamond.



##still having issues seeing a pattern, its pretty busy and I cant seems to see a trend

## to see general trend of the data we might want to add  a smoothing curve.
## "In statistics and image processing, to smooth a data set is to create an approximating function that 
## attempts to capture important patterns in the data, 
## while leaving out noise or other fine-scale structures/rapid phenomena. 
## In smoothing, the data points of a signal are modified so individual points (presumably because of noise) are reduced, 
## and points that are lower than the adjacent points are increased leading to a smoother signal.
## "Wkipedia

## we do that by adding a layer called geom_smooth.
ggplot(diamonds, aes(x=carat, y=price)) +
    geom_point() +
    geom_smooth()
## The gray area around the curve is a confidence interval, 
##suggesting how much uncertainty there is in this smoothing curve.

## if you only want to see the smoothing curve simply remove geom_point
ggplot(diamonds, aes(x=carat, y=price)) +
    #geom_point() +
    geom_smooth()
## I use # to simply treat it as a comment instead of removing it

## this is certainly an improvement but linear model would have given us a better idea
ggplot(diamonds, aes(x=carat, y=price)) + 
    geom_point() + 
    geom_smooth(se=FALSE, method="lm") ## se=FALSE gets rid of the grey area (standard error) 
## and method="lm" tells geom_smooth to use "lm" (linear model)



## if the plot is busy and you are just interested in the linear model, remove the scatter points
ggplot(diamonds, aes(x=carat, y=price)) + 
    geom_smooth( method="lm") 


## lets try a different geom
## another common one is boxplot
## "The box plot (a.k.a. box and whisker diagram) is a standardized way of displaying 
## the distribution of data based on the five number summary: minimum, first quartile, median, third quartile, and maximum"

ggplot(data= diamonds, mapping =aes(x=color, y=price)) +
    geom_boxplot()

## as we already seen, there seems to be  lots of outliers and long tails (wide range),
## again, if you're intrested in fraud this is a good thing, but for summary info , sometimes
## its better to get rid (visually) of the outliers so that you can make a better sense of the 
## distribution of the variable, in this case the price
## you do that by adding scale_y_log 
ggplot(data=diamonds, mapping= aes(x=color, y=price)) + 
    geom_boxplot() + 
    scale_y_log10()

## you might also see this version
ggplot(data= diamonds, mapping= aes(color, log10(price))) +
    geom_boxplot()

## Logarithm scales are often used when there is a large range of values
## involved with the variables under consideration. 
## i like this explanation:
## https://mathspace.co/learn/world-of-maths/logarithms/logarithmic-scales-25013/logarithmic-scales-1081/



#######################################################################

## BEYOND THE WORKSHOP

## We talked about the variety of pkgs available in R
## there is a pkg called "plotly" that  translate 'ggplot2' graphs 
## to an interactive web-based version and/or 
## create custom web-based visualizations directly from R

## so it uses the same syntax/command as ggplot2, but shows an interactive 
## plot instead

## first install plotly
## you can do so by going to Tools>Install Packages > plotly
## using the command
install.packages("plotly")
library(plotly)

## now whatever you want to graph , just use the function ggplotly
## try this and then place your cursor over the graph
ggplotly(ggplot(data= diamonds, mapping =aes(x=color, y=price)) +
    geom_boxplot())
## cool, no??? try it with the different graphs that we worked on previously

## so ggplotly simply converts ggplot object to plotly object

## its a good practice to give your plot a name and then use ggplotly
BarDiamonds <- ggplot(data= diamonds)+
                    geom_bar(mapping = aes(x=cut, fill=clarity),
                    position = "fill")

ggplotly(BarDiamonds )

## as you can see plotly is good when you are making web-based presentation
## for your everyday explanatory analysis ggplot2 alone should suffice.

############################################################3
## Remember to never save your workspace
## as R is powerful and remebers stuff from past sessions
## my advice is to have that done in your global option.
## Tools > Global Options > R Genearl > Save workspace to .RData on exit> Never
