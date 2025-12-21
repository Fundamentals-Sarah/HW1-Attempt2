## in R, 
## read file from a URL, from github
## file is in **excel**
## create the object '**fragility23**' to hold the data

# use the code in line 8 to give you a clean slate to work from

rm(list = ls())

# import your data

# open a file from a url (file in Excel), name it 'fragility23'
linkGit='https://github.com/Fundamentals-Sarah/HW1-Attempt2/raw/refs/heads/main/FSI-2023-DOWNLOAD.xlsx'

# check that the data object was added to the Environment tab to the right under Data

# install.packages('rio') in order to conduct your analyses, this may need to be readded each session
library(rio) # package needed
fragility23=rio::import(file = linkGit) 

# first operations, the next four operations allow you to visualize whether parts of the data are pulled in correctly and could be used for QC purposes

## Pull a list of column names from the object fragility23 into one list
names(x = fragility23)

## Check if R is correctly identifying certain columns as holding numeric data, if the code works numeric data should be identified with num
str(object = fragility23)

## Show only the first 10 rows, in the order they appear in the object
head(x = fragility23,10)

## show only the first 10 rows, in the order they appear in the object
tail(fragility23,10)

## Below code shows transformative manipulations for starting to analyze the data

## Limit your data to exclude those columns you wont be working with, which keeps the data more manageable in R: 
##Country, Total,
## S1: Demographic Pressures,
## P1: State Legitimacy,
## E2: Economic Inequality
## into object 'frag23_sub'

fragility23[,c(5,11,9)]

## A better option for limiting columns is to use column names instead of numbers which can be miscounted, example below
fragility23[,c('S1: Demographic Pressures','P1: State Legitimacy','E2: Economic Inequality')]

##grep pattern allows you to search the data for certain strings of text, you can even shorten the search of the string like instead of Total use Tot
grep(pattern = "Country|S1|P1|E2|Total",x = names(fragility23),fixed = F,value = T) # test

## indicates what you are keeping for strings for a data object for transformation and give this object a new name as a subset
keep=grep("Country|S1|P1|E2|Total",names(fragility23),fixed = F,value = T)
frag23_sub=fragility23[,keep]


## renaming the 'frag23_sub' columns, use S1,P1 E2 only because shorter variable names are easier to work with
names(frag23_sub)[3:5]=c("S1","E2", "P1")

##  Returns a list of the top ten best countries on the column 'E2' from 'frag23_sub'
tail(frag23_sub[order(x=-frag23_sub$E2),],10) #option1
tail(frag23_sub[order(x=-frag23_sub$E2),'Country'],10) #option2


## Calculates basic summary statistics for those countries including in "frag23_sub"
summary(object = frag23_sub)

## Returns the value of the worst quartile of the Total with the sub data object, after you run this also run q3_Total alone to get that result
q3_Total=quantile(x = frag23_sub$Total, 
                  probs = 0.75, 
                  na.rm = TRUE)

## Returns the correlations between "S1","E2", and "P1"
cor(x=frag23_sub[,-c(1,2)]) #no 'Country', no "Total'

## Returns correlations between "S1","E2", "P1" and their "significance", with 1.0 showing Pearsen's coefficient
#?install.packages("corrtable")
library(corrtable)
corrtable::correlation_matrix(df = frag23_sub[,-c(1,2)])

## Create a regression plot of P1 and E2 on S1 so we can better see how these variables are associated with one another
lm(S1~P1+E2,data=frag23_sub)
model <- lm(S1 ~ P1 + E2, data = frag23_sub)
summary(model)



# some plotting
## Produce a aplot for the 'P1' variable in a histogram format
hist(x = frag23_sub$P1) #base r



## The code below will produce the visual correlation between S1 and E2 in black and white
plot(x=frag23_sub$S1, y=frag23_sub$E2)

## Now apply color coding to the visual correlation to color points of countries in the worst quartile of Total
frag23_sub$Total>=q3_Total
frag23_sub$worstQt=frag23_sub$Total>=q3_Total

plot(frag23_sub$S1, 
     frag23_sub$E2,pch=20,
     col = as.factor(frag23_sub$worstQt))


# Check to ensure there are now red dots visible on the visual correlation as a quick quality check

#install.packages("sjPlot")
library(sjPlot)
plot_models(model)


