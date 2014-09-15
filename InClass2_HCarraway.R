#Hunter Carraway
#CMDA 3654
#In class assignment 2
getwd()
setwd("/Users/hunter/Desktop/CMDA 3456/R code")
load('exampleData.rData')

summary(custdata)
#income has 328 'NA'
#housing.type, recent.move, and num.vehicles show 56 obs that were inputted incorrectly
uciCar <- read.table('http://www.win-vector.com/dfiles/car.data.csv',sep=',',header=T)
summary(uciCar)
#The ratings of 1210 of the cars are unacceptable for manufacturing standards

load('credit.RData')
mapping <- list('A11'='... < 0 DM',
                'A12'='0 <= ... < 200 DM',
                'A13'='... >= 200 DM / salary assignments for at least 1 year',
                'A14'='no checking account',
                'A30'='no credits taken/all credits paid back duly',
                'A31'='all credits at this bank paid back duly',
                'A32'='existing credits paid back duly till now',
                'A33'='delay in paying off in the past',
                'A34'='critical account/other credits existing (not at this bank)',
                'A40'='car (new)',
                'A41'='car (used)',
                'A42'='furniture/equipment',
                'A43'='radio/television',
                'A44'='domestic appliances',
                'A45'='repairs',
                'A46'='education',
                'A47'='(vacation - does not exist?)',
                'A48'='retraining',
                'A49'='business',
                'A410'='others',
                'A61'='... < 100 DM',
                'A62'='100 <= ... < 500 DM',
                'A63'='500 <= ... < 1000 DM',
                'A64'='.. >= 1000 DM',
                'A65'='unknown/ no savings account',
                'A71'='unemployed',
                'A72'='... < 1 year',
                'A73'='1 <= ... < 4 years',
                'A74'='4 <= ... < 7 years',
                'A75'='.. >= 7 years',
                'A91'='male : divorced/separated',
                'A92'='female : divorced/separated/married',
                'A93'='male : single',
                'A94'='male : married/widowed',
                'A95'='female : single',
                'A101'='none',
                'A102'='co-applicant',
                'A103'='guarantor',
                'A121'='real estate',
                'A122'='if not A121 : building society savings agreement/life insurance',
                'A123'='if not A121/A122 : car or other, not in attribute 6',
                'A124'='unknown / no property',
                'A141'='bank',
                'A142'='stores',
                'A143'='none',
                'A151'='rent',
                'A152'='own',
                'A153'='for free',
                'A171'='unemployed/ unskilled - non-resident',
                'A172'='unskilled - resident',
                'A173'='skilled employee / official',
                'A174'='management/ self-employed/highly qualified employee/ officer',
                'A191'='none',
                'A192'='yes, registered under the customers name',
                'A201'='yes',
                'A202'='no')

#A for loop used to map the characters as factors into numeric values
for (i in 1: (dim(d))[2])
{
  if (class (d[,i]) == 'character')
  {
    d[,i] <- as.factor(as.character(mapping[d[,i]]))
  }
}

summary(d$Personal.status.and.sex)
summary(d)


#loads the ggplot library
library(ggplot2)

#creates a subset of custdata called custdata2 which uses data with ages 0-100 and income greater than 0
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100
                     & custdata$income > 0))
custdata2

#plots a scatterplot with a smoothing curve
ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() + ylim(0, 200000)

install.packages("hexbin")
#loads the hexbin library
library(hexbin)

#plots a hexbin plot that shows the density of the groups of points much better than a scatterplot would
#the smoothing curve is very similar to that of the scatterplot
ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white") +
  ylim(0,200000)



ggplot(custdata2) +
  geom_bar(aes(x=num.vehicles, y=income, fill=num.vehicles),
  stat="identity") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6))
#The graph shows that a larger income does not mean that person has more vehicles
#in fact usually the lower income brackets had the larger amount of vehicles


#the graph shows that an equal amount of people with incomes greater and less than 30k/year move
#which means that money isn't necessarily a reason families move locations
ggplot(custdata2) +
  geom_bar(aes(x=recent.move, fill=income.lt.30K))+
  theme_bw()+
  ggtitle("How many people have an income of less than 30k and moved recently")
