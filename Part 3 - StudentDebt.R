
###########################################

library('tidyverse')
library('httr')
library("ggplot2")
library('gganimate')

##########################################

# https://open.canada.ca/data/en/dataset/d954f6ad-82fc-4e61-98e3-27c3b25b56a0

debt_Student <- read.csv("37100037.csv")
debt_Student <- debt_Student %>% 
  filter(Statistics != "Percentage of graduates with debt who had paid it off at time of interview") %>% 
  filter(Statistics != "Percentage of graduates who owed debt to the source at graduation")
  


debt_Student_Clean <- debt_Student[, c(1,2,4,6,7,13)] # pull the important columns only
colnames(debt_Student_Clean) <- c("Year", "Location", "Education",
                             "Information", "Type", "Value")   #rename the columns

 
	
## Question 1
## Are students incurring more student debt in 2015 vs 2000?


view(debt_Student_Clean)

Debt_DollarAmount <- debt_Student_Clean %>% 
                filter(Type == "Dollars") %>% 
                filter(Location != "United States")  # filter out the United States data


Debt2000 <- Debt_DollarAmount %>% 
    filter(Year == 2000)
Debt2005 <- Debt_DollarAmount %>% 
  filter(Year == 2005)
Debt2010 <- Debt_DollarAmount %>% 
  filter(Year == 2010)
Debt2015 <- Debt_DollarAmount %>% 
  filter(Year == 2015)

Years <- c(2000,2005,2010,2015)
CanadaDebt <- data.frame(Years = c(2000,2005,2010,2015), AvgDebt = as.numeric(0))

CanadaDebt[1,2] <- summarise(Debt2000, Average = mean(Value,na.rm = T))
CanadaDebt[2,2] <- summarise(Debt2005, Average = mean(Value,na.rm = T))
CanadaDebt[3,2]<- summarise(Debt2010, Average = mean(Value,na.rm = T))
CanadaDebt[4,2]<- summarise(Debt2015, Average = mean(Value,na.rm = T))


## How much debt does a student graduating from college, or university have on average after graduating? This data collects the dollar amount 
## if the total is less than $25000 

## we can see that the average amount of student debt is increasing and approaching the $25000 upper limit in 2015. 

Debt_Percent <- debt_Student_Clean %>% 
  filter(Type == "Percent") %>% 
  filter(Location != "United States")

DebtPer2000 <- Debt_Percent %>% 
  filter(Year == 2000)
DebtPer2005 <- Debt_Percent %>% 
  filter(Year == 2005)
DebtPer2010 <- Debt_Percent %>% 
  filter(Year == 2010)
DebtPer2015 <- Debt_Percent %>% 
  filter(Year == 2015)

CanadaDebt$Percent <- as.numeric(0)

CanadaDebt[1,3] <- summarise(DebtPer2000, Average = mean(Value,na.rm = T))
CanadaDebt[2,3] <- summarise(DebtPer2005, Average = mean(Value,na.rm = T))
CanadaDebt[3,3]<- summarise(DebtPer2010, Average = mean(Value,na.rm = T))
CanadaDebt[4,3]<- summarise(DebtPer2015, Average = mean(Value,na.rm = T))


## graph of Average Debt per Year
ggplot(CanadaDebt, aes(Years, AvgDebt)) +
  stat_smooth(method = lm, se = FALSE) + geom_point() +
  ggtitle("Average Student Debt at Graduation") +
  xlab ("Year") + ylab("Average Debt") +
  theme(plot.title = element_text(hjust = 0.5))

## the following code will create the animated version of the above graph. The code will create 100 .png files, which will then need to 
## be converted into a gif using any free gif creater found with a google search. have attached the completed gf to submission

ggplot(CanadaDebt, aes(Years, AvgDebt)) +
  geom_line() +
  ggtitle("Average Student Debt at Graduation") +
  xlab ("Year") + ylab("Average Debt") +
  theme(plot.title = element_text(hjust = 0.5)) + transition_reveal(Years)


## graph of Average Percentage of Students with over $25000 of debt  per Year
ggplot(CanadaDebt, aes(Years, Percent)) +
  stat_smooth(method = lm, se = FALSE) + geom_point() +
  xlab ("Year") + ylab("Percentage") +
  ggtitle("Percent of Students with more than $25000 Debt") +
  theme(plot.title = element_text(hjust = 0.5))

################# END of 1st Question #########################
###############################################################


## Create a regresson model for debt in Canada, split by the level of education

College <- Debt_DollarAmount %>% 
        filter(Education == "College")

Bachelor <- Debt_DollarAmount %>% 
  filter(Education =="Bachelor's")
 

Master <- Debt_DollarAmount %>% 
  filter(Education == "Master's")

Doctorate <- Debt_DollarAmount %>% 
  filter(Education == "Doctorate")


RegressionModelAllYears <- data.frame(College = College$Value, Bachelor = Bachelor$Value, Master = Master$Value, Doctorate = Doctorate$Value)

options(scipen = 10)

fitAllYears <- lm(Bachelor ~ ., data = RegressionModelAllYears, na.action = na.exclude) # We have Doctorate is not significant + has multiple NAs
summary((fitAllYears))


qplot(fitAllYears$residuals)
## pretty normal distribution of residuals



#################################################
## What if we wanted to just look at  Alberta? ##
#################################################



AlbertaCollege <- Debt_DollarAmount %>% 
  filter(Education == "College") %>% 
  filter (Location =="Alberta")

AlbertaBachelor <- Debt_DollarAmount %>% 
  filter(Education =="Bachelor's") %>% 
  filter (Location =="Alberta")


AlbertaMaster <- Debt_DollarAmount %>% 
  filter(Education == "Master's") %>% 
  filter (Location =="Alberta")

AlbertaDoctorate <- Debt_DollarAmount %>% 
  filter(Education == "Doctorate") %>% 
  filter (Location =="Alberta")


RegressionModelAlberta <- data.frame(College = AlbertaCollege$Value, Bachelor = AlbertaBachelor$Value, 
                                     Master = AlbertaMaster$Value, Doctorate = AlbertaDoctorate$Value)

fitAlberta <- lm(Bachelor ~ ., data = RegressionModelAlberta)
summary(fitAlberta)


## if we knew the avg student debt college, masters and doctorate graduates have, we can predict the average debt a bachelor graduate will have at graduation?

# 1b ...can we accurately predict the likelihood a student will leave university with student debt? 

avg.datapoint <- data.frame(College = mean(College$Value), Master = mean(na.omit(Master$Value)), Doctorate = mean(na.omit(Doctorate$Value)))

predict(fitAllYears,avg.datapoint)
predict(fitAlberta,avg.datapoint)

