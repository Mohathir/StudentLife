#necessary packages
library(pedometrics)
library(car)
library(reshape2)
library(ggplot2)
library(plotly)

#downloading data
sleep.raw <- read.csv("SleepStudyData.csv")
#removing missing cases
sleep <- sleep.raw[complete.cases(sleep.raw),]
#recoding "yes" and "no" factors as numeric 1's and 0's
sleep$Enough <- ifelse(sleep$Enough == "Yes",1,0)
sleep$PhoneReach <- ifelse(sleep$PhoneReach == "Yes",1,0)
sleep$PhoneTime <- ifelse(sleep$PhoneTime == "Yes",1,0)
sleep$Breakfast <- ifelse(sleep$Breakfast == "Yes",1,0)

#checking to ensure correct data types
sapply(sleep,class)
#looks good!

#EDA

#creating correlation matrix
cor.matrix <- cor(sleep)

#creating heat map
cor.melt <- melt(cor.matrix)

cor.heat <- ggplot(data = cor.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
cor.heat

#seems like hours of sleep correlates positively with student perceptions of getting enough sleep - makes sense
#Students feel tired when they don't get enough sleep, negative correlation - makes sense
#hours of sleep and perceived tiredness have negative correlation
#breakfast positively correlates with hours and negatively correlates with perceived tiredness - useful explanatory variable

#lets get a better handle on how phone habits impact hours of sleep and perceived tiredness using boxplots
#lets start with if students sleep with their phone in reach
box.reach <- plot_ly(x = sleep$Tired, y = sleep$Hours, fillcolor = sleep$PhoneReach, type = "box")
box.reach <- box.reach %>% layout(xaxis = list(title = 'Perceived Tiredness'),yaxis = list(title ='Hours of Sleep' ),boxmode = "group")
box.reach
#Seems like overall those who do not sleep with their phone in reach get more hours of sleep

#how about if students use their phone 30 mins before bed
box.phone <- plot_ly(x = sleep$Tired, y = sleep$Hours, fillcolor = sleep$PhoneTime, type = "box")
box.phone <- box.phone %>% layout(xaxis = list(title = 'Perceived Tiredness'),yaxis = list(title ='Hours of Sleep' ),boxmode = "group")
box.phone
#seems like some students who don't use their phone before bed still have difficulty sleeping


#testing to see if students who use phone before bed get less sleep compared to students that don't
#seperating students who use phone before bed versus those that don't
student.phoneindex <- which(sleep$PhoneTime == 1)
student.phone <- sleep[student.phoneindex,]
student.nophone <- sleep[-student.phoneindex,]

#testing hypothesis
t.test(student.nophone$Hours, student.phone$Hours, alternative = "greater")
#Accept null hypothesis, means are the same, no significant difference in hours of sleep for phone versus no phone


#what about if students sleep with their phone in reach, do they get less sleep? 
#maybe students are using their phone just not right before bed

#seperating students who sleep with phone in reach versus those that don't
student.reachindex <- which(sleep$PhoneReach == 1)
student.reach <- sleep[student.reachindex,]
student.noreach <- sleep[-student.reachindex,]

#testing hypothesis
t.test(student.noreach$Hours,student.reach$Hours, alternative = "greater")
#Accept null hypothesis, means are the same, no significant difference hours of sleep for phone in reach versus not

#I'm skeptical that this is true, students might be lying about their phone habits...

#lets do one more test using the breakfast variable we identified before as correlating with hours of sleep and perceived tiredness
student.breakfastindex <- which(sleep$Breakfast== 1)
student.breakfast <- sleep[student.breakfastindex,]
student.nobreakfast <- sleep[-student.breakfastindex,]

#do students feel more tired if they do not eat breakfast?
t.test(student.nobreakfast$Tired,student.breakfast$Tired, alternative = "greater")
#reject null hypothesis, students who do not eat breakfast typically feel more tired than students who eat breakfast

#Do students who eat breakfast get more hours of sleep versus those that don't?
t.test(student.breakfast$Hours,student.nobreakfast$Hours, alternative = "greater")
#reject null hypothesis, students who eat breakfast typically get more hours of sleep than students who don't


#Data analysis

#lets see if we can predict hours of sleep using linear regression
sleep.pred <- lm(Hours ~., data = sleep)

#examining model
summary(sleep.pred)
#students' perceptions of getting enough sleep, using their phone before bed, and eating breakfast are postive,significant predictors of hours of sleep
#PhoneTime has a postive estimate!!??, worth noting that this dataset only measures hours of sleep and not sleep quality i.e. light vs REM vs SWS (deep) sleep


#examining assumptions of regression model
par(mfrow = c(2, 2))
plot(sleep.pred)

#seems like assumptions hold, might have issues with normality

#conducting vstep analysis
v <- stepVIF(sleep.pred, threshold = 5, verbose = T)
#no multi-collinearity

#checking for influential outliers
cooks.D <- as.vector(cooks.distance(v))

influential <- which(cooks.D > 1)

influential

#no influential outliers to remove

# Build the model on training data -
set.seed(80)  #setting seed to reproduce results of random sampling
index <- sample(1:nrow(sleep), 0.7*nrow(sleep))  #row indices for training data, 70/30 split
trainingData <- sleep[index, ]  # model training data
testData  <- sleep[-index, ] #testing data


#creating model
sleep.train <- lm(Hours ~., data = trainingData)

#checking if model is similar to original to ensure there is not a weird sample of training data
summary(sleep.train)
summary(sleep.pred)
#looks good

#using model to predict amount of sleep student gets based on lifestyle factors,
fitted.results <- predict(sleep.train,testData,type='response')

Error <- mean(round(fitted.results) != testData$Hours)
print(paste('Accuracy',(1-Error)*100,'%'))
RMSE.rm = sqrt(sum((testData$Hours- fitted.results)^2) / nrow(testData))
print(RMSE.rm)

#Our model can accurately predict the hours of sleep for a student 39% of the time with an RMSE of 1.35 hours.


#lets see the confusion matrix on prediction

confusion <- as.data.frame(table(round(fitted.results),testData$Hours))
colnames(confusion) <- c("Predicted","Actual", "Freq")

confusion.matrix <- ggplot(data = confusion,
                           mapping = aes(x = Predicted,
                                         y = Actual)) +
  geom_text(aes(label = Freq), vjust = 1, fontface = "bold")
confusion.matrix

#model often under-predicts the amount of sleep for a given student


#Testing it on Declan's sleep data
data.declan <- rbind(c(1,8,1,1,1,1),sleep)
data.declan <- data.declan[1,]

results.declan <- predict(sleep.pred,data.declan,type = 'response')

print(results.declan)
#7.65 hours is pretty close to the actual hours of sleep for Declan: 8 hours. 

#during last week of school though, Declan's sleep habits worsen. Let's see if the model predicts that he get less sleep.
data.declan <- rbind(c(0,7,1,1,4,1),data.declan)
results.declan <- predict(sleep.pred,data.declan[1,],type = 'response')
results.declan
#model accurately predicts that during the last week of school, I get on average one and a half less hours of sleep than usual





