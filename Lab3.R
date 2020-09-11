#################################################### 
## Danielle Senechal
## CSC 315, Lab #3 
####################################################
library(readr)
library(ggplot2)
library(dplyr)
#####################Question 1#####################
happy <-  matrix(c(26, 233, 164, 117, 473, 293, 172, 383, 132), nrow=3, ncol=3, byrow = T)
colnames(happy) <- c("Not Too Happy", "Pretty Happy", "Very Happy")
rownames(happy) <- c("Above Average", "Average", "Below Average")
happy
#There does not appear to be a strong relationship between happiness and income. For the people
#with above average incomes, there is not a big different between very happy and not too happy, 
#they mostly answered pretty happy. For those with above average income, they were more likely 
#to say they were pretty happy or very happy. For those with average income, there is not much 
#of a relationship, just that they most likely said they were pretty happy as well. There is not
#a strong relationship in this data.

#####################Question 2#####################
cscsurvey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")
cscsurvey

#####################Question 3#####################
q3 <- table(cscsurvey$Fight, cscsurvey$YannyOrLaurel)
q3 <- prop.table(q3, margin = 1)
q3
q3df <- data.frame(type = rownames(q3), q3)
q3df
ggplot(q3df) + geom_col(aes(Var1, Freq, fill = Var2)) +
  labs(x = "Fight Preference?", y = "Proportion", fill = "Laurel or Yanny?", 
       title = "Distribution of Hearing Laurel and Yanny by Fight Preference") +
  theme_classic()
#There appears to be an association between the fight preference and hearing Laurel/Yanny for 
#this class. Of all who said they would rather fight 1 horse-sized duck, theres a 25% chance they
#heard Laurel, and a 75% chance they heard Yanny. Of all who said they woud rather fight 100 
#duck-sized horses, theres a 89% chance they heard Laurel, and a 11% chance they heard Yanny.
#Though there appears to be an association, that does not imply causation since this is an 
#observational study.

#####################Question 4#####################
t <- table(cscsurvey$Fight, cscsurvey$YannyOrLaurel)
tcol <- prop.table(t, margin = 2)
tcol
#(a) Of those who heard Yanny, 85.71% said they would prefer to fight one horse-sized duck.
trow <- prop.table(t, margin = 1)
trow
#(b) Of those who prefer to fight one horse-sized duck, 75% heard Yanny.

#####################Question 5#####################
high = cscsurvey$hsGPA
college = cscsurvey$collegeGPA
gpa_table <- data.frame(high, college)
fit <- lm(college ~ high, data = gpa_table)
predict(fit, data.frame(college = .5))

ggplot(cscsurvey, aes(hsGPA, collegeGPA)) + geom_point() + theme_classic() + 
  labs(x = "High School GPA", y = "College GPA",
       title = "High School GPA vs. College GPA") +
  geom_smooth(method = "lm", color = "indianred3")

#####################Question 6#####################
cor(cscsurvey$hsGPA, cscsurvey$collegeGPA)
#There is little association between High School GPA and College GPA because the correlation is 
#0.3387949, which means there is weak positive association.

#####################Question 7#####################
ggplot(cscsurvey, aes(hsGPA, collegeGPA, color = Gender)) + geom_point() +
  theme_classic() + labs(x = 'High School GPA', y = "College GPA",
       title = "High School GPA vs. College GPA") +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)
#a) The slopes of the gender lines are similar, they are almost parallel. This means there is
#a similar trend in the data.
#b) The line for males has a higher y-intercept then the girls. This means that males have on 
#average a higher college GPA than females. 

#####################Question 8#####################
fit = lm(mtcars$wt ~ mtcars$mpg)
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Car Weight (Thousands of pounds)", y = "Gas Mileage (MPG)",
       title = "MPG by Car Weight") +
  geom_smooth(method = "lm", color = "purple")

#####################Question 9#####################
fit = lm(mtcars$mpg ~ mtcars$wt)
fit
#The y-intercept is 37.285, which means that if the weight of the car is 0, the miles per gallon
#would be 37.285. The slope is -5.344, which means that for each increase of one thousand pounds,
#the fuel efficiency of the car decreases by 5.344 miles per gallon. 

#####################Question 10####################
predict(fit)
predict(fit, data.frame(mtcars$wt == 3)) #for some reason this command will not run correctly
-5.344*3+37.285
#A car that weighs 3000 pounds will get around 21.253 miles per gallon. We cannot predict the
#miles per gallon of a 7000 pound car because that value is unknown due to it being outside 
#of the interval. It would be extrapolation.
