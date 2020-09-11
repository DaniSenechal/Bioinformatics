####################################################################
# Name: Danielle Senechal
# Lab 7: Hypothesis testing for population means.

# For these questions, we will assume that the Central 
# Limit Theorem applies (i.e., that the populations are normally 
# distributed or that 'n' is sufficiently large), and that the 
# samples are representative of the population of interest. 

# Turn in a Notebook that answers the questions below
####################################################################

library(readr)
library(ggplot2)
library(dplyr)

# Read in our survey data
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")

##########################################################################
# 1. Assume that the mean amount of sleep an adult gets is 7.8 hours 
#    per night. Is there evidence that students in CSC 315 
#    get a different amount of sleep? (Here we assume that our 
#    survey results are representative of all CSC 315 students).

# (a) State the null and alternative hypotheses (done for you):

# H0: mu_sleep = 7.9
# HA: mu_sleep != 7.9

# where mu_sleep is the mean amount of sleep a CSC 315 student gets
# per night.

# (b) Calculate / find the test statistic (and specify the degrees of freedom)

res <- t.test(survey$Sleep, mu = 7.9)
t <- res$statistic
t

# (c) Find the p-value using the t.test function

res$p.value

# The p-value is 0.0007533573

# (d) Find the p-value 'manually' based on the test statistic and
#     appropriate degrees of freedom 

2*pt(-abs(t), df = res$parameter) # p-value

# The p-value is 0.0007533573, similar to 1c.

# (e) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

# Since the p-value of 0.0007533573 falls below 0.05, we reject the null 
# hypothesis in favor of the alternative. This means there is sufficient 
# evidence that students in CSC 315 get a different amount of sleep than 
# average adult.

##########################################################################

##########################################################################
# 2. Is there evidence that the college GPA of a 'cat' person 
# differs from that of a 'dog' person?

# (a) State the null and alternative hypotheses (done for you):

#   H0: mu_cat - mu_dog = 0
#   HA: mu_cat - mu_dog != 0,

#   where mu_cat and mu_dog are the mean college GPAs for students preferring
#   cats and dogs, respectively

# (b) Create side-by-side boxplots (using ggplot) showing College GPA for 'cat' and
#     'dog' people. Make sure to label the y-axis and give the 
#     chart a title.

ggplot(survey, aes(x=CatOrDogPerson, y = collegeGPA, fill = CatOrDogPerson)) + 
  geom_boxplot() + theme(legend.position = "none") +
  ggtitle("Cat vs Dog People and College GPA") +
  labs(x = "Cat or Dog", y = "College GPA")

# (c) We will now formally test the hypotheses that mean college GPA
#     is different between 'cat' and 'dog' people. The command 
#     t.test(x,y) will perform a two-sample t-test for the null 
#     hypothesis that the 'x' and 'y' populations have the same mean.
#     Use the t.test function to find the test statistic and the 
#     corresponding degrees of freedom. Note that in your call to t.test,
#     'x' is a vector of College GPAs for 'cat' people and 'y' is a 
#     vector of College GPAs for 'dog' people

s <- split(survey$collegeGPA, survey$CatOrDogPerson)

res <- t.test(s$Cat, s$Dog)
res 

# Cat people have an average college GPA of 3.615, and dog people
# have an average college GPA of 3.402727.

# (d) Find the p-value from the result of the t.test function

res$p.value

# The p-value is 0.1912803. 

# (e) Find the p-value 'manually' based on the test statistic and
#     appropriate degrees of freedom from the t.test result (which 
#     is stored in the $parameter object)

2*pt(-abs(res$statistic), df = res$parameter)

# (f) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

# Since the p-value of 0.1912803 is above 0.05, we fail to reject the null hypothesis. 
# There is not sufficient enough evidence that the college GPA of a 'cat' person 
# differs from that of a 'dog' person. 

# (g) What would it mean in the context of this problem if a Type I 
#     error occurred?

# A type one error would mean we conclude that there is not sufficient enough
# evidence that the college GPA of a 'cat' person differs from that of a 'dog' person,
# when in reality the college GPA of a 'cat' person differs from that of a 'dog' person.

##########################################################################

##########################################################################
# 3:  Find the p-values associated with the following t test 
# statistics, for a one-sample t-test, and state whether you would reject 
# or fail to reject the null hypothesis at α=0.05:

# (a) t = 2.78, n = 45

2*pt(-abs(2.78), df = 45 - 1)

# Since the p-value of 0.0079072 falls below 0.05, we reject the null hypothesis, in favor
# of the alternative.

# (b) t = -3.3, n = 51

2*pt(-abs(-3.3), df = 51 - 1)

# Since the p-value of 0.00176857 falls below 0.05, we reject the null hypothesis, in favor
# of the alternative.

# (c) t = 1.11, n = 100

2*pt(-abs(1.11), df = 100 - 1)
# Since the p-value of 0.2696611 does not fall below 0.05, we fail to reject the null hypothesis.

##########################################################################

# use the cereal data to complete the last question
cereal <- read.delim("http://pastebin.com/raw/0G6DrHyC")


#################################################################################
# 4:  The 'sugars' column contains the sugar content (in grams), while
#     the 'shelf' column contains the shelf in which the cereal is 
#     shelved on, with 1 = lower shelf, 2 = middle shelf (which is at
#     eye level for children), and 3 = top shelf. The code below constructs
#     a boxplot comparing sugar content across only the lower and middle 
#     shelves
#################################################################################

# remove data from the top shelf (Note: make sure dplyr is loaded
#     before running the next statement)
cereal <- filter(cereal, shelf != 3)

# change shelf column to factor
cereal$shelf <- factor(cereal$shelf)
levels(cereal$shelf) <- c("lower", "middle")

# generate boxplot
ggplot(cereal) + geom_boxplot(aes(shelf, sugars, fill = shelf)) +
  theme_classic() + theme(legend.position = "none") +
  ggtitle("Sugar content in cereals by shelf level") + 
  labs(x = "shelf level", y = "sugar content (grams)")

# Now let's formally test whether mean sugar content differs
# between the lower shelf and the middle shelf.

# (a) State the null and alternative hypotheses, making sure
#     to define the 'mu' parameters

#   H0: mu_middle - mu_lower = 0
#   HA: mu_middle - mu_lower != 0,

#   where mu_middle and mu_lower are the mean sugar conents in the cereals on the 
#   middle shelf and the lower shelf respectively 

# (b) Use the t.test function to find the test statistic 
#     and corresponding degrees of freedom

s <- split(cereal$sugars, cereal$shelf)
s

res <- t.test(s$lower, s$middle)
res$statistic
res$parameter

# (c) Find the p-value

res$p.value

# (d) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

# Since the p-value of 0.002149394 falls below 0.05, we reject the null hypothesis. 
# This means there is suffcient evidence that the mean sugar conents in the cereals 
# on the middle shelf differ from the cereals on the the lower shelf.

#################################################################################

