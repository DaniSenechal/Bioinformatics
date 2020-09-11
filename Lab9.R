##############################################################
# Name: Danielle Senechal
# CSC-315
# Lab #9: Limma, heatmaps, and analyzing processed GEO data
#############################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps below and to explicitly answer the following questions
##########################################################################

library(limma)
library(GEOquery)
library(ggplot2)
library(dplyr)

##################################################################################
# 1.The code below reads in our class survey data and performs a 
#   2-sample t-test to evaluate whether there is a statistically
#   significant difference in Hours of Sleep between 'Cat' vs. 'Dog'
#   people. Based on the code below, (a) find the p-value and state 
#   your conclusion regarding the null hypothesis of H0: mu_cat - mu_dog = 0;
#   and (b) calculate the difference in mean Alcohol consumption between
#   groups, using the formula: 
#     mean hours of sleep for dog people - mean hours of sleep for cat people
##################################################################################
survey <- read.csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")
s <- split(survey$Sleep, survey$CatOrDogPerson)
res <- t.test(s$Cat, s$Dog, var.equal = TRUE)

# a) 
  
  res$p.value
  
  # The p-value is 0.7970757. Since the p-value does not fall below 0.05, we
  # fail to reject the null hypothesis. This means that there not a statistically
  # significant difference in Hours of Sleep between 'Cat' vs. 'Dog'
  # people.
  
# b)

  diff(res$estimate)
  mean(s$Dog) - mean(s$Cat)
  
  # The difference of the means is 0.2.
  
##################################################################################
# 2.Fit a linear model that predicts Hours of Sleep based on 
#   whether an individual is a cat or a dog person. You should use
#   the treatment contrast where 'cat person' is the reference (x = 0) and 
#   'dog person' is the treatment (x = +1)
  
  animal <- as.integer(survey$CatOrDogPerson == "Dog")
  df <- mutate(survey, animal)
  # View(select(df, CatOrDogPerson, animal))

  ggplot(df, aes(animal, Sleep)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    theme_classic() + 
    xlab("Animal (0 = Cat, 1 = Dog)") +
    ggtitle("Relationship between Sleep and Cat vs Dog People")
   
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.
  
  fit <- lm(Sleep ~ animal, data = survey)
  fit
  
  # The y-intercept is 6.3 hours of sleep. This means that the mean amount of sleep 
  # that cat lovers get is 6.3.

# (b) Find and interpret the slope of the regression line in the context of 
#     this problem
  
  # As seen in the summary generated above, the slope of the regression line is 0.2, 
  # which shows the difference in the means of the hours of sleep for cat vs dog people. 
  # This slope shows that the mean average of sleep for cat lovers is 0.2 less than the
  # mean average of sleep for dog lovers.

# (c) What is the p-value for the hypothesis test that there is a
#     significant difference in Hours of Sleep between the two groups?
#     (show this result in R, based on the linear model) Note: the p-value 
#     from the linear model should match the p-value from the two-sample 
#     t-test from problem 1(a) above.
  
  summary(fit)
  
  # As seen in the summary generate above, the p-value is 0.7971, which does match
  # the p-value generated in 1a. There is not a significant difference in hours of 
  # sleep between the two groups. 
  
##################################################################################


###############################################################
# 3. Get the processed data for GSE19143 and pull out the 
#    expression data and phenotype data. Note that this
#    dataset contains gene expression samples from children
#    with Acute Lymphoblastic Leukemia (ALL), a cancer of
#    the bone marrow. Tumor samples were treated with
#    the anti-inflammatory drug prednisolone, and determined 
#    to be either sensitive (responsive) or resistant 
#    (non-responsive) to this drug. 
###############################################################
  
  GSE19143 <- getGEO("GSE19143")

  GSE19143.expr <- exprs(GSE19143[[1]]) 
  GSE19143.p <- pData(GSE19143[[1]])
  
# (a) How many samples had their gene expression values profiled?
  
  # There are 52 samples
  
# (b) How many probes are on the array?

  # There are 22283 probes.
  
# (c) Take the log2 of the expression data, and generate a boxplot
#     to show that the samples are properly processed and normalized.
#     The analysis beginning with question 5 must use the log2 data; 
#     otherwise the results will not be correct.

  GSE19143.expr <- log2(GSE19143.expr)
  boxplot(GSE19143.expr, main = "log2 processed data")

#####################################################################
# 4.How many individuals are resistant to prednisolone and
# how many are sensitive? 
#####################################################################
  
  indv <- as.character(GSE19143.p$characteristics_ch1.5)
  table(indv)
  
  # 27 individuals are sensitive and 25 are resistant.

#####################################################################
# 5. Find the top differentially expressed probes, with a FDR of 10%,
# between individuals that are resistant vs. sensitive to prednisolone.
# Note: there should be 16 probes total. How many of these probes 
# are up-regulated (i.e., have higher expression) in resistant 
# individuals and how many are down-regulated (i.e., have lower 
# expression) in resistant individuals. 
#####################################################################

  designs <- model.matrix(~0+indv)
  head(designs)
  colnames(designs) <- c("Resistant", "Sensitive")
  head(designs)
  fit <- lmFit(GSE19143.expr, designs)
  contrast.matrix <- makeContrasts(Sensitive - Resistant, levels=designs)
  contrast.matrix
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit2 <- eBayes(fit2)
  tt <- topTable(fit2, sort.by = "p")
  tt
  tt.1 <- topTable(fit2, sort.by = "p", p.value = 0.1, number = nrow(GSE19143.expr))
  nrow(tt.1)
  
  probe <- rownames(tt)[1]
  m <- match(probe, rownames(GSE19143.expr))
  df <- data.frame(expr = GSE19143.expr[m,], indv = indv)
  means <- df %>% group_by(indv) %>% summarize(mean = mean(expr))
  means
  diff(means$mean) #-2.442641
  
  # Since the difference in means is less than zero, the expression is 
  # higher in those who are prednisolone resistant. 
  
########################################################################
# 6. Construct a heatmap of these top 16 probes, with individuals 
# color-coded by response to prednisolone (with green=sensitive and 
# red = resistant). (Note: if you are unable to complete question 5), 
# you may do this with the first 16 probes in the expression matrix).
########################################################################

  m <- match(rownames(tt.1), rownames(GSE19143.expr))
  X <- GSE19143.expr[m,]
  col.heat <- colorRampPalette(c("yellow", "blue"))(200)
  col.indv <- as.integer(as.factor(indv))
  col.indv <- c("green", "red")[col.indv]

  heatmap(X, ColSideColors = col.indv, col = col.heat)
  
########################################################################
# 7. If you answered question 5 correctly, the SECOND hit 
# should be for the probe 209374_s_at. Show that this probe
# corresponds to the gene IGHM, by first downloading the 
# correct platform data from GEO, and then finding the gene
# associated with this probe. 
#######################################################################

  platform <- annotation(GSE19143[[1]])   
  platform
  pl <- getGEO(platform)
  pl <- Table(pl)
  probe <- rownames(tt.1)[2] 
  m <- match(probe, pl$ID)
  pl$`Gene Symbol`[m]

#####################################################################
# 8. How many probes are there for the gene IGHM on the platform
# in this study? Note: you must search for this gene using the
# regular expressions covered in the GEO-and-limma.R script. Your 
# code must also output the number of probes. 
####################################################################

  m <- match(rownames(tt.1), pl$ID)
  pl$`Gene Symbol`[m]
  length(pl$`Gene Symbol`[m])

########################################################################
# Final Notes: the heatmap in question 6 provides a candidate list
# of probes associated with prednisolone response in children with 
# leukemia. Although much additional work and testing would need to be 
# done, this kind of gene signature could ultimately be used to 
# determine whether a child with leukemia would benefit from 
# prednisolone treatment, or whether an alternative treatment might be 
# more effective.

# The IGHM finding is also interesting. IGHM is a gene that codes
# for an antibody protein involved in the immune reponse; the 
# fact that this gene is differentially expressed beween responders and 
# non-respnoders suggests that a patient's immune system may play a
# role in how they respond to prednisolone)
########################################################################