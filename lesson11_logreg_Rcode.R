# ==================================
# Lesson 11 - Logistic Regression
# 
# Melinda Higgins, PhD
# dated 03/27/2017
# ==================================

# ==================================
# we're be working with the 
# helpmkh dataset
# ==================================

library(tidyverse)
library(haven)

helpdat <- haven::read_spss("helpmkh.sav")

# Note: info on data set is included in readme (full code book)

# ============================================.
# For this lesson we'll use the helpmkh dataset
#
# Let's focus on homeless as the main outcome variable
# which is dichotomous coded 0 and 1. We'll use
# logistic regression to look at predicting whether someone
# was homeless or not using these variables
# age, female, pss_fr, pcs, mcs, cesd and indtot
# ============================================.

# pcs and mcs look at physical and mental quality of life. CSD is depression, indtot - inventory of drug use consequence
h1 <- helpdat %>%
  select(homeless, age, female, pss_fr,
         pcs, mcs, cesd, indtot) 

# ============================================.
# Let's run a logistic regression of indtot to predict
# the probability of being homeless
# we'll also SAVE the predicted probabilities
# and the predicted group membership
#
# let's look at different thresholds pprob
# ctable gives us the classification table
#
# use the plots=roc to get the ROC curve
# ============================================;

m1 <- glm(homeless ~ indtot, data=h1,
          family=binomial)
# there's an option for glm called na.action
# in your report, talk about missing
# the model object m1 is both a lm and glm type at the same time

# look at the model results
m1

# summary of the model results
# Summary is a general function that does different things based on what kind of object is passed through.
summary(m1) 

# coefficients of the model - these are the
# RAW Betas 
coef(m1)

# take the exp to get the odds ratios
exp(coef(m1))

# look at the predicted probabilities
# review the help for predict.glm

# First argument is the model object. Third argument is basically outcome
m1.predict <- predict(m1, newdata=h1,
                      type="response")

# plot the continuous predictor
# for these predicted probabilities
plot(h1$indtot, m1.predict)

# Look at the tradeoffs for at threshol
# of 0.5
# confusion matrix
table(h1$homeless, m1.predict > 0.5)

# rearrange slightly
# compare to this online calculator
# http://statpages.info/ctab2x2.html
# She uses this page to calculate sensitivity and specificity

t1 <- table(m1.predict > 0.5, h1$homeless)
t1 # this shows homeless as the 0/1, and then model predictions are TRUE/FALSE. This shows false negatives and false positives.

tpr <- t1[2,2]/(t1[2,2]+t1[1,2]) # true positive rate
tpr #senstivity
tnr <- t1[1,1]/(t1[1,1]+t1[2,1]) # true negative rate
tnr #specificity

# we can use these data to compile 
#     TRUE Positives
#     FALSE Positives
#     TRUE Negatives
#     FALSE Negatives

# see what happens if we try other
# threshold values
t1 <- table(m1.predict > 0.6, h1$homeless)

tpr <- t1[2,2]/(t1[2,2]+t1[1,2])
tpr #senstivity
tnr <- t1[1,1]/(t1[1,1]+t1[2,1])
tnr #specificity

t1 <- table(m1.predict > 0.4, h1$homeless)

tpr <- t1[2,2]/(t1[2,2]+t1[1,2])
tpr #senstivity
tnr <- t1[1,1]/(t1[1,1]+t1[2,1])
tnr #specificity

t1 <- table(m1.predict > 0.2, h1$homeless)

tpr <- t1[2,2]/(t1[2,2]+t1[1,2])
tpr #senstivity
tnr <- t1[1,1]/(t1[1,1]+t1[2,1])
tnr #specificity

# another way to look at cross tables
library(gmodels)
CrossTable(h1$homeless, m1.predict > 0.5)

# another way to look at tradeoffs
# of picking different thresholds
# make an ROC curve

library(ROCR)
p <- predict(m1, newdata=h1, 
             type="response")
pr <- prediction(p, as.numeric(h1$homeless))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b=1, col="red")
# For reading this model, a linear straight line means no better than 50/50. 


# the area under this curve compared
# to the y=x reference line
# tells you how well the model is predicting
# and AUC of 0.5 is a bad model - no better
# than flipping a coin
# AUC of 0.6-0.7 is still not very good
# AUC of 0.7-0.8 is pretty good
# AUC of 0.8-0.9 is good
# AUC 0.9-1.0 is great 

# For reading this model, a linear straight line means no better than 50/50. 

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# also try running with Rcmdr
library(Rcmdr)

# see https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


