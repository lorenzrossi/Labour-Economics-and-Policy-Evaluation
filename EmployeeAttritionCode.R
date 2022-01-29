library(car)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)
library(ggplot2)
library(Hmisc)
library(olsrr)
library(tidyverse)
library(caret)
library(Metrics)
library(leaps)

data <- read.csv('EmployeeAttritionDef.csv')
str(data)

#data visualisation 



#correlation matrix

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res <- rcorr(as.matrix(data[4:28]))
flattenCorrMatrix(res$r, res$P)

# Extract the correlation coefficients
res$r
# Extract p-values
res$P

#corrplot

library(corrplot)
# Insignificant correlation are crossed

corrplot(res$r, method="color",
         type="upper", order="hclust",
         addCoef.col = "black", tl.cex = 0.45, 
         tl.col="black", tl.srt=90,
         p.mat = res$P, sig.level = 0.01,
         number.cex = .4, insig = "blank", diag=FALSE)

#scaling

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

iris_norm <- as.data.frame(lapply(data[8:28], min_max_norm))

#linear model

#LASSO

#create vars for yearscompany - yearspromotion, yearsrole - yearsprom, yearsmanager - yearspromotion
#and see corrplot

#Regression Analysis


#RCT

#overtime, regression with the other variables created

#logit

fullmodel <- lm(Attrition~., family = binomial, data = DFAttAll[2:20]) 

summary(fullmodel)

vif(fullmodel)
sqrt(vif(fullmodel)) > 10
sqrt(vif(fullmodel)) > 5


#RDD

#1. create categorical var: last promotion in 3Y for lower half income

#cat_variable <- as.factor(ifelse(existing_variable < 3, 'A',
#                                 ifelse(existing_variable < 4, 'B', 
#                                        ifelse(existing_variable < 5, 'C', 
#                                               ifelse(existing_variable < 6, 'D',0)))))


#logit 2
