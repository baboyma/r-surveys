# PROJECT: r-surveys
# AUTHOR: Baboyma Kagniniwa
# PURPOSE: Survey Data Analysis Workshop - sample code
# CREATED ON: 2023-03-12
# UPDATED ON: 2023-11-27

#install.packages("haven")
#install.packages("survey")
#install.packages("jtools")
#install.packages("remotes")
#remotes::install_github("carlganz/svrepmisc")

library(tidyverse)
library("haven")
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")

nhanes2012 <- read_dta("D:/data/Seminars/Applied Survey Data Analysis R/nhanes2012_reduced.dta")

nhanes2012 <- foreign::read.xport("Data/Raw/DEMO_G.xpt")
nhanes2012 <- nhanes2012 |> janitor::clean_names()

nhanes2012 |> glimpse()

# the "id" is actually the PSU!!!!!
nhc <- svydesign(id=~sdmvpsu, weights=~wtint2yr,strata=~sdmvstra, nest=TRUE, survey.lonely.psu = "adjust", data=nhanes2012)
nhc
summary(nhc)

# descriptive statistics with continuous variables
# calculating means
# the documentation for svymean is found under surveysummary
svymean(~ridageyr, nhc)

# calculating standard deviations
svysd(~ridageyr, design = nhc, na = TRUE)

svymean(~pad630, nhc, na = TRUE)

svymean(~hsq496, nhc, na = TRUE)

# listwise deletion of missing cases across all variables
# notice that the means shown below do not match those from above
svymean(~ridageyr+pad630+hsq496, nhc, na = TRUE)

# getting the mean, standard deviation, variance and coeff of var
svymean(~pad680, nhc, na = TRUE)
svysd(~pad680,design = nhc, na = TRUE)
svyvar(~pad680, design = nhc, na = TRUE)
cv(svymean(~pad680,design = nhc, na = TRUE))
svymean(~pad680, nhc, na = TRUE, deff = TRUE)

# quantiles
svyquantile(~hsq496, design = nhc, na = TRUE, c(.25,.5,.75),ci=TRUE)

# totals
svytotal(~dmdborn4,design = nhc, na = TRUE)
# can get the cv and deff for totals
cv(svytotal(~dmdborn4,design = nhc, na = TRUE))
svytotal(~dmdborn4,design = nhc, na = TRUE, deff = TRUE)

# means and proportions for binary variables
svymean(~female, nhc)
confint(svymean(~female, nhc))
# per comment on page 70, use svyciprop rather than confint()
# there are many options for method
# likelihood uses the (Rao-Scott) scaled chi-squared 
# distribution for the loglikeli-hood from a binomial distribution
svyciprop(~I(female==1), nhc, method="likelihood")
# li is short for likelihood
svyciprop(~I(female==0), nhc, method="li")
# fits a logistic regression model and computes 
# a Wald-type interval on thelog-odds scale, which 
# is then transformed to the probability scale
svyciprop(~I(female==1), nhc, method="logit")
# uses a logit transformation of the mean and 
# then back-transforms to the probablity scale. 
# This appears to be the method used by SUDAAN and 
# SPSS COMPLEX SAMPLES.
svyciprop(~I(female==1), nhc, method="xlogit")

## per the documentation: reproduces Stata svy: mean
svyciprop(~I(female==1), nhc, method="me", df=degf(nhc))
## per the documentation: reproduces Stata svy: prop
svyciprop(~I(female==1), nhc, method="lo", df=degf(nhc))

# extra examples
svymean(~female, nhc)
svyciprop(~I(female==0), nhc, method="mean")
svyciprop(~I(female==1), nhc, method="mean")

# descriptive stats for categorical variables
# frequencies
svytable(~female, design = nhc)

# crosstabulations
# 2-way
svytable(~female+dmdborn4, nhc)
# 2-way
svytable(~interaction(female, dmdborn4), design = nhc)
# 3-way
svytable(~interaction(female, dmdborn4, hsq571), design = nhc)
# 4-way
svytable(~interaction(female, dmdborn4, hsq571, paq665), design = nhc)

# chi-square test
svychisq(~female+dmdborn4, nhc, statistic="adjWald")

# graphing of continuous variables
# histogram 
# default is to show density on y-axis
svyhist(~pad630, nhc)

# histogram with count on y-axis
svyhist(~ridageyr, nhc, probability = FALSE)

# boxplot with just one variable and all outliers
svyboxplot(~hsq496~1, nhc, all.outliers=TRUE)

# boxplot with two variables
# the grouping variable must be a factor
svyboxplot(~hsq496~factor(female), nhc, all.outliers=TRUE)

# barchart
barplt<-svyby(~pad675+pad630, ~female, nhc, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

# dotplot
dotchart(barplt)

# scatterplot with weights as bubble size
svyplot(~pad675+pad630, nhc, style="bubble")

# examples of other types of graphs
smth<-svysmooth(~pad630, design=nhc)
plot(smth)
dens<-svysmooth(~pad630, design=nhc,bandwidth=30)
plot(dens)
dens1<-svysmooth(~pad630, design=nhc)
plot(dens1)

# subpops

# the mean of age
svymean(~ridageyr, nhc)
# the mean of age broken out by the variable female
svyby(~ridageyr, ~female, nhc, svymean)

# the mean of age broken bout the female and marital status
svyby(~ridageyr, ~dmdmartl+female, nhc, svymean)
# using three variables to define the subpopulation
# need the "na = TRUE"
svyby(~pad630, ~dmdmartl+dmdeduc2+female, nhc, na = TRUE, svymean)

# using subset.survey.design to see the results for only one group
# subsetting to include only males
smale <- subset(nhc,female == 0)
summary(smale)
svymean(~ridageyr,design=smale)

# t-test
svyttest(pad675~0, nhc, na = TRUE)
# paired samples t-test
# example from page 124
svyttest(I(pad660-pad675)~0, nhc, na = TRUE)
# two-sample t-tests
svyttest(pad630~female, nhc)
# calculating the difference by hand
# the matrix created in the svyby call is needed for the svycontrast function
svyttest(ridageyr~female, nhc)
summary(svyglm(ridageyr~female, design=nhc))
a <- svyby(~ridageyr, ~female, nhc, na.rm.by = TRUE, svymean, covmat = TRUE)
vcov(a)
a
# calculating the difference between two means
svycontrast(a, c( -1, 1))
# 4.589723 - 6.153479

# another example, this time using svypredmeans
summary(svyglm(pad630~female, design=nhc))

# can't have the variable female in the model when you want 
# to get the predicted means for that variable
ttest1 <- (svyglm(ridageyr~1, design=nhc))
summary(ttest1)
svypredmeans(ttest1, ~female)

# getting the confidence interval for the difference
tt<-svyttest(pad630~female, nhc)
tt
confint(tt, level=0.9)

# t-test with a subpopulation
svyttest(pad630~dmdeduc2, subset(nhc, female == 1 ) )

# multiple linear regression
# need to use summary to get standard errors, t and p-values
# main effects only
summary(svyglm(pad630~female+hsq571, design=nhc, na.action = na.omit))
# including an interaction term
summary(svyglm(pad630~female*hsq571, design=nhc, na.action = na.omit))
glm1 <- (svyglm(pad630~female*hsq571, design=nhc, na.action = na.omit))
glm1
confint(glm1)

summary(svyglm(pad630~factor(female)*factor(dmdmartl), design=nhc, na.action = na.omit))
ols1 <- (svyglm(pad630~1, design=nhc, na.action = na.omit))
predmarg<-svypredmeans(ols1, ~interaction(female,dmdmartl))
predmarg

# non-parametric tests
# Wilcoxon signed rank test
wil <- svyranktest(hsq496~female, design = nhc, na = TRUE, test = c("wilcoxon"))
wil

# median test
mtest <- svyranktest(hsq496~female, design = nhc, na = TRUE, test=("median"))
mtest

# Kruskal Wallis test
kwtest <- svyranktest(hsq496~female, design = nhc, na = TRUE, test=("KruskalWallis"))
kwtest


# logistic regression
logit1 <- (svyglm(paq665~factor(hsd010)+ridageyr, family=quasibinomial, design=nhc, na.action = na.omit))
summary(logit1)
# using a subpopulation
subset1 <- subset(nhc, ridageyr > 20)
logit2 <- (svyglm(paq665~factor(hsd010)+ridageyr, family=quasibinomial, design=subset1, na.action = na.omit))
summary(logit2)
regTermTest(logit2, ~ridageyr)
# getting pseudo-R-square values
psrsq(logit2, method = c("Cox-Snell"))
psrsq(logit2, method = c("Nagelkerke"))

# ordered logistic
ologit1 <- svyolr(factor(dmdeduc2)~factor(female)+factor(dmdborn4)+pad680, design = nhc, method = c("logistic"))
summary(ologit1)

# Poisson model
summary(svyglm(pad675~female, design=nhc, family=poisson()))

## other types of analyses available in the survey package
# weighted PCA
pc <- svyprcomp(~pad630+pad675+hsd010, design=nhc,scale=TRUE,scores=TRUE)
pc
biplot(pc, weight="scaled")

# Cronbach's alpha
svycralpha(~hsq571+dmdborn4, design=nhc, na.rm = TRUE)