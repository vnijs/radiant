## @knitr load.libs
library(ggplot2)
library(tidyr)
library(dplyr)

## @knitr titanic.data
load('data/titanic2.rda')

## @knitr plot.survival.counts
pclass.surv <- titanic %>%
  ggplot(aes(x=pclass,fill=survived)) + geom_bar(position='dodge') 

gender.surv <- titanic %>%
  ggplot(aes(x=sex,fill=survived)) + geom_bar(position='dodge') 

age.surv <- titanic %>%
  filter(!age=='NA') %>%
  mutate(age.f=cut(age,breaks=c(0,20,30,40,50,60,100))) %>%
  ggplot(aes(x=age.f,fill=survived)) + geom_bar(position='dodge') 


## @knitr titanic.logit.A

## define age groups 
titanic.est <- titanic %>%
  filter(!age=='NA') %>%
  mutate(age.f=cut(age,breaks=c(0,20,30,40,50,60,100)))
  
## define logit model
logit.titanic.A <- glm(survived=='Yes'~pclass+sex+age.f,
                       data=titanic.est,
                       family=binomial(link="logit"))


## @knitr pred.logit.A
## create prediction array 
pred.df <- expand.grid(pclass=levels(titanic.est$pclass), 
                       sex=levels(titanic.est$sex),
                       age.f=levels(titanic.est$age.f))

## predictions
pred.df$Prob <- predict(logit.titanic.A, newdata = pred.df, type = "response")

## @knitr plot.predictions.logit.A
ggplot(data=pred.df,aes(x=age.f,y=Prob,group=sex,color=sex)) + geom_line(linetype='dotted') + 
  geom_point() + ylab('Survival Probability') + xlab('Age Group') + facet_wrap(~pclass)  



## @knitr plot.beta.estimates

sum.c <- summary(logit.titanic.A)$coefficients

estimates.logit.A <- data.frame(
  Parameter=rownames(sum.c),                                
  Estimate=sum.c[,'Estimate'],
  confint.default(logit.titanic.A))

ggplot(data=filter(estimates.logit.A,
                   !Parameter=='(Intercept)'),  
  aes(x=Parameter,y=Estimate, 
      ymin=X2.5..,ymax=X97.5..)) + 
  geom_pointrange() + 
  coord_flip() + 
  xlab(' ') + 
  geom_hline(xintercept=0)

## @knitr logit.titanic.B
logit.titanic.B <- glm(survived=='Yes'~pclass*sex+age.f,
                       data=titanic.est,
                       family=binomial(link="logit"))


## @knitr pred.logit.B

## predictions
pred.df$Prob <- predict(logit.titanic.B, newdata = pred.df, type = "response")

ggplot(data=pred.df,aes(x=age.f,y=Prob,group=sex,color=sex)) + geom_line(linetype='dotted') + 
  geom_point() + ylab('Survival Probability') + xlab('Age Group') + facet_wrap(~pclass)  


## @knitr load.seatbelt.data
load('data/brfss11_sub.rda')
names(brfss11)











