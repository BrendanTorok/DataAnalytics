library(pROC)
library(dplyr)
library(aod)
dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Book5.csv')
dataset <- data.frame(dataset_raw)
dataset
#Question 1
# create subset of both males and females that reach cutoff
dataset$temp_level <- ifelse(dataset$temp >= 98.6, 1, 0)
dataset
temp_subset <- dataset |>
  filter(temp_level==1)

notemp_subset <- dataset |>
  filter(temp_level==0)

# create subset of only males and females that reach cutoff
males_temp <- temp_subset |>
  filter(sex==1)
females_temp <- temp_subset |>
  filter(sex==2)

# create subset of only males and females that don't reach cutoff
males_notemp <- notemp_subset |>
  filter(sex==1)
females_notemp <- notemp_subset |>
  filter(sex==2)

# get the number of males and females 
num_males <- nrow(subset(dataset, dataset$sex == 1))
num_females <- nrow(subset(dataset, dataset$sex == 2))

# get the sample proportion of males and females
pp_males <- nrow(males)/num_males
pp_females <- nrow(females)/num_females

#Summarize data relating to temp level by sex
summary(males_temp$temp)
summary(males_temp$Heart.rate)
sd(males_temp$temp)
sd(males_temp$Heart.rate)
summary(females_temp$temp)
summary(females_temp$Heart.rate)
sd(females_temp$temp)
sd(females_temp$Heart.rate)

summary(males_notemp$temp)
summary(males_notemp$Heart.rate)
sd(males_notemp$temp)
sd(males_notemp$Heart.rate)
summary(females_notemp$temp)
summary(females_notemp$Heart.rate)
sd(females_notemp$temp)
sd(females_notemp$Heart.rate)
nrow(females_notemp)
nrow(males_notemp)

#Question 2

# get summary statistics for temp and heart rate for the temp
summary(males$temp)
summary(males$Heart.rate)
summary(females$temp)
summary(females$Heart.rate)

#Question 3
#Risk difference for high body temp lvl between men and women
pp_males - pp_females

#prop of temp=1 is same across men and women based on effect at a=0.05
num_success_m <- nrow(males)
num_success_f <- nrow(females)
num_failure_m <- num_males - nrow(males_temp)
num_failure_f <- num_females - nrow(females_temp)
result3 <- prop.test(c(num_success_m, num_success_f), c(num_males, num_females), 
                     alternative="two.sided", 
                     conf.level=0.95, 
                     correct=FALSE)
sqrt(result3$statistic)

prop.test(c(6, 16), c(49, 51), alternative="two.sided", correct=FALSE)
sqrt(5.3281)
#Question 4
#logistic regression with sex as explanatory variable
m <- glm(dataset$temp_level ~ dataset$sex, family=binomial)

#are odds of temp=1 the same between males and females
summary(m)
#odds ratio for sex
odds_ratio <- exp(coef(m))
odds_ratio



#associated 95% confidence interval
exp(confint(m))

#c-statistic for this model
probs4 <- predict(m, type=c("response"))
roc_curve4 <- roc(dataset$temp_level, probs4)
roc_curve4

#Question 5
#Multiple logistic regression predicting temp lvl from sex and heart rate
mlr <- glm(dataset$temp_level ~ dataset$sex + dataset$Heart.rate, family=binomial)
summary(mlr)
wald.test(b=coef(mlr), Sigma = vcov(mlr), Terms = 2:3)

#odds ratio for sex

exp(cbind(OR = coef(mlr), confint.default(mlr)))

#odds ratio for heart rate (10-beat increase)
exp(cbind(OR = coef(mlr)*10, confint.default(mlr)*10))
#c-statistic for model

dataset$prob <- predict(mlr, type=c("response"))
roc_curve5 <- roc(dataset$temp_level ~ dataset$prob)
roc_curve5
auc_value <- auc(roc_curve5)
plot(roc_curve5, main="ROC Curve for Multiple Logistic Regression Model")
text(x=0.2, 
     y = 0.3, 
     labels= paste("AUC = ", round(auc_value, 3)))

