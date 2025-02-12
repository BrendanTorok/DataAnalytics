library(foreign)
library(dplyr)
library(aod)
library(pROC)
library(GGally)
#dataset obtained from https://www.openml.org/search?type=data&sort=runs&id=1461&status=active

dataset_raw <- read.arff('/Users/brendantorok/Documents/BU MET/CS555/phpkIxskf.arff')
dataset <- data.frame(dataset_raw)
names(dataset) <- c("age", 
                    "occupation", 
                    "marital.status", 
                    "education",
                    "default",
                    "balance",
                    "housing.loan",
                    "personal.loan",
                    "contact.type",
                    "contact.day",
                    "contact.month",
                    "contact.duration",
                    "campaign",
                    "pdays",
                    "previous",
                    "poutcome",
                    "y")
dataset_cleaned <- dataset |>
  filter(!contact.duration==0)
nrow(dataset_cleaned)
#get a simple sample from dataset of size 1000
set.seed(12345)
simple_sample <- dataset_cleaned %>% sample_n(1000)
#simplify the success data to have success = 1 and anything else = 0

simple_sample$success <- ifelse(simple_sample$poutcome == "success" | 
                                  (simple_sample$poutcome == "other" & 
                                     simple_sample$y==2), 1, 0)

length(which(simple_sample$success == "1"))

nrow(simple_sample)

#get preliminary boxplot for balance age and contact duration
par(mfrow = c(2, 3))

boxplot(simple_sample$balance~simple_sample$success,
        main = "Bank Balance by Success Group",
        xlab = "Success (0 = Failure, 1 = Success)",
        ylab = "Balance")

boxplot(simple_sample$age~simple_sample$success,
        main = "Age by Success Group",
        xlab = "Success (0 = Failure, 1 = Success)",
        ylab = "Age")

boxplot(simple_sample$contact.duration~simple_sample$success,
        main = "Contact Duration by Success Group",
        xlab = "Success (0 = Failure, 1 = Success)",
        ylab = "Contact Duration")

par(mfrow = c(1, 1))
# remove balance and contact duration outliers from dataset

Q1 <- quantile(simple_sample$balance, 0.25)
Q3 <- quantile(simple_sample$balance, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
sample_dataset_balance <- simple_sample |>
  filter(balance >= lower_bound & balance <= upper_bound)

Q1_cd <- quantile(simple_sample$contact.duration, 0.25)
Q3_cd <- quantile(simple_sample$contact.duration, 0.75)
IQR_cd <- Q3_cd - Q1_cd
lower_bound_cd <- Q1_cd - 1.5 * IQR_cd
upper_bound_cd <- Q3_cd + 1.5 * IQR_cd
sample_dataset_cleaned <- sample_dataset_balance |>
  filter(contact.duration >= lower_bound_cd & contact.duration <= upper_bound_cd)

nrow(sample_dataset_cleaned)


# create a scatterplot matrix for balance, age, and contact.duration

ggpairs(sample_dataset_cleaned,
        columns = c("contact.duration", "age", "balance"),
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("smooth", method = "lm", color="red", 
                                       se = TRUE)))

# create subsets based on failure and success
success_subset <- sample_dataset_cleaned |>
  filter(success==1)
failure_subset <- sample_dataset_cleaned |>
  filter(success==0)

#first describe the datasets relating to success and failure for age, balance, 
#and contact duration
summary(success_subset$age)
sd(success_subset$age)
summary(success_subset$balance)
sd(success_subset$balance)
summary(success_subset$contact.duration)
sd(success_subset$contact.duration)
summary(failure_subset$age)
sd(failure_subset$age)
summary(failure_subset$balance)
sd(failure_subset$balance)
summary(failure_subset$contact.duration)
sd(failure_subset$contact.duration)

nrow(success_subset)
nrow(failure_subset)

par(mfrow = c(2, 3))

boxplot(sample_dataset_cleaned$age ~ sample_dataset_cleaned$success,
        main = "Age by Success Group",
        xlab = "Success (1 = Success, 0 = Failure)",
        ylab = "Age")

boxplot(sample_dataset_cleaned$balance ~ sample_dataset_cleaned$success,
        main = "Bank Balance by Success Group",
        xlab = "Success (1 = Success, 0 = Failure)",
        ylab = "Balance")

boxplot(sample_dataset_cleaned$contact.duration~sample_dataset_cleaned$success,
        main = "Contact Duration by Success Group",
        xlab = "Success (1 = Success, 0 = Failure)",
        ylab = "Contact Duration")


#plot balance, age, contact duration with best fit lines for success group
qt(0.975, 24)
cor.test(success_subset$balance, success_subset$age, 
         alternative="two.sided", 
         conf.level=0.95)
cor.test(success_subset$balance, success_subset$contact.duration, 
         alternative="two.sided", 
         conf.level=0.95)
cor.test(success_subset$age, success_subset$contact.duration, 
         alternative="two.sided", 
         conf.level=0.95)

plot(success_subset$age, success_subset$balance,
     xlab = "Age", 
     ylab = "Balance",
     main = "Balance vs. Age (Success Group)")
abline(lm(balance ~ age, data = success_subset), col = "blue")

plot(success_subset$contact.duration, success_subset$balance,
     xlab = "Contact Duration", 
     ylab = "Balance",
     main = "Balance vs. Contact Duration (Success Group)")
abline(lm(balance ~ contact.duration, data = success_subset), col = "blue")

plot(success_subset$age, success_subset$contact.duration,
     xlab = "Age", ylab = "Contact Duration",
     main = "Contact Duration vs. Age (Success Group)")
abline(lm(contact.duration ~ age, data = success_subset), col = "blue")


#plot balance, age, contact duration with best fit lines for failure group
qt(0.975, 813)

cor.test(failure_subset$balance, failure_subset$age,
         alternative="two.sided", 
         conf.level=0.95)
cor.test(failure_subset$balance, failure_subset$contact.duration,
         alternative="two.sided", 
         conf.level=0.95)
cor.test(failure_subset$age, failure_subset$contact.duration,
         alternative="two.sided", 
         conf.level=0.95)

plot(failure_subset$age, failure_subset$balance,
     xlab = "Age", 
     ylab = "Balance",
     main = "Balance vs. Age (Failure Group)")
abline(lm(balance ~ age, data = failure_subset), col = "blue")

plot(failure_subset$contact.duration, failure_subset$balance,
     xlab = "Contact Duration", 
     ylab = "Balance",
     main = "Balance vs. Contact Duration (Failure Group)")
abline(lm(balance ~ contact.duration, data = failure_subset), col = "blue")

plot(failure_subset$age, failure_subset$contact.duration,
     xlab = "Age", ylab = "Contact Duration",
     main = "Contact Duration vs. Age (Failure Group)")
abline(lm(contact.duration ~ age, data = failure_subset), col = "blue")


# Perform t.tests for all predictors

DF = nrow(success_subset) - 1

qt(0.975, DF)

t_test_balance <- t.test(balance ~ success,
                         alternative="two.sided", 
                         conf.level=0.95, 
                         data=sample_dataset_cleaned)
t_test_balance

t_test_contact_duration <- t.test(contact.duration ~ success,
                                  alternative="two.sided", 
                                  conf.level=0.95, 
                                  data=sample_dataset_cleaned)
t_test_contact_duration

t_test_age <- t.test(age ~ success, 
                     alternative="two.sided", 
                     conf.level=0.95,
                     data=sample_dataset_cleaned)
t_test_age


# multiple logistic regression predicting success from age, balance, and 
# contact duration

mlr <- glm(success ~ age + balance + contact.duration, 
           data=sample_dataset_cleaned, 
           family=binomial) 
summary(mlr)
wald.test(b=coef(mlr), Sigma = vcov(mlr), Terms = 2:4)

# odds ratio for success
exp(cbind(OR = coef(mlr), confint.default(mlr)))
exp(cbind(OR = coef(mlr)*100, confint.default(mlr)*100))

#roc curve and c-statistic
sample_dataset_cleaned$prob <- predict(mlr, type=c("response")) 
roc_curve <- roc(success ~ prob, data=sample_dataset_cleaned) 
roc_curve
auc_value <- auc(roc_curve)
par(mfrow = c(1, 1))
plot(roc_curve5, main="ROC Curve for Multiple Logistic Regression Model") 
text(x=0.2,
     y=0.3,
     labels=paste("AUC = ", round(auc_value, 3)))

# Test if there is a different variable that can be used - in this case previous

t.previous <- t.test(previous ~ success, 
                          alternative = "two.sided", 
                          conf.level = 0.95, 
                          data = sample_dataset_cleaned)
t.previous

# add previous to mlr in place of contact duration
mlr.previous <- glm(success ~ age + balance + previous, 
                    data = sample_dataset_cleaned, 
                    family = binomial)
summary(mlr.previous)
exp(cbind(OR = coef(mlr.previous), confint.default(mlr.previous)))
exp(cbind(OR = coef(mlr.previous)*100, confint.default(mlr.previous)*100))
roc_curve <- roc(sample_dataset_cleaned$success, predict(mlr.previous, 
                                                         type = "response"))
wald.test(b=coef(mlr), Sigma = vcov(mlr.previous), Terms = 2:4)
auc.previous <- auc(roc_curve)

plot(roc_curve, main="ROC Curve for Multiple Logistic Regression Model") 
text(x=0.2,
     y=0.3,
     labels=paste("AUC = ", round(auc.previous, 3)))

# save dataset to .csv

write.csv(sample_dataset_cleaned,
          "/Users/brendantorok/Documents/BU MET/CS555/CS555finalproject.csv", 
          row.names=FALSE)