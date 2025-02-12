dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Book3.csv')
dataset <- data.frame(dataset_raw)
names(dataset) <- c("occupation", "education.level", "income", "percent.women", "prestige")


#Question 1

plot(dataset$education.level, dataset$prestige, 
     xlab="Education Level (years)",
     ylab="Prestige Score",
     main="Relationship Between Education Level and Prestige Score of Occupation")

cor(dataset$education.level, dataset$prestige)

#Question 2

m <- lm(dataset$prestige ~ dataset$education.level)
m
residm <- resid(m)

#F test
qf(0.95, df1=1, df2=100)

qf(0.95, df1=3, df2=15)

qf(0.85, df1=4, df2=95)


#anova
anova(m)

#Residual plot
plot(dataset$education.level, residm, 
     xlab="Education Level (years)",
     ylab="Residual",
     main="Residual Plot for Regression of Education Level on Prestige Score of Occupation")
abline(0, 0, col="red")

#Variance

#Assumptions plots
dataset

plot(dataset$education.level, dataset$prestige, 
     xlab="Education Level (years)",
     ylab="Prestige Score",
     main="Relationship Between Education Level and Prestige Score of Occupation")
abline(m, lty=2, col="blue")


hist(residm, breaks = seq(-30, 20, by=2),
     main = "Histogram of Residual",
     xlab = "Residual")

#Outliers calculation

residm <- as.numeric(residm)
Q1 <- summary(residm)["1st Qu."]
Q3 <- summary(residm)["3rd Qu."]  
IQR_resid <- Q3 - Q1  
upper_limit <- Q3 + (1.5*IQR_resid)
lower_limit <- Q1 - (1.5*IQR_resid)

lower_outliers <- which(residm < lower_limit)
lower_outliers
upper_outliers <- which(residm > upper_limit)
upper_outliers

which(resid(m) == min(resid(m)))
min(residm)
which(resid(m) == max(resid(m)))
max(residm)

dataset

# Influence point

filtered_data <- dataset[-53, ]
fm <- lm(prestige ~ education.level, data = filtered_data)
summary(m)
summary(fm)

plot(filtered_data$education.level, filtered_data$prestige, 
     xlab="Education Level (years)",
     ylab="Prestige Score",
     main="Relationship Between Education Level and Prestige Score of Occupation")
abline(fm, lty=2, col="blue")


#Question 3 multiple linear regression
dataset

mlr <- lm(formula = prestige ~ education.level + income + percent.women, data=dataset)
summary(mlr)

qf(0.95, df1=3, df2=98)
qf(0.70, df1=4, df2=95)
qf(0.90, df1=6, df2=23)

anova(mlr)

#Question 4
#get t test statistic for a = 0.05
qt(1-0.025, 98)

#Confidence intervals
# simply plug the multiple linear regression and the conf level into confint()
confint(mlr, level=0.95)



#Question 5

residmlr <- resid(mlr)
fittedmlr <- fitted(mlr)

plot(fittedmlr, 
     residmlr,
     main = "Versus Fits 
     (Response is Prestige Score)",
     ylab = "Residual",
     xlab = "Fitted Value")
abline(0, 0, col="red")



#Outliers calculation mlr

residmlr <- as.numeric(residmlr)
Q1_mlr <- summary(residmlr)["1st Qu."]
Q3_mlr <- summary(residmlr)["3rd Qu."]  
IQR_residmlr <- Q3_mlr - Q1_mlr  
upper_limitmlr <- Q3_mlr + (1.5*IQR_residmlr)
lower_limitmlr <- Q1_mlr - (1.5*IQR_residmlr)

lower_outliersmlr <- which(residmlr < lower_limitmlr)
lower_outliersmlr
upper_outliersmlr <- which(residmlr > upper_limitmlr)
upper_outliersmlr

#Outliers calculation fitted mlr

fittedmlr <- as.numeric(fittedmlr)
Q1_fit <- summary(fittedmlr)["1st Qu."]
Q3_fit <- summary(fittedmlr)["3rd Qu."]  
IQR_fit <- Q3_fit - Q1_fit 
upper_limitfit <- Q3_fit + (1.5*IQR_fit)
lower_limitfit <- Q1_fit - (1.5*IQR_fit)

lower_outliersfit <- which(fittedmlr < lower_limitfit)
lower_outliersfit
upper_outliersfit <- which(fittedmlr > upper_limitfit)
upper_outliersfit

summary(fittedmlr)

# influence point

mlr <- lm(formula = prestige ~ education.level + income + percent.women, data=dataset)
summary(mlr)
data_no24 <- dataset[-24, ]
mlr_no24 <- lm(formula = prestige ~ education.level + income + percent.women, data=data_no24)
summary(mlr_no24)

