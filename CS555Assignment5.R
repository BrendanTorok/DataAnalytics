library(car)
library(emmeans)
dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Book4.csv')
dataset <- data.frame(dataset_raw)
# Question 1

physics <- dataset[dataset$group == 'Physics student', ]
summary(physics)
sd(physics$age)
sd(physics$iq)
math <- dataset[dataset$group == 'Math student', ]
summary(math)
sd(math$age)
sd(math$iq)
chemistry <- dataset[dataset$group == 'Chemistry student', ]
summary(chemistry)
sd(chemistry$age)
sd(chemistry$iq)

m_physics <- lm(physics$iq ~ physics$age)
m_math <- lm(math$iq ~ math$age)
m_chemistry <- lm(chemistry$iq ~ chemistry$age)

par(mfrow = c(2, 3))

plot(physics$age, physics$iq, 
     xlab="Age (years)",
     ylab="IQ",
     main="Phyics Group IQ vs Age")
abline(m_physics, lty=2, col="blue")

plot(math$age, math$iq, 
     xlab="Age (years)",
     ylab="IQ",
     main="Math Group IQ vs Age")
abline(m_math, lty=2, col="blue")

plot(chemistry$age, chemistry$iq, 
     xlab="Age (years)",
     ylab="IQ",
     main="Chemistry Group IQ vs Age")
abline(m_chemistry, lty=2, col="blue")

par(mfrow = c(1, 1))

boxplot(dataset$iq~dataset$group, data=dataset, 
        main="IQ by Group", 
        xlab="Group", 
        ylab="IQ", 
        ylim=c(20, 55))

#Question 2
qf(0.95, df1=2, df2=42)
is.factor(dataset$group)
dataset$group <- factor(dataset$group)
is.factor(dataset$group)
anova_result <- aov(iq ~ group, data=dataset)
summary(anova_result)
TukeyHSD(anova_result)

#Question 3 one-way ANOVA in regression framework
rm(list=ls())
.rs.restartR()
dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Book4.csv')
dataset <- data.frame(dataset_raw)

#Create dummy variables
dataset$physics_dummy <- ifelse(dataset$group=='Physics student', 1, 0)
dataset$math_dummy <- ifelse(dataset$group=='Math student', 1, 0)
model <- lm(iq ~ physics_dummy + math_dummy, data = dataset)
summary(model)


#Question 4

Anova(lm(iq ~ group + age, data = dataset), type=3)

model_adjusted <- lm(iq ~ group + age, data=dataset)
emm_options(contrasts=c("contr.treatment", "contr.poly"))
emmeans(model_adjusted, specs = "group")

# calculate f score given n categories (8) and n sample (250) with alpha (0.05)
qf(0.95, df1=7, df2=242)

# calculate number of additional tests to perform
(6*(6-1))/2

#qualitative interaction - effects are opposite rather than having different magnitudes
#quantitative interaction - effects are the same direction but magnitude may change
# 
# if there is an interaction, two-way ANOVA is not the most appropriate - stratify 
# then perform one way ANOVA on other factors 
