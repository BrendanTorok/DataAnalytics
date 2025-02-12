library(asbio)
library(ggplot2)

#Import participant and non-participant data into separate variables
part <- c(230.16, 210.99, 288.73, 590.28, 582.59, 635.21, 249.86, 441.66,
          572.43, 357.78, 396.79, 298.38, 282.99, 368.51, 388.59, 256.32,
          408.82, 424.94, 477.96, 428.74, 432.52, 428.27, 596.79, 456.30,
          446.38)

non_part <- c(614.61, 503.46, 425.22, 688.77, 184.00, 299.73, 350.65, 394.94, 
              261.55, 295.28, 139.69, 462.78, 179.59, 301.75, 436.58, 371.39, 
              469.02, 378.09, 287.31, 448.55, 332.64, 403.98)
calorie_data <- data.frame(
  intake = c(part, non_part),
  group = c(rep("participant", length(part)), 
            rep("non-participant", length(non_part)))
)
calorie_data

#Get the summaries for both samples
summary_part <- summary(part)
part_count <- length(part)
summary_part
sd_part <- sd(part)
summary_non_part <- summary(non_part)
non_part_count <- length(non_part)
summary_non_part
sd_non_part <- sd(non_part)

#graph participant vs non-participant in the same graph

ggplot(calorie_data, aes(x = group, y = intake, fill = group)) +
  geom_boxplot() +
  labs(title = "Calorie Intake by Group", x = "Group", y = "Calorie Intake") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green")) +
  theme(legend.position = "none")


#Question 2 determine if mean calorie consumption is significantly different from 425

alpha = 0.05
sig_value = 425
qt(0.975, 24)

part_t <- (mean(part) - 20) / (sd_part/sqrt(part_count))
part_t

part_p <- 1 - pt(part_t, df=length(part)-1)

t.test(part, mu=425, alternative="two.sided", conf.level=0.95)


#Question 4 calculate 90% confidence interval

t.test(part, mu=425, alternative="two.sided", conf.level=0.90)

#Question 4 
qt(0.95, 21)
qt(0.95, 42.9)

t.test(part, non_part, alternative="greater", conf.level=0.95)

#Question 5 calculate outliers
IQR_part = summary_part[5] - summary_part[2]
IQR_non_part = summary_non_part[5] - summary_non_part[2]

upper_outliers_part <- summary_part[5] + (1.5*IQR_part)
upper_outliers_part
lower_outliers_part <- summary_part[2] - (1.5*IQR_part)
lower_outliers_part


upper_outliers_non_part <- summary_non_part[5] + (1.5*IQR_non_part)
upper_outliers_non_part 
lower_outliers_non_part <- summary_non_part[2] - (1.5*IQR_non_part)
lower_outliers_non_part

pnorm(1.35, lower.tail = FALSE)

#calculate confidince interval
sd = 1.12
n = 34
mean = 6.1
a=0.05
z = 1.960

interval <- 1.96 * (1.12/sqrt(34))
6.1 + interval
6.1 - interval

#calculate z score
z <- (91.66-90)/(4/sqrt(67))
2 * pnorm(z, lower.tail = FALSE)

#calculate SEM
sem <- 5/sqrt(10)

z = (104-100)/sem

2*pnorm(z, lower.tail = FALSE)

qt(0.86, 23)

(102.22 - 96)/(9/sqrt(29))

pnorm(2.045, lower.tail=FALSE)

sqrt((15^2/9) + (10^2/11))

1.645 * 5.838

38-12

#Calculate two tailed p score test
#Get z score
#pnorm(z, lower.tail = FALSE)
#P value = 2 * pnorm result


#Calculate z score
#SEM = sd / sqrt(n)
#Z = sample mean - population mean / SEM

#Get z score
#If using both tails, then use the alpha value given
#If using one tail, then half alpha by 2 before checking Z score

#If p value is less than alpha, that means you reject null hypothesis
#If p value is greater than alpha, you fail to reject null hypothesis

#If the t statistic is greater than the critical value that means you reject 
#null hypothesis, it also means p value is less than alpha

#Can plug the z score into pnorm to get the probability. Set lower.tail=FALSE
#to get the probability above the z score

#Getting confidence interval for two sample means
#Margin of error = z * sample error
#(Mean1 - mean 2) +- margin of error

#Degrees of freedom
#Use n-1
#If there are two samples, use n-1 of smallest sample size

#qt() can be used to get critical value 
#Ex, critical value for df = 21 and confidence interval is 95%
#qt(0.95, 21)

#Assumptions made when doing two sample test:
#  Samples are independent and randomly selected
#Variable of interest is measured the same way in each population
#Parameter of interest is normally distributed (or at least similar shapes without outliers)

