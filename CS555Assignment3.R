dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Assignment3dataset.csv')

dataset_raw

fish <- data.frame(dataset_raw)
names(fish)[names(fish) == "Number.of.meals.with.fish"] <- "num_meals"
names(fish)[names(fish) == "Total.Mercury.in.mg.g"] <- "mercury_content"
fish

#Question 1
plot(fish$num_meals, fish$mercury_content, 
     xlab="Weekly Fish Meals",
     ylab="Mercury Level in Hair (mg/g)",
     main="Relationship Between Fish Consumption and Mercury Levels in Hair 
     Among Fishermen",
     xlim = c(min(fish$num_meals), max(fish$num_meals)),
     ylim = c(min(fish$mercury_content), max(fish$mercury_content)))

#Question 2

std.num_meals <- (fish$num_meals - mean(fish$num_meals)) / sd(fish$num_meals)
std.mercury_content <- (fish$mercury_content - mean(fish$mercury_content)) / sd(fish$mercury_content)

n <- length(fish$num_meals)
r <- 1 / (n - 1) * sum(std.num_meals*std.mercury_content)
r

#Calculate using cor() function
cor(fish$num_meals, fish$mercury_content)

#Question 3

m <- lm(mercury_content ~ num_meals, data=fish)
m
summary(m)

plot(fish$num_meals, fish$mercury_content, 
     xlab="Weekly Fish Meals",
     ylab="Mercury Level in Hair (mg/g)",
     main="Relationship Between Fish Consumption and Mercury Levels in Hair 
     Among Fishermen",
     xlim = c(min(fish$num_meals), max(fish$num_meals)),
     ylim = c(min(fish$mercury_content), max(fish$mercury_content)))

abline(m, lty=2, col="red")

#Question 4
#F value
qf(0.95, df1=1, df2=98)

#anova table
m_anova <- anova(m)
m_anova

#standard error table
summary_m <- summary(m)
summary_m

#F test
m_anova["num_meals", "Mean Sq"]/m_anova["Residuals", "Mean Sq"]

#90% confidence interval

slope <- coef(summary_m)["num_meals", "Estimate"]
error_slope <- coef(summary_m)["num_meals", "Std. Error"]

#0.95 used for two-tailed 90% confidence interval
t_critical <- qt(0.95, df=98)

lower_bound <- slope - t_critical * error_slope
upper_bound <- slope + t_critical * error_slope
confidence_interval <- c(lower_bound, upper_bound)
confidence_interval

x <- c(8, 13, 20, 30)
y <- c(24, 2, 9, 22)
cor(x, y)

