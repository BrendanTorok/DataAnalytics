library(RColorBrewer)
dataset_raw <- read.csv(
  '/Users/brendantorok/Documents/BU MET/CS555/Assignment1dataset.csv', 
  header = FALSE)
dataset <- as.numeric(dataset_raw)
hist(dataset, 
     breaks = max(dataset), 
     main = 'Histogram of Hospital Stay Length', 
     xlab = 'Hospital Stay Length (Days)',
     ylab = 'Frequency of Stay Length',
     ylim = c(0, 24),
     xlim = c(2, 15),
     xaxt = "n", #suppress x axis default values
     yaxt = "n", #suppress y axis default values
     col = brewer.pal(9, "BuGn"))

#Add custom labels at intervals for x-axis and y-axis
axis(1, at = seq(0, 30, by = 1), labels = seq(0, 30, by = 1))
axis(2, at = seq(0, 24, by = 2), labels = seq(0, 24, by = 2))

#Get summary statistics
five <- fivenum(dataset)
summary(dataset)
sd(dataset)
range <- IQR(dataset)

upper_outliers <- (five[4] + (1.5 * range))
lower_outliers <- (five[2] - (1.5 * range))

#Calculate IQR
five[4] - five[2]

#Calculate pnorm
pnorm(2.3002, 3.1412, 0.8461)

qnorm(0.97, 0, 1)

#Calculations Q4b
u <- 3/sqrt(35)
u

(6-5)/u

u <-329/sqrt(80)
u
z_score <- (893-1158)/u
pnorm(z_score)