#Part 1
#A)Compute and plot the probability distribution for the number of perfect scores over the 5 attempts (both the PMF and CDF)
#PMF plot
n <- 5
p <- 0.4
pmf <- dbinom(0:n, size = n, prob = p)
plot(0:n, pmf, type = "h", xaxt = "n",
     main = "PMF for Number of Perfect Scores Over 5 Attempts", 
     xlab = "Number of Attempts", ylab = "PMF")
points(0:n, pmf, pch = 10)   
axis(side = 1, at = 0:n, labels = TRUE)

#CDF plot
cdf <- cumsum(pmf)
cdf <- c(0, cdf)
cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "CDF for Number of Perfect Scores Over 5 Attempts", 
     xlab = "Number of Attempts", ylab = "CDF")


#B)What is the probability that a student will score a perfect score in exactly 2 out of the 5 attempts?
dbinom(2, size = n, prob = p)


#C)What is the probability that a student will score a perfect score in at least 2 out of the 5 attempts?
sum(dbinom(2:n, size = n, prob = p))


#D)Simulate the number of perfect scores over 5 attempts for 1000 students. Show the barplot of the frequencies of successes.
dist <- rbinom(1000, size = n, prob = p)
dist.table <- table(dist)
plot(dist.table, type="h", col="blue", ylab = "Frequency", 
     xlab = "Perfect Score Attempts", 
     main = "Simulated Perfect Scores Over 5 Attempts for 1000 Students")

#Part 2
#A)Compute and plot the probability distribution for scoring the three perfect scores (both the PMF and CDF). The student will only go for a maximum of 10 failures before giving up.
#PMF plot
n <- 10
p <- 0.7
pmf <- dnbinom(3:n, size = n, prob = p)
plot(3:n, pmf, type = "h", xaxt = "n",
     main = "PMF for Number of Failures Before 3 Perfect Scores", 
     xlab = "Number of Failures", ylab = "PMF")
points(3:n, pmf, pch = 18)   
axis(side = 1, at = 3:n, labels = TRUE)

#CDF plot
cdf <- cumsum(pmf)
cdf <- c(0, cdf)
cdfplot <- stepfun(3:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "CDF for Number of Failures Before 3 Perfect Scores", 
     xlab = "Number of Failures", ylab = "CDF")

#B)What is the probability that the student will have the three perfect scores with exactly 5 failures?
r <- 3 #number of successes
f <- 5 #number of failures
p = 0.7
dnbinom(f, size = r, prob = p)

#C)What is the probability that the student will have the three perfect scores with at most 5 failures?
sum(dnbinom(0:f, size = r, prob = p))

#D) Simulate the number of failures to get the three perfect scores for 1000 students. Show the barplot of the frequencies of the failures.
perf.scores <- rnbinom(1000, size = 3, prob = p)
perf.scores <- table(perf.scores)
barplot(perf.scores, ylim = c(0,120), xlab = "Number of Failures", 
        ylab = "Frequency", 
        main = "Number of Failures to Get 3 Perfect Scores For 1000 Students",
        col = "Red")

#Part 3
#A)Compute and plot the probability distribution (both the PMF and CDF) for the number of multiple choice questions out of the 25 questions that the student will be given?
K <- 25 #sample size
m.multiplechoice <- 40 #interested
n.written <- 60 #not interested

pmf <- dhyper(0:K, m = m.multiplechoice, n = n.written, k = K)
plot(0:K, pmf, type = "h", xaxt = "n",
     main = "PMF for Number of Multiple Choice Questions in Exam", 
     xlab = "Number of Multiple Choice Questions", ylab = "PMF")
points(0:K, pmf, pch = 18)   
axis(side = 1, at = 0:K, labels = TRUE)

cdf <- phyper(0:K, m = m.multiplechoice, n = n.written, k = K)
cdf <- c(0, cdf)
cdfplot <- stepfun(0:K, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "CDF for Number of Multiple Choice Questions in Exam", 
     xlab = "Number of Multiple Choice Questions", ylab = "CDF")

#B)What is the probability that the student will have exactly 15 multiple choice questions out of the 25 questions in the exam?

dhyper(15, m = m.multiplechoice, n = n.written, k = K)

#C)What is the probability that the student will have at least 15 multiple choice questions out of the 25 questions in the exam?

sum(dhyper(15:K, m = m.multiplechoice, n = n.written, k = K))

#D)Simulate the number of multiple choice questions for 1000 students. Show the barplot of the frequencies of the multiple-choice questions.
par(mai = c(1, 1, 1, 1))
par(mgp=c(3,1,0))
simulation <- rhyper(1000, m = m.multiplechoice, n = n.written, k = K)
simulation.table <- table(simulation)
barplot(simulation.table, ylim = c(0,200), las=2, ylab = "Frequency", 
        xlab = "Number of Multiple Choice Questions", 
        main = "Number of Multiple Choice Questions for 1000 Students", 
        col="dark green")

#Part 4
#A)What is the probability that the professor will have to answer exactly 10 questions per day?
dpois(10, lambda=15)

#B)What is the probability that the professor will have to answer at most 10 questions per day?
ppois(10, lambda=15)

#C)What is the probability that the professor will have to answer between 7 and 12 questions (inclusive)?
sum(dpois(7:12, lambda=15))

#D)Calculate and plot the PMF for the first 20 questions.
pmf <- dpois(0:20, lambda=15)
plot(0:20,pmf,type="h", main = "PMF for the First 20 Questions",
     xlab="Question Number",ylab="PMF", ylim = c(0, 0.12))
abline(h=0, col="red")

#E) Suppose the course runs for 50 days. Simulate the number of questions the professor gets per day over the course run. Show a barplot of the frequencies of the number of questions. Show a boxplot of the number of questions. What do you infer from the plots?
pois.simulation <- rpois(50, lambda = 15)
plot(table(pois.simulation), ylab = "Frequency", 
     xlab = "Number of Questions Received", 
     main = "50 Day Simulation for Number of Questions Received Each Day")
boxplot(pois.simulation, main = "50 Day Simulation for Number of Questions Received Each Day")

#Looking at the boxplot, we can see that the average is 15 questions as expected, 
#but there seem to be more outliers on the upper end than the lower end. 
#This is possibly because there is no upper limit for the number of questions 
#that can be recieved while there is a lower limit of 0 for the number of 
#questions received. Although 15 is the average number of questions, 
#18 was the most frequently recieved number of questions. It is evident that the 
#scores are more highly distributed above 15 than below 15 by looking at the 
#barplot which has a value of 11 questions above average, whereas the lowest 
#value is only 6 questions less than the average.

#Part 5
#A)Plot the PDF of this distribution covering the three standard deviations on either side of the mean
mu = 100
sigma = 15
x <- seq(50,150)
pdf = dnorm(x, mean = mu, sd = sigma)
plot(x, pdf, type="l", col="dark green", xaxt="n", ylim = c(0, 0.030), 
     xlab = "Number of Dollars Spent", ylab = "PDF", 
     main = "PDF of Number of Dollars Spent At An Amusement Part")
axis(side = 1, at = c(mu-3*sigma,mu-2*sigma,mu-sigma,mu,mu+sigma,mu+2*sigma,mu+3*sigma), 
     labels = TRUE)

#B)What are the chances that a randomly selected visitor will spend more than $125?

probability.above <- 1 - pnorm(125, mu, sigma)
probability.above * 100
#4.8% chance

#C)What is the chance that a randomly selected visitor will spend between $85 and $95 (inclusive)?
probability.between <- pnorm(95, mu, sigma) - pnorm(85, mu, sigma)
probability.between * 100
#21% chance

#D)What are the chances of spending within one standard deviation, two standard deviations, and three standard deviations, respectively?
probabiity.one.stdev <- pnorm(mu + sigma, mu, sigma) - pnorm(mu - sigma, mu, sigma)
probabiity.one.stdev * 100
#68% chance

probabiity.two.stdev <- pnorm(mu + 2*sigma, mu, sigma) - pnorm(mu - 2*sigma, mu, sigma)
probabiity.two.stdev * 100
#95% chance

probabiity.three.stdev <- pnorm(mu + 3*sigma, mu, sigma) - pnorm(mu - 3*sigma, mu, sigma)
probabiity.three.stdev * 100
#99% chance

#E)Between what two values will the middle 90% of the money spent will fall?
(top_value <- qnorm(0.95, mu, sigma))
(low_value <- qnorm(0.05, mu, sigma))

#F)If the theme park gives a free T-shirt for the top 1% of the spenders, what will be the minimum amount you have to spend to get the free T-shirt?
qnorm(0.99, mu, sigma)

#G)Show a plot for 10,000 visitors using the above distribution. This is continuous distribution.

y <- rnorm(10000, mu, sigma)
hist(y, xlab ="Amount Spent", 
     main = "Simulatiom of Amount Spent for 10000 Visitors", ylim=c(0,3000), 
     col="blue")





