library(prob)
Prob <- prob::prob

#Part 2
#A)
S <- rolldie(3, makespace=TRUE)
sample_range <- subset(S, (X1 + X2 + X3 > 3) & (X1 + X2 + X3 < 8))
head(sample_range)
tail(sample_range)
Prob(sample_range)

#B)
(sample_same <- subset(S, (X1 == X2) & (X2 == X3)))
Prob(sample_same)

#C)

sample_two_same <- subset(S, (X1 == X2 & X1 != X3) | (X1 == X3 & X1 != X2) | (X2 == X3 & X1 != X3))
head(sample_two_same)
tail(sample_two_same)
Prob(sample_two_same)

#D)

sample_none_same <- subset(S, (X1 != X2) & (X1 != X3) & (X3 != X2))
head(sample_none_same)
tail(sample_none_same)
Prob(sample_none_same)

#E)

sample_two_same_range <- intersect(sample_two_same, sample_range)
head(sample_two_same_range)
tail(sample_two_same_range)
Prob(sample_two_same_range)

#Part 3
sum_of_first_N_even_squares <- function(x){
  sum_of_N <- 0
  n <- 0
  i<-0
  while (i <= x){
    curr_square = n*n
    sum_of_N = sum_of_N + curr_square
    n <- n+2
    i <- i+1
    }
  return(sum_of_N)
}

sum_of_first_N_even_squares(2)
sum_of_first_N_even_squares(5)
sum_of_first_N_even_squares(10)

#Part 4
#A)
tsla <- read.csv("https://people.bu.edu/kalathur/datasets/TSLA2022.csv")

sm <- summary(tsla$Close)
names(sm) <- c("Min", "Q1", "Q2", "Mean", "Q3", "Max")
sm

#B)
minvalue <- min(tsla$Close)
minrow <- which.min(tsla$Close)
mindate <- tsla[which.min(tsla$Close), "Date"]
paste("The minimum Tesla value of", minvalue, "is at row", minrow, "on", mindate, sep=" ")

#C)
maxvalue <- max(tsla$Close)
maxrow <- which.max(tsla$Close)
maxdate <- tsla[which.max(tsla$Close), "Date"]
paste("The maximum Tesla value of", maxvalue, "is at row", maxrow, "on", maxdate, sep=" ")

#D)
p_closing_greater <- sum(tsla$Close > tsla$Open)/nrow(tsla)
paste("The probability that the tesla closing price is greater than the opening price is ", p_closing_greater)

#E)
p_trading_volume <- sum(tsla$Volume > 100000000)/nrow(tsla)
paste("The probability that the tesla trading volume on any given day is greater than 100 million shares is ", p_trading_volume)

#F)
hv <- subset(tsla, Volume > 100000000)
p_hv_closing_greater <- sum(hv$Close > hv$Open)/nrow(hv)
paste("The probability that the tesla closing price is higher than the opening price on a day with trading volume greater than 100 millions shares is ", p_hv_closing_greater)

#G)
num_days <- nrow(tsla)
total_cost <- sum(tsla$Low)
last_close_price <- tsla[num_days, 5]
sale_price <- last_close_price * num_days
total_profit = sale_price - total_cost
paste("If 1 tesla share is bought at the low price on each day and sold on the last day's closing price, a total profit of", total_profit, "is made", sep=" ")
