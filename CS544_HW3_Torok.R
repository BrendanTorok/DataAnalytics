library(UsingR)
library(plotly)
#Part 1
#A)
par(mai = c(2, 2, 1, 1))
par(mgp=c(7,1,0))
forbes <- read.csv("https://people.bu.edu/kalathur/datasets/forbes.csv")
barplot(table(forbes$country), ylim=c(0,800), xlab="Country", ylab="Frequency",
        main="Number of Billionaires Per Country", las=2, col=rainbow(30))

#B)
gender.df = as.data.frame(table(forbes$gender))
plot_ly(gender.df, labels = ~Var1, values = ~Freq, type="pie", 
        title = "Distribution of Billionaires by Gender")


#C)
par(mgp=c(8,1,0))
top_5_categories <- names(sort(table(forbes$category), decreasing=TRUE)[1:5])
top_5_categories
top5data <- subset(forbes, category %in% top_5_categories)
barplot(table(top5data$gender, top5data$category), las=2, ylab="Frequency", 
        xlab="Category of Billionaire", ylim=c(0,400),
        main="Number of Billionaires by Gender in Top 5 Categories", 
        col= c("red","blue"), legend.text = TRUE)

#D)
#Based on the above plots, we can infer that billionaires are most likely to be 
#male and from the United States. Additionally, we can infer that the most likely
#source of their wealth is from the finance & investments industry. This 
#industry also has the lowest proportion of female billionaires. Conversely, 
#the least likely billionaire based on the charts would be a female billionaire 
#from an African country.

#Part 2
#A)
us_quarters <- read.csv("https://people.bu.edu/kalathur/datasets/us_quarters.csv")
view(us_quarters)
highest_quarter_denver <- us_quarters[order(us_quarters$DenverMint, 
                                            decreasing=TRUE)[1], ]
(paste("The State with the highest production for the DenverMint is", 
       highest_quarter_denver$State))

highest_quarter_philly <- us_quarters[order(us_quarters$PhillyMint, 
                                            decreasing=TRUE)[1], ]
(paste("The State with the highest production for the PhillyMint is", 
       highest_quarter_philly$State))

lowest_quarter_denver <- us_quarters[order(us_quarters$DenverMint)[1], ]
(paste("The State with the lowest production for the DenverMint is", 
       lowest_quarter_denver$State))

lowest_quarter_philly <- us_quarters[order(us_quarters$PhillyMint)[1], ]
(paste("The State with the lowest production for the PhillyMint is", 
       lowest_quarter_philly$State))


#B)
options(scipen = 7)
us_quarters[c("DenverMint", "PhillyMint")]
m <- as.matrix(us_quarters[c("DenverMint", "PhillyMint")])
barplot(t(m), beside = TRUE, names.arg = us_quarters$State, las=2, 
        ylim=c(0,1000000), legend.text=TRUE, col = c("blue", "grey"))


#C)
par(mgp=c(3,1,0))
boxplot(m, col=c("blue", "grey"), ylim=c(0,1000000), 
        main="Production of Denver and Philly Quarter Mints", 
        ylab="Production (thousands)", xlab="Mint")

#D)
fn.denver = fivenum(us_quarters$DenverMint)
denver.lower <- fn.denver[4] - 1.5*(fn.denver[4] - f[2])
denver.higher <- fn.denver[4] + 1.5*(fn.denver[4] - f[2])
denvermint.outliers <- us_quarters$State[us_quarters$DenverMint < denver.lower 
                                         | us_quarters$DenverMint > denver.higher]
if (length(denvermint.outliers) == 0){
  print("There are no outliers for the DenverMint")
} else{
(paste("The outlier(s) for the DenverMint is", denvermint.outliers))
}

fn.philly = fivenum(us_quarters$PhillyMint)
philly.lower <- fn.philly[4] - 1.5*(fn.philly[4] - f[2])
philly.higher <- fn.philly[4] + 1.5*(fn.philly[4] - f[2])
phillymint.outliers <- us_quarters$State[us_quarters$PhillyMint < philly.lower 
                                         | us_quarters$PhillyMint > philly.higher]
if (length(phillymint.outliers) == 0){
  print("There are no outliers for the PhillyMint")
} else{
(paste("The outlier(s) for the PhillyMint is", phillymint.outliers))
}

#Part 3
#A)
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks.csv")

pairs(stocks[-1])

#B)

(cm <- round(cor(stocks[-1]), 2))

#C)
#1.) From the correlated matrix we can infer that TSLA and FB have the lowest 
#correlation at 0.05, and are therefore not correlated
#2.) From the correlated matrix we can infer that GOOG and MSFT are the most 
#correlated at 0.95, therefore if GOOG or MSFT stocks move, they likely move 
#together in the same direction -> negative or positive stock price loss/gain
#3.) From the pair wise plots and correlation matrix we can see that no stocks
#have a negative correlation, therefore we know that we cannot assume that any
#stocks will move in opposite directions
#4.) From the pairwise plot we can see that GOOG and TSLA may become correlated
#when GOOG stock is either low, or high since the prices look to be positively 
#correlated when removing GOOG prices in the 2400 range

#D)

index_names <- rownames(cm)
for (i in 1:length(index_names)){
  row <- i
  stock_name <- paste("Top 3 For Stock", index_names[row])
  top_3_stocks <- sort(cm[row, ], decreasing = TRUE)[2:4]
  print(stock_name)
  print(top_3_stocks)
  cat("\n")
}


#Part 4
#A)
scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")
hs <- hist(scores$Score)
breaks <- hs$breaks
counts <- hs$counts

for (i in 1:length(counts)) {
  lower_bound <- breaks[i]
  upper_bound <- breaks[i + 1]
  count <- counts[i]
  result <- paste(count, " students in range (", lower_bound, ",", 
                  upper_bound, "]", sep="")
  print(result)
}

#B)
hs.grades <- hist(scores$Score, breaks = c(30, 50, 70, 90))
graded.breaks <- hs.grades$breaks
graded.counts <- hs.grades$counts

for (i in 1:length(graded.counts)) {
  lower_bound <- graded.breaks[i]
  upper_bound <- graded.breaks[i + 1]
  count <- graded.counts[i]
  grade.letter <- LETTERS[3:1]
  result <- paste(count, " students in ", grade.letter[i], " range (", 
                  lower_bound, ",", upper_bound, "]", sep="")
  print(result)
}

