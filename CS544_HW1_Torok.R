#Question 1

#A)
scores  = c(40, 88, 60, 23, 76, 51, 59, 99, 96, 34)

#B)
(n = length(scores))

#C)
(first_and_second = scores[1:2])

#D)
(first_and_last = scores[c(1,n)])

#E)
(middle_two = scores[c((n/2),((n/2)+1))])

#Question 2

#A)
(avg_score = mean(scores))

#B)
(below_avg = scores <= avg_score)

#C)
(above_avg = scores >= avg_score)

#D)
(count_below_avg = sum(below_avg))

#E)
(count_above_avg = sum(above_avg))

#Question 3

#A)
(scores_below_avg = scores[c(below_avg)])

#B) 
(scores_above_avg = scores[c(above_avg)])

#Question 4

#A)
(odd_index_values = scores[c(seq(1, n, by=2))])

#B)
(even_index_values = scores[c(seq(2, n, by=2))])

#Question 5

#A)
(format_scores_version1 = paste(LETTERS[1:n], scores[1:n], sep = "="))

#B)
(format_scores_version2 = paste(LETTERS[n:1], scores[1:n], sep = "="))

#Question 6

#A)
(scores_matrix = matrix(scores, nrow = 2, ncol = (n/2), byrow = TRUE))

#B)
(first_and_last_version1 = scores_matrix[, c(1, ncol(scores_matrix))])

#Question 7

#A)
(named_matrix = scores_matrix)
row_names = paste("Quiz", seq(1:nrow(named_matrix)), sep="_")
col_names = paste("Student", seq(1:ncol(named_matrix)), sep="_")
dimnames(named_matrix) = list(row_names, col_names)
named_matrix

#B)

(first_and_last_version2 = named_matrix[, c(1, ncol(scores_matrix))])

