#Q1.

#Hoeffding inequality 
# P(|Ein(g) - Eout(g)| > epison) <= 2*M * e ^( -2 *(epsilon ^2) *N)
#p = 2*M * e^(-2 * epsilon^2 * N);
hoeffding <- function(M, epsilon, N) {
  2*M * exp(-2 * epsilon^2 * N)
}



#If we set epsilon = 0.05 and want the probability bound 2 *M*e^(-2 *epsilon ^2*N)
#to be at most 0:03, what is the least number of examples N (among the given
#choices) that is needed for the case M = 1?

answers=c(500,1000,1500,2000)
epsilon = 0.05 #deviation of ein from eout
delta = 0.03 #probability ein deviates from eout  more than the epsilon

M =1 
hoeffding(M, epsilon,answers[1]) < delta
#0.1647
hoeffding(M, epsilon,answers[2])  < delta
#0.01347589
hoeffding(M, epsilon,answers[3])  < delta
#0.001106169
hoeffding(M, epsilon,answers[4])  < delta
#9.079986e-05

#
#Q2,
M=10
hoeffding(M, epsilon,answers[1])  < delta
#1.6417
hoeffding(M, epsilon,answers[2])  < delta
# 0.1347589
hoeffding(M, epsilon,answers[3])  < delta
#0.01106169
hoeffding(M, epsilon,answers[4])  < delta
#0.0009079986

#Q3
M=100
hoeffding(M, epsilon,answers[1])  < delta
#16.417
hoeffding(M, epsilon,answers[2])  < delta
#1.347589
hoeffding(M, epsilon,answers[3])  < delta
#0.1106169
hoeffding(M, epsilon,answers[4])  < delta
#0.009079986



#Q4
#http://www.quora.com/How-do-you-calculate-break-point-of-a-learning-algorithm


#Q5
i, v, ii

#Q6
#single interval, k=3
# +++
# ++-
# +--
# +-+ <-- Not valid
# -++
# -+-
# --+
# ---

#two intervals
#http://www.quora.com/How-do-you-calculate-break-point-of-a-learning-algorithm
#if you have 4 fours, can shatter all points + - | + -
#if you have 5 points, you can't shatter combination + - + - + and hence breakpoint is 5


#Q7
#http://book.caltech.edu/bookforum/archive/index.php/t-4232.html
#http://book.caltech.edu/bookforum/archive/index.php/t-4234.html
#http://www.quora.com/Explain-VC-dimension-and-shattering-in-lucid-Way
#http://www-alg.ist.hokudai.ac.jp/~thomas/WALT/intr2.html
#Single interval
#C(N+1, 2) + 1
#1/2(N ^2) + 1/2(N) +1

#break pt 2M+1 -> M=1  k =3

#Two interval ?? guess
#C(N=1,4) + C(N+1, 2) + 1
#break pt 2M+1 -> M=4  k =5

#Q8
#M interval
#break pt -> 2M+1

#Q9

#Q10


q1. b 
q2. c 
q3. d
q4. b
q5. b
q6. c
q7. c guessed
q8. d
q9. c x -> guess d
q10. d x -> guess b


