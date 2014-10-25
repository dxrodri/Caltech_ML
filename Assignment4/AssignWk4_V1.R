require(MASS)
require(corpcor)
set.seed(100)
#Q1
#d = 4mH(2N)exp(-1/8 *epsilon ^2 *N)
#epsilon = sqrt(8/N *ln(4mh(2N)/delta)

hoeffding <- function(M, epsilon, N) {
  2*M * exp(-2 * epsilon^2 * N)
}

#growth function
#For N > dvc, use thesimple approximate bound N ^ dvc for the growth function mH(N).
mh <- function (N, dvc) {
  if (N > dvc ){
    k = N^dvc
  } else {
    k = 2^N
  }
  return(k)
}

vc <- function (N, dvc, delta){
  epsilon = sqrt ((8/N) * log ((4 * mh(2*N,dvc)) / delta))
  return(epsilon)
}

dvc = 10; #VC dimension
delta = 0.05 #prob that epsilon will hold
N=400000
vc(N,dvc,delta) < delta
#FALSE
N=420000
vc(N,dvc,delta) <delta
#FALSE
N=440000
vc(N,dvc,delta) <delta
#FALSE
N=460000
vc(N,dvc,delta) <delta
#TRUE


#Q2.
#http://book.caltech.edu/bookforum/showthread.php?t=3964

rademacherPenaltyBound <- function(N, dvc, delta) {
  result = sqrt ((2 * log(2*N*mh(N,dvc)))/N) + sqrt((2/N) * log(1/delta)) + (1/N);
  return (result) #epsilon
}

parrondoandVandenBroekBound <- function(N, dvc, delta) {
  oldepsilon = epsilon= vc(N,dvc,delta)
  repeat { 
    epsilon = sqrt(1/(N)*(2* oldepsilon + log((6*mh(2*N, dvc)/delta))))
    if(abs(epsilon  -oldepsilon) <2) {
      break;
    }
    oldepsilon = epsilon;
  }
  return(epsilon)
}

devroyeBound <-function(N, dvc, delta) {
  oldepsilon = epsilon= vc(N,dvc,delta)
  repeat { 
    epsilon = sqrt(1/(2*N)*(4* oldepsilon*(1+oldepsilon) + log((6*mh(2*N, dvc)/delta))))
    if(abs(epsilon  -oldepsilon) <2) {  #http://book.caltech.edu/bookforum/showthread.php?t=3964
      break;
    }
    oldepsilon = epsilon;
  }
  return(epsilon)
}


#Q2
dvc = 50; #VC dimension
delta = 0.05 #prob that epsilon will hold	
N=10000
vc(N,dvc,delta)
#0.6321749
rademacherPenaltyBound(N, dvc, delta)
#0.3313088
parrondoandVandenBroekBound(N, dvc, delta) 
#0.2238808
devroyeBound(N, dvc, delta)
#0.1587591

#Q3
dvc = 50; #VC dimension
delta = 0.05 #prob that epsilon will hold	
N=5
vc(N,dvc,delta)
#4.25
rademacherPenaltyBound(N, dvc, delta)
#2.81
parrondoandVandenBroekBound(N, dvc, delta) 
#1.774357
devroyeBound(N, dvc, delta)
#3.18

#Q4.
targetFunction <- function(x) {
	return(sin(pi*x))
}



maxIterations <- 1000
trainingPoints <- 1000
numOfSimulations <-1000

trainingSet <- data.frame(runif(trainingPoints, -1, 1))
names(trainingSet ) <- c('x')

#Assume that the training set has only two examples (picked independently), and that the learning algorithm picks the hypothesis that minimizes the mean squared error on the examples.  

targetsVector = vector(mode="numeric", length=trainingPoints)
for (i in 1:trainingPoints) {
  targetsVector[i] = targetFunction(trainingSet[i,])
}

#find mean weight - gbar
weightsVector = vector(mode="numeric", length=numOfSimulations)
for (runId in 1:numOfSimulations) {
    #take two sample points
    samplePoints <- trainingSet[sample(nrow(trainingSet), 2), ]	
    trainingPointsAsMatrix=data.matrix(samplePoints)
    pseudoinverse = pseudoinverse(trainingPointsAsMatrix)
    targetValues <- targetFunction(samplePoints);
    weights <- data.frame(t(tcrossprod(pseudoinverse,t(targetValues))))
    #print(weights)
    weightsVector[runId]=as.numeric(weights)
}
gbar <-mean(weightsVector)
gbar
#[1] 1.443317

gbarTargets =  tcrossprod(as.matrix(trainingSet),gbar); # transpose trainingSet
bias <- mean((gbarTargets- targetsVector)**2) #mean squared
bias
# 0.2395808

#variance
hypothesizedValues <- tcrossprod(weightsVector,as.matrix(trainingSet))
totalMean <-0
for (i in 1:ncol(hypothesizedValues)) {
  totalMean = totalMean + mean((gbarTargets - hypothesizedValues[i,])**2)
}
#variance
totalMean/ncol(hypothesizedValues)
# 0.2027974

#Q7

#VC Dimension
#Q8
mychoose <- function(N,q) {
  if(q> N) {
    return(0)
  } else {
    return(choose(N,q))
  }
}
mh8 <- function(N, q) {
  if (N == 1) {
    return(2)
  } else {
    value = 2 *mh8(N-1,q) - mychoose(N-1,q) 
    return(value)
  }
}
    

for (q in 1:10) {
  dvc=0
  N=0
  repeat { 
    N=N+1
    if (mh8(N,q) < 2 ^N) {
      dvc = N-1
      break
    }
  }
  print(paste("For q = ",q, " dvc is ", dvc))
}

N=1, q=1  -- 1< 2, dvc=0
N=2, q=1

#Q9 b guess

#q1. d
#d2. d
#q3. c
#q4. e
#q5. b
#q6. a
#q7. ?
#8. c
#9. b guess
#10 d x e guess


