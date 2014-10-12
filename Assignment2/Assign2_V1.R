require(reshape)
require(MASS)
set.seed(100)

`%notin%` <- function(x,y) !(x %in% y)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

numberOfTosses <- 10
numberOfCoins <- 1000
numOfSimulations <-100000
rbinom(1,10,0.5)
PHead <- 0.5^10 #fair coin #each coin flipped independently 10 times
c1 <- 1
v_1 = rbinom(1,numberOfTosses,0.5)/numberOfTosses


c_random <- sample(numberOfCoins, 1)
v_rand <- 0

for (runId in 1:numOfSimulations) {
  v_rand = v_rand+ (rbinom(1,numberOfTosses,0.5)/numberOfTosses)
}
v_rand <- v_rand/numOfSimulations
v_rand

v_min <-0
for (runId in 1:numOfSimulations) {
  noOfHeads <-data.frame(rbinom(numberOfCoins,numberOfTosses,0.5))
  v_min = v_min + (min(noOfHeads[,1])/numberOfCoins)
}
v_min <- v_min/numOfSimulations
#Q1 b
v_min

#Q2.  Which coin or coins satisfy Hoefding's Inequality?
#Consider a set of r independent random variables {X1, . . . , Xr}. . If we know ai <= Xi <= bi , 
# then let deltai = bi - ai
#Let M = Sum Xi of i from 1 to r
#Then for any alpha  belongng to   (0, 1/2)
#  Pr[|M - E[M]| > alpha]
##d

#Type I false positive reject Ho when it is actually true. Smoke alarm goes off when actually there is no fire
#Type II false negative

#False Accept: accept when imposter is not an actual customer
#False Reject: reject when it is actuall customer

#Q3. What is the probability of error that h makes in approximating y?

# h makes an error with prob of u in approx f
# h with noisy version: P(y|x) = lamda if y= f(x) , 1-lamda if y != f(x)
#False Reject  (1-lambda) * (1-u)*  + False Accept (lamda) *  Probab of making the error mu

#====================
##If we use the same h to approximate a noisy version of f given by: P(y|x)  = lamda if y = f(x) , or 1-lamda if y != fx()

#http://book.caltech.edu/bookforum/showthread.php?p=11682

#You have a hypothesis function, h(x) that you train on a target function, y=f(x), but it makes an error with probability mu (oh well, at least you tried ). Now, after finding h(x), you apply that to some noisy data set (real-world data).

#Now, the probability that h(x) makes an error in noiseless data is mu and the probability that it doesn't is 1-mu. 

#In addition, the probability that you make an error, simply due to noise, is 1-lambda, with lambda probability that the noise produces no error.

#Therefore, since these are binary functions, the probability that you actually make an error when you apply h(x) to a noisy version of f(x) is: "the probability that there is an error due to noise (1-lambda), AND no error due to the "deterministic" error (1-u) OR the probability that there is no error due to noise (lambda) AND there is a "deterministic" error (mu). 

#Note, the probability distributions for "mu" and "lambda" are statistically independent (this is the assumption).

#Therefore: P_{error} = P_{noise error}*P_{no mu error} + P_{no noise error}*P_{mu error} = (1-lambda)*(1-mu) + lambda*mu.

#Q4. At what value of lambda will the performance of h be independent of mu 

#set
#lamnda to 0, P_{error) = (1-0)(1-u) + 0
#lamnda to 0.5 , P_{error) = (1-0.5) (1-u) + .5*mu = .5(1-u) + .5u = .5 -.5u + .5u = .5
#lamnda to 1, P(_error) = (1-1)*(1-u) + 1*u = 0+u

#For question 4, you can see if you set lambda = 0.5, that P_{error} reduces to 1/2, and mu drops out. Intuitively, what you are saying if lambda = 0.5 is that your noise is so bad, that half the time, you are making errors. Well, in this situation, you don't expect mu to influence the outcome because your data is already uniformly random. 

#Q5.

fTargetFunction <- function(trainingPoint, targetLineSlope,targetLineCoeff){
   predictedYValue <- (targetLineSlope * trainingPoint$x) + targetLineCoeff
   if(trainingPoint$y > predictedYValue ) {
     trainingPointOrientation = 1 #above line
   } else {
     trainingPointOrientation = -1 #below line
   }
   trainingPointOrientation
}

fNonLinearTargetFunction <- function(trainingPoint) {
   targetValue <- sign(trainingPoint$x ^2 +  trainingPoint$y ^2  - 0.6)
   targetValue
}


gHypothesisFunction <- function(trainingPoint, weights){
  crossprod <-  crossprod(t(trainingPoint), t(weights))
  sign(crossprod)
}

gHypothesisFunctionWeightsUsingLM <- function(trainingSet, targetLineSlope, targetLineCoeff){
   targetVector <- numeric()
   for (i in 1:nrow(trainingSet)) {
   	trainingPoint <- trainingSet [i,]
	targetValue <- fTargetFunction(trainingPoint,targetLineSlope, targetLineCoeff);
	targetVector <- c(targetVector, targetValue);
   }
   trainingSetAsMatrix=data.matrix(trainingSet)
   pseudoinverse = pseudoinverse(trainingSetAsMatrix)
   weights <- tcrossprod(pseudoinverse,t(targetVector))
   return(weights)
}

trainPerceptron <- function(runId, trainingPoints,maxIteration,weights,trainingSet) {
   if(missing(maxIteration)) {
      maxIteration = 100000000;
   }
   #Initialize weights to 0
   if(missing(weights)) {
     weights  <- data.frame(0,0,0)
     names(weights) <- c("w0","w1","w2")
   } 

   #Generate targetLine
   targetLine <- data.frame(runif(2,-1,1),runif(2,-1,1))
   names(targetLine ) <- c('x','y')

   fit <- lm(targetLine$y~targetLine$x)
   targetLineSlope <- fit$coefficients[2]
   targetLineCoeff <- fit$coefficients[1]

   #Generate training set
   if(missing(trainingSet)) {
     print("Generating a new trainingSet");
     trainingSet <- data.frame(1,runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
     names(trainingSet) <- c('c','x','y')
   }

   iterationCount = 1

   #print(paste("Number of rows in trainingSet: ", nrow(trainingSet)))
   #print(paste("Max iterator: ", maxIteration))
   while (iterationCount <= maxIteration) {
     #print(paste("Iteration: ", iterationCount))
     misClassifiedSet = data.frame()
     for (i in 1:nrow(trainingSet)) {
   	trainingPoint <- trainingSet [i,]
        #print(paste(i," - TrainingPoint: ", trainingPoint[1]," ",trainingPoint[2]))
   
        targetFunctionValue <- fTargetFunction(trainingPoint,targetLineSlope,targetLineCoeff)

   	hypotheziedValue <- gHypothesisFunction(trainingPoint,weights)
        #print(paste("For ", trainingPoint[1], "", trainingPoint[2], "hypothezied value: ", hypotheziedValue,  "TrainingPtOrientation", targetFunctionValue))
   	if(hypotheziedValue  != targetFunctionValue ) {
      	   if(nrow(misClassifiedSet) ==0) {  #create a new data frame
             misClassifiedSet <- cbind(trainingPoint,targetFunctionValue)
      	   } else {
             misClassifiedSet <- insertRow (misClassifiedSet,cbind(trainingPoint,targetFunctionValue),nrow(misClassifiedSet))
	   }
           #print(paste("Number of rows in misClassifiedSet: ", nrow(misClassifiedSet)))
      	}
     }
     if (nrow(misClassifiedSet) == 0 ) {
       print(paste("Run Id ", runId, " match at iteration ", iterationCount, " with weights ", weights[1], "", weights[2], weights[3]))
       return (list("targetLineSlope"=targetLineSlope, "targetLineCoeff"=targetLineCoeff, "weights"=weights, 
		     "iterationCount" = iterationCount, "success"=TRUE))
      }
     #print(paste("Number of rows in misClassifiedSet:-> ", nrow(misClassifiedSet)))

     #pick a random misclassified point and adjust weight
     misClassifiedPoint <- misClassifiedSet[sample(nrow(misClassifiedSet), 1), ]
     #print(paste("Randomly selected misclassified pt", misClassifiedPoint[1],"",misClassifiedPoint[2],"",misClassifiedPoint[3]))

     #adjust the weight in the orientation by a learning rate of [1,-1]
     weights$w1 = weights$w1 + (misClassifiedPoint$x*(misClassifiedPoint$targetFunctionValue))
     weights$w2 = weights$w2 + (misClassifiedPoint$y*(misClassifiedPoint$targetFunctionValue))
     weights$w0 = weights$w0 + (misClassifiedPoint$c*(misClassifiedPoint$targetFunctionValue))#change the orientation

     #print(paste("Adjusted weights ", weights[1], "", weights[2]))
     iterationCount =  iterationCount+1
   }
   print(paste("Run Id ", runId, " No match after ", (iterationCount -1), " iterations"," weights ", weights[1], "", weights[2], weights[3]))
   return (list("targetLineSlope"=targetLineSlope, "targetLineCoeff"=targetLineCoeff, "weights"=weights, 
		"iterationCount" = iterationCount, "success"=FALSE))
 }


trainLM <- function(trainingPoints) {

   #Generate targetLine
   targetLine <- data.frame(runif(2,-1,1),runif(2,-1,1))
   names(targetLine ) <- c('x','y')

   fit <- lm(targetLine$y~targetLine$x)
   targetLineSlope <- fit$coefficients[2]
   targetLineCoeff <- fit$coefficients[1]


   #Generate training set
   trainingSet <- data.frame(1,runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
   names(trainingSet) <- c('c','x','y')

   #Get weights using LM
   weights <- t(data.frame(gHypothesisFunctionWeightsUsingLM(trainingSet, targetLineSlope, targetLineCoeff)))
   colnames(weights) <- c("w0","w1","w2")
   rownames(weights) <- c(1)
   return (list("targetLineSlope"=targetLineSlope, "targetLineCoeff"=targetLineCoeff, "weights"=weights, 
		"trainingSet" = trainingSet, "success"=TRUE))
}

trainingPoints <- 100
numOfSimulations <-1000

#Q5
#Take N = 100. Use Linear Regression to find g and evaluate Ein, the fraction of
#in-sample points which got classied incorrectly. Repeat the experiment 1000
#times and take the average. Which of the following values is closest to the
#average Ein? (closest is the option that makes the expression jyour answer ??
#given optionj closest to 0. Use this definition of closest here and throughout)

#In sample error
errorCount = 0
trainedFunctionLMResults <- trainLM(trainingPoints) 
trainingSet <- trainedFunctionLMResults$trainingSet
for (runId in 1:numOfSimulations) {
  runErrorCount =0
  for (i in 1:nrow(trainingSet)) {
    targetFunctionValue <- fTargetFunction(trainingSet[i,], trainedFunctionLMResults$targetLineSlope,trainedFunctionLMResults$targetLineCoeff)
    tcrossprod <- tcrossprod(data.matrix(trainingSet[i,]), data.matrix(trainedFunctionLMResults$weights))
    hypothesizedFunctionValue <- sign(tcrossprod)
    if(targetFunctionValue != hypothesizedFunctionValue) {
      errorCount = errorCount + 1
      runErrorCount = runErrorCount + 1
    }
  }
  print(paste("Error Count at runId ", runId, " is " , runErrorCount))
}
print(paste("Total Error Count", errorCount, "Avg In Sample error is [", errorCount/(numOfSimulations*trainingPoints),"]"))

#"Total Error Count 3000 Avg In Sample error is [ 0.03 ]"
#(closest is the option that makes the expression jyour answer ??
#given optionj closest to 0.

#Q6 Out Of sample
testingPoints <- 100
errorCount = 0
for (runId in 1:numOfSimulations) {
  runErrorCount =0
  testingSet <- data.frame(1,runif(testingPoints, -1, 1),runif(testingPoints, -1, 1))
  names(testingSet) <- c('c','x','y')
  for (i in 1:nrow(testingSet)) {
    targetFunctionValue <- fTargetFunction(testingSet[i,], trainedFunctionLMResults$targetLineSlope,trainedFunctionLMResults$targetLineCoeff)
    tcrossprod <- tcrossprod(data.matrix(testingSet[i,]), data.matrix(trainedFunctionLMResults$weights))
    hypothesizedFunctionValue <- sign(tcrossprod)
    if(targetFunctionValue != hypothesizedFunctionValue) {
      errorCount = errorCount + 1
      runErrorCount = runErrorCount + 1
    }
  }
  print(paste("Error Count at runId ", runId, " is " , runErrorCount))
}
print(paste("Total Error Count", errorCount, "Avg Out of Sample error is [", errorCount/(numOfSimulations*testingPoints),"]"))

#"Total Error Count 266 Avg Out of Sample error is [ 0.0266 ]"

#Q7
maxIterations <- 1000
trainingPoints <- 10
numOfSimulations <-1000

#runId <- 1
results = data.frame()
for (runId in 1:numOfSimulations) {
  trainedFunctionLMResults <- trainLM(trainingPoints) 
  trainingSet <- trainedFunctionLMResults$trainingSet
  weights <- data.frame(trainedFunctionLMResults$weights)
  trainedFunctionPerceptronResults <- trainPerceptron(runId, trainingPoints,maxIterations,weights, trainingSet) 
  resultColumn <- cbind("targetLineSlope"=trainedFunctionPerceptronResults$targetLineSlope,"targetLineCoeff"=trainedFunctionPerceptronResults$targetLineCoeff,
         "w1"=trainedFunctionPerceptronResults$weights$w1,"w2"=trainedFunctionPerceptronResults$weights$w2, "w0"=trainedFunctionPerceptronResults$weights$w0, "iterationCount" = trainedFunctionPerceptronResults$iterationCount, "success"=trainedFunctionPerceptronResults$success)
  #print(resultColumn)
  results<-rbind(results,resultColumn)
}
#remove all runs which failed to find match within configured number of iterations
results<-results[results$success == 1,]
print (results)
print(paste("Q7. Avg no of iterations is[", mean(results$iterationCount) , "] for " , trainingPoints ," data points using ", numOfSimulations, " simulations "))

#[1] "Q7. Avg no of iterations is[ 8.5 ] for  10  data points using  1000  simulations "


#8
trainingPoints <- 1000
trainingSet <- data.frame(1,runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
names(trainingSet) <- c('c','x','y')
targetVector <- numeric()
for (i in 1:nrow(trainingSet)) {
 trainingPoint <- trainingSet [i,]
 targetValue <- fNonLinearTargetFunction(trainingPoint);
 targetVector <- c(targetVector, targetValue);
}
#Generate noise for 10% of values by flipping the sign
targetVector = data.matrix(targetVector)
sampleTargetVectorIndexes=sample(nrow(targetVector),0.1*nrow(targetVector),)
targetVector[sampleTargetVectorIndexes,] = targetVector[sampleTargetVectorIndexes,] * -1
targetVector = as.vector(targetVector)
trainingSetAsMatrix=data.matrix(trainingSet)
pseudoinverse = pseudoinverse(trainingSetAsMatrix)
weights <- data.frame(t(tcrossprod(pseudoinverse,t(targetVector))))
names(weights) <- c("w0","w1","w2")

#In sample error
errorCount = 0
for (runId in 1:numOfSimulations) {
  runErrorCount =0
  for (i in 1:nrow(trainingSet)) {
    targetFunctionValue <- targetVector[i]
    tcrossprod <- tcrossprod(data.matrix(trainingSet[i,]), data.matrix(weights))
    hypothesizedFunctionValue <- sign(tcrossprod)
    if(targetFunctionValue != hypothesizedFunctionValue) {
      errorCount = errorCount + 1
      runErrorCount = runErrorCount + 1
    }
  }
  print(paste("Error Count at runId ", runId, " is " , runErrorCount))
}
print(paste("Q8 Total Error Count", errorCount, "Avg In Sample error is [", errorCount/(numOfSimulations*trainingPoints),"]"))
#[1] "Q8 Total Error Count 511000 Avg In Sample error is [ 0.511 ]"

#9. Transform training data
transformedTrainingSet = data.frame()
for (i in 1:nrow(trainingSet)) {
  trainingPoint <- trainingSet[i,]
  transformedTrainingPoint <- c(trainingPoint$c,trainingPoint$x, trainingPoint$y, trainingPoint$x*trainingPoint$y,trainingPoint$x^2, trainingPoint$y^2)
  transformedTrainingSet <-  rbind(transformedTrainingSet, transformedTrainingPoint)
}
names(transformedTrainingSet) <- c("c","x","y","xy","x2","y2")
## Train on the  transformed data points
targetVector <- numeric()
for (i in 1:nrow(transformedTrainingSet)) {
 trainingPoint <- transformedTrainingSet [i,]
 targetValue <- fNonLinearTargetFunction(trainingPoint);
 targetVector <- c(targetVector, targetValue);
}
##Train
transformedTrainingSetAsSet=data.matrix(transformedTrainingSet)
pseudoinverse = pseudoinverse(transformedTrainingSetAsSet)
weights <- data.frame(t(tcrossprod(pseudoinverse,t(targetVector))))
names(weights) <- c("w0","w1","w2","w3","w4","w5")

#Given probable hypothesized values
weightsOptions = data.frame()
weightsOptions = rbind(weightsOptions, c(-1,-0.05,0.08,.13,1.5,1.5))
weightsOptions = rbind(weightsOptions, c(-1,-0.05,0.08,.13,1.5,15))
weightsOptions = rbind(weightsOptions, c(-1,-0.05,0.08,.13,15,1.5))
weightsOptions = rbind(weightsOptions, c(-1,-1.5,0.08,.13,.05,.05))
weightsOptions = rbind(weightsOptions, c(-1,-0.05,0.08,1.5,.15,.15))
names(weightsOptions) =  c("w0","w1","w2","w3","w4","w5")

for (j in 1:nrow(weightsOptions)) {
  errorCount=0
  weightsOption = weightsOptions[j,]
  for (i in 1:nrow(transformedTrainingSet)) {
    trainingPoint <- transformedTrainingSet[i,]
    hypothesizedValue <- sign(tcrossprod(data.matrix(trainingPoint), data.matrix(weightsOption)))
    if(hypothesizedValue != targetVector[i]) {
      errorCount  = errorCount + 1 
    }
  }
  print(paste("Error rate for option  ",j,   errorCount/nrow(transformedTrainingSet)))
}
#[1] "Error rate for option   1  is  0.1"
#[1] "Error rate for option   2  is  0"
#[1] "Error rate for option   3  is  0.1"
#[1] "Error rate for option   4  is  0.2"
#[1] "Error rate for option   5  is  0.7"

#Q10 Out of sample error calculation
trainingPoints <- 1000
trainingSet <- data.frame(1,runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
names(trainingSet) <- c('c','x','y')
targetVector <- numeric()
for (i in 1:nrow(trainingSet)) {
 trainingPoint <- trainingSet [i,]
 targetValue <- fNonLinearTargetFunction(trainingPoint);
 targetVector <- c(targetVector, targetValue);
}

#Add noise for 10%
targetVector = data.matrix(targetVector)
sampleTargetVectorIndexes=sample(nrow(targetVector),0.1*nrow(targetVector),)
targetVector[sampleTargetVectorIndexes,] = targetVector[sampleTargetVectorIndexes,] * -1
targetVector = as.vector(targetVector)

transformedTrainingSet = data.frame()
for (i in 1:nrow(trainingSet)) {
     trainingPoint <- trainingSet[i,]
     transformedTestingPoint <- c(trainingPoint$c,trainingPoint$x, trainingPoint$y, trainingPoint$x*trainingPoint$y,trainingPoint$x^2, trainingPoint$y^2)
     transformedTrainingSet <-  rbind(transformedTrainingSet, transformedTestingPoint)
}
transformedTrainingSetAsMatrix=data.matrix(transformedTrainingSet)
pseudoinverse = pseudoinverse(transformedTrainingSetAsMatrix)
weights <- data.frame(t(tcrossprod(pseudoinverse,t(targetVector))))
names(weights) <- c("w0","w1","w2","w3","w4","w5")

testingPoints <- 1000
errorCount = 0
for (runId in 1:numOfSimulations) {
  runErrorCount =0
  testingSet <- data.frame(1,runif(testingPoints, -1, 1),runif(testingPoints, -1, 1))
  names(testingSet) <- c('c','x','y')
  transformedTestingSet = data.frame()
  for (i in 1:nrow(testingSet)) {
     testingPoint <- testingSet[i,]
     transformedTestingPoint <- c(testingPoint$c,testingPoint$x, testingPoint$y, testingPoint$x*testingPoint$y,testingPoint$x^2, testingPoint$y^2)
     transformedTestingSet <-  rbind(transformedTestingSet, transformedTestingPoint)
  }
  names(transformedTestingSet) <- c("c","x","y","xy","x2","y2")
  targetTestingVector <- numeric()
  for (i in 1:nrow(transformedTestingSet)) {
    targetTestingFunctionValue <- fNonLinearTargetFunction(transformedTestingSet[i,]);
    targetTestingVector <- c(targetTestingVector, targetTestingFunctionValue);
  }
  #Add 10% noise
  targetTestingVector = data.matrix(targetTestingVector)
  sampleTestingTargetVectorIndexes=sample(nrow(targetTestingVector),0.1*nrow(targetTestingVector),)
  targetTestingVector[sampleTestingTargetVectorIndexes,] = targetTestingVector[sampleTestingTargetVectorIndexes,] * -1
  targetTestingVector = as.vector(targetTestingVector)

  #Measure out of sample erro
  for (i in 1:nrow(transformedTestingSet)) {
    tTestingCrossProd <- tcrossprod(data.matrix(transformedTestingSet[i,]), data.matrix(weights))
    hypothesizedFunctionValue <- sign(tTestingCrossProd)
    if(targetTestingVector[i] != hypothesizedFunctionValue) {
      errorCount = errorCount + 1
      runErrorCount = runErrorCount + 1
    }
  }
  print(paste("Error Count at runId ", runId, " is " , runErrorCount))
}
print(paste("Total Error Count", errorCount, "Avg Out of Sample error is [", errorCount/(numOfSimulations*testingPoints),"]"))
# "Total Error Count 136321 Avg Out of Sample error is [ 0.136321 ]"


#Q1 b
#Q2 d
#Q3 e
#Q4 b
#Q5 b x  corrects  c ?0.03 closest to 0.01
#Q6 c
#Q7 b x (a closest to zero)  a->1 b->15  my calculated value was 8.5
#Q8 b x d (0.51)
#Q9 b x  ??why a
#Q10 b
