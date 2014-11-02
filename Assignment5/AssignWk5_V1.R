#Q1 Linear Regression Error
sigma=0.1
d=8
N=10
ExpectedEin = sigma^2 *(1-(d+1)/N)
ExpectedEin
#0.001
N=25
sigma^2 *(1-(d+1)/N)
#0.0064
N=100
sigma^2 *(1-(d+1)/N)
#0.0091
N=500
#0.00982

#Q2 NonLinear Transforms


#Q3
#number of parameters + 1

#Q4 Gradient Descent
#use chain rule
#https://www.khanacademy.org/math/multivariable-calculus/partial_derivatives_topic/partial_derivatives/v/partial-derivatives-2


#Q5

surfaceErrorFunction <- function(u,v) {
 (u * exp(v) - 2 * v * exp(-u)) ^ 2
}

PDEu <- function(u,v) {
 2*(exp(v) + 2 * v * exp(-u)) * (u * exp(v) - 2 * v * exp(-u))
}
PDEv <- function(u, v) { 
 2*(exp(v) * u - 2*v*exp(-u)) * (exp(v) * u - 2 * exp(-u))
}

learningRate <- 0.1
tolerance <- 10.0 ^ -14

u=1.0
v=1.0
step <- 0
surfaceError <- surfaceErrorFunction(u,v)
surfaceError
while(surfaceError > tolerance) {
  uDescent <- PDEu(u,v)
  vDescent <- PDEv(u,v)
  u <- u -  uDescent * learningRate
  v <- v -  vDescent * learningRate
  surfaceError <- surfaceErrorFunction(u,v)
  surfaceError
  step <- step + 1
  step
}
step
#10

#q6
# u
#[1] 0.04473629
#> v
#[1] 0.02395871


#Q7
u=1.0
v=1.0
step <- 0
maxSteps <- 15
while(step <= maxSteps) {
  uDescent <- PDEu(u,v)
  vDescent <- PDEv(u,v)
  u <- u -  uDescent * learningRate
  uDescent <- PDEu(u,v)
  vDescent <- PDEv(u,v)
  v <- v -  vDescent * learningRate
  step <- step + 1
  step
}
surfaceError <- surfaceErrorFunction(u,v)
surfaceError
# 0.1326554


#8. logistic regression
fTargetFunction <- function(trainingPoint, targetLineSlope,targetLineCoeff){
   predictedYValue <- (targetLineSlope * trainingPoint$x) + targetLineCoeff
   if(trainingPoint$y > predictedYValue ) {
     trainingPointOrientation = 1 #above line
   } else {
     trainingPointOrientation = -1 #below line
   }
   trainingPointOrientation
}

crossEntropyFn <- function(trainingPoint, result, weights) {
 error <- log(1 + exp(-result * tcrossprod(as.matrix(trainingPoint), as.matrix(weights))))
 return(error)
}

gradient <- function( point, result, weights){
  num = -(result * point)
  den = 1 + (exp(result * tcrossprod(as.matrix(point), as.matrix(weights))))
  return (num/den)
}


train <- function(trainingSet, weights, targetLineSlope, targetLineCoeff){
  targetVector <- numeric()
  for (i in 1:nrow(trainingSet)) {
   trainingPoint <- trainingSet [i,]
   targetValue <- fTargetFunction(trainingPoint,targetLineSlope, targetLineCoeff);
   targetVector <- c(targetVector, targetValue);
  }

  learningRate = 0.01
  previousWeights = data.frame(-1,-1,-1)
  step = 0
  displacement = 1
  threshold = 0.01

  while (displacement >= threshold) {
    sampleSet = sample(1:nrow(trainingSet))

    for (i in 1:length(sampleSet)) {
      feature = trainingSet[i,]
      result = targetVector[i]
    
      descent = gradient(feature, result,weights)
      weights = weights - learningRate * descent
    }
    displacement = norm(as.matrix(previousWeights) - as.matrix(weights))
    previousWeights = weights
    step = step + 1
    #print( paste("Disp", displacement))
  }
  print(paste("Wts", weights, "step", step))
  return(weights)
}

avgCrossEntropy <- 0.0
trainingPoints <- 100
testingPoints <- 1000

maxIteration=10
iterationCount = 1
while (iterationCount <= maxIteration) {
  #Generate targetLine
  targetLine <- data.frame(runif(2,-1,1),runif(2,-1,1))
  names(targetLine ) <- c('x','y')

  fit <- lm(targetLine$y~targetLine$x)
  targetLineSlope <- fit$coefficients[2]
  targetLineCoeff <- fit$coefficients[1]

  #Initialize weights to 0
  weights  <- data.frame(0,0,0)
  names(weights) <- c("w0","w1","w2")

  trainingSet <- data.frame(1,runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
  names(trainingSet) <- c('c','x','y')
  weights <- train(trainingSet, weights, targetLineSlope, targetLineCoeff)

  testingSet <- data.frame(1,runif(testingPoints, -1, 1),runif(testingPoints, -1, 1))
  names(testingSet) <- c('c','x','y')
  crossEntropy <- 0
  for (i in 1:nrow(testingSet)) {
    testingPoint <- testingSet [i,]
    result <- fTargetFunction(testingPoint,targetLineSlope, targetLineCoeff);
    crossEntropyVal <-    crossEntropyFn(testingPoint,result,  weights);
    #print(crossEntropyVal);
    crossEntropy = crossEntropy + crossEntropyVal
  }
  print(paste("CrossEntropy Error Avg",crossEntropy/nrow(testingSet)))
  avgCrossEntropy <- avgCrossEntropy + (crossEntropy/nrow(testingSet))
  iterationCount = iterationCount +1
}
print(paste("CrossEntropy Error Avg Mult Iterations",avgCrossEntropy/maxIteration))


#q8.
# 0.11

#q9
#353




#q1. c
#q2
#q3. c
#q4. e
#q5. d
#q6. e
#q7. a
#q8. d
#q9. b x a
#q10
