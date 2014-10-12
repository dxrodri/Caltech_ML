require(reshape)
set.seed(100)

`%notin%` <- function(x,y) !(x %in% y)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

	
#http://math.stackexchange.com/questions/324589/detecting-whether-a-point-is-above-or-below-a-slope
#If the line equation is : y=ax+b and the coordinates of a point is (alpha,beta) then compare beta and a*(alpha)+b, for example if beta>a*(alpha)+b 
#then the point is above the line, etc.
#fTargetFunction <- function((alpha,beta), a,coeff)

fTargetFunction <- function(trainingPoint, targetLineSlope,targetLineCoeff){
   predictedYValue <- (targetLineSlope * trainingPoint$x) + targetLineCoeff
   if(trainingPoint$y > predictedYValue ) {
     trainingPointOrientation = 1 #above line
   } else {
     trainingPointOrientation = -1 #below line
   }
   trainingPointOrientation
}

gHypothesisFunction <- function(trainingPoint, weights,weightsCoefficient=0){
  crossprod <- weightsCoefficient + crossprod(t(trainingPoint), t(weights))  #weightsCoefficient wo
  sign(crossprod)
}


train <- function(runId, trainingPoints,maxIteration) {
   if(missing(maxIteration)) {
      maxIteration = 100000000;
   }
   #Initialize weights to 0
   weights  <- data.frame(0,0)
   names(weights) <- c("w1","w2")

   #Generate targetLine
   targetLine <- data.frame(runif(2,-1,1),runif(2,-1,1))
   names(targetLine ) <- c('x','y')

   fit <- lm(targetLine$y~targetLine$x)
   targetLineSlope <- fit$coefficients[2]
   targetLineCoeff <- fit$coefficients[1]


   #Generate training set
   trainingSet <- data.frame(runif(trainingPoints, -1, 1),runif(trainingPoints, -1, 1))
   names(trainingSet) <- c('x','y')

   iterationCount = 1

   #print(paste("Number of rows in trainingSet: ", nrow(trainingSet)))
   #print(paste("Max iterator: ", maxIteration))
   #while (TRUE) {
   weightsCoefficient = 0
   while (iterationCount <= maxIteration) {
     #print(paste("Iteration: ", iterationCount))
     misClassifiedSet = data.frame()
     for (i in 1:nrow(trainingSet)) {
   	trainingPoint <- trainingSet [i,]
        #print(paste(i," - TrainingPoint: ", trainingPoint[1]," ",trainingPoint[2]))
   
        targetFunctionValue <- fTargetFunction(trainingPoint,targetLineSlope,targetLineCoeff)

   	hypotheziedValue <- gHypothesisFunction(trainingPoint,weights, weightsCoefficient)
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
       print(paste("Run Id ", runId, " match at iteration ", iterationCount, " with weights ", weights[1], "", weights[2]))
       return (list("targetLineSlope"=targetLineSlope, "targetLineCoeff"=targetLineCoeff, "weights"=weights, 
		    "weightsCoefficient"=weightsCoefficient, "iterationCount" = iterationCount, "success"=TRUE))
      }
    
     #print(paste("Number of rows in misClassifiedSet:-> ", nrow(misClassifiedSet)))

     #pick a random misclassified point and adjust weight
     misClassifiedPoint <- misClassifiedSet[sample(nrow(misClassifiedSet), 1), ]
     #print(paste("Randomly selected misclassified pt", misClassifiedPoint[1],"",misClassifiedPoint[2],"",misClassifiedPoint[3]))

     #http://ciml.info/dl/v0_8/ciml-v0_8-ch03.pdf
     #Algorithm 5 PerceptronTrain(D, MaxIter)
#	1: wd   0, for all d = 1 . . .D // initialize weights
#	2: b   0 // initialize bias
#	3: for iter = 1 . . . MaxIter do
#	4: for all (x,y) 2 D do
#	5: a   åDd
#	=1 wd xd + b // compute activation for this example
#	6: if ya  0 then
#	7: wd   wd + yxd, for all d = 1 . . .D // update weights
#	8: b   b + y // update bias
#	9: end if
#	10: end for
#	11: end for
#	12: return w0, w1, . . . , wD, b


     #adjust the weight in the orientation by a learning rate of [1,-1]
     weights$w1 = weights$w1 + (misClassifiedPoint$x*(misClassifiedPoint$targetFunctionValue))
     weights$w2 = weights$w2 + (misClassifiedPoint$y*(misClassifiedPoint$targetFunctionValue))
     weightsCoefficient = weightsCoefficient + misClassifiedPoint$targetFunctionValue #change the orientation

     #print(paste("Adjusted weights ", weights[1], "", weights[2]))
     iterationCount =  iterationCount+1
   }
   print(paste("Run Id ", runId, " No match after ", (iterationCount -1), " iterations"," weights ", weights[1], "", weights[2]))
   return (list("targetLineSlope"=targetLineSlope, "targetLineCoeff"=targetLineCoeff, "weights"=weights, 
		"weightsCoefficient"=weightsCoefficient,"iterationCount" = iterationCount, "success"=FALSE))
 }



trainingPoints <- 10
testingPoints <- 10
maxIterations <- 1000
numOfSimulations <-1000
#runId=1
#result <- train(runId,trainingPoints,maxIterations)

results = data.frame()
for (runId in 1:numOfSimulations) {
  result <- train(runId,trainingPoints,maxIterations) 
  resultColumn <- cbind("targetLineSlope"=result$targetLineSlope,"targetLineCoeff"=result$targetLineCoeff,
         "w1"=result$weights$w1,"w2"=result$weights$w2, "weightsCoefficient"=result$weightsCoefficient, "iterationCount" = result$iterationCount, "success"=result$success)
  #print(resultColumn)
  results<-rbind(results,resultColumn)
}
#remove all runs which failed to find match within configured number of iterations
results<-results[results$success == 1,]
#print (results)
print(paste("Q7. Avg no of iterations is[", mean(results$iterationCount) , "] for " , trainingPoints ," data points using ", numOfSimulations, " simulations "))


#Question  8
#Pick one targetLine
trainFunctionAttributes <- results[1,]
print (paste("Train function used for testing: slope and coeff ",trainFunctionAttributes$targetLineSlope, " ",  trainFunctionAttributes$targetLineCoeff, "",trainFunctionAttributes$trainFunctionAttributes$w1))
#Generate test set
testingSet <- data.frame(runif(testingPoints, -1, 1),runif(testingPoints, -1, 1))
names(testingSet) <- c('x','y')
errorCount = 0
for (runId in 1:numOfSimulations) {
  for (i in 1:nrow(testingSet)) {
    targetFunctionValue <- fTargetFunction(testingSet[i,], trainFunctionAttributes$targetLineSlope,trainFunctionAttributes$targetLineCoeff)
    hypothesizedFunctionValue <- gHypothesisFunction(testingSet[i,], 
  				data.frame(trainFunctionAttributes$w1,trainFunctionAttributes$w2), trainFunctionAttributes$weightsCoefficient)
    if(targetFunctionValue != hypothesizedFunctionValue) {
      errorCount = errorCount + 1
    }
  }
}
print(paste("Q8. Error rate is[", errorCount/(numOfSimulations*testingPoints) , "] for " , testingPoints ," data points using ", numOfSimulations, " simulations "))
