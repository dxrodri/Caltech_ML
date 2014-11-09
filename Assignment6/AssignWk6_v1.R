require(reshape)
require(MASS)
require(corpcor)

setwd("C:/Users/Sham/Documents/DataScience/Caltech_ML/Assignments/Caltech_ML/Assignment6")
trainingSet = read.table("in.dta.txt")
testingSet = read.table("out.dta")
transformedTrainingSet = data.frame(matrix(ncol = 8, nrow = nrow(trainingSet)))
for (i in 1:nrow(trainingSet)) {
  trainingPoint <- trainingSet[i,]
  transformedTrainingPoint <- c(1,trainingPoint$V1,trainingPoint$V2,trainingPoint$V1^2,trainingPoint$V2^2,
				trainingPoint$V1*trainingPoint$V2,abs(trainingPoint$V1-trainingPoint$V2),abs(trainingPoint$V1+trainingPoint$V2))
  transformedTrainingSet[i,] <-  as.vector(transformedTrainingPoint)
}
transformedTrainingSetAsSet=data.matrix(transformedTrainingSet)
pseudoinverse = pseudoinverse(as.matrix(transformedTrainingSetAsSet))
weights <- data.frame(tcrossprod(pseudoinverse,t(as.matrix(trainingSet[,3]))))

trainingResult <- sign(tcrossprod(data.matrix(transformedTrainingSetAsSet), t(data.matrix(weights))))
inSampleError <- length(which(trainingResult != as.matrix(trainingSet[,3])))/length(trainingResult)
inSampleError
#0.02857143

transformedTestingSet = data.frame(matrix(ncol = 8, nrow = nrow(testingSet)))

for (i in 1:nrow(testingSet)) {
  testingPoint <- testingSet[i,]
  transformedTestingPoint <- c(1,testingPoint$V1,testingPoint$V2,testingPoint$V1^2,testingPoint$V2^2,
				testingPoint$V1*testingPoint$V2,abs(testingPoint$V1-testingPoint$V2),abs(testingPoint$V1+testingPoint$V2))
  transformedTestingSet[i,] <-  as.vector(transformedTestingPoint)
}

testingResult <- sign(tcrossprod(data.matrix(transformedTestingSet), t(data.matrix(weights))))
outSampleError <- length(which(testingResult != as.matrix(testingSet[,3])))/length(testingResult)
outSampleError
#0.084


#Q3
k <- -3
lambda <- 10 ^ k
wtDecayTerm <-crossprod(as.matrix(weights), (as.matrix(weights)))*(lambda/nrow(trainingSet))
transformedTrainingSetWithDecay <- transformedTrainingSet+wtDecayTerm

transformedTrainingSetWithDecayAsSet=data.matrix(transformedTrainingSetWithDecay)
pseudoinverseWithDecay = pseudoinverse(as.matrix(transformedTrainingSetWithDecayAsSet))
weightsWithDecay <- data.frame(tcrossprod(pseudoinverseWithDecay,t(as.matrix(trainingSet[,3]))))

trainingResult <- sign(tcrossprod(data.matrix(transformedTrainingSetWithDecayAsSet), t(data.matrix(weightsWithDecay))))
inSampleError <- length(which(trainingResult != as.matrix(trainingSet[,3])))/length(trainingResult)
inSampleError
#0.02857143

testingResult <- sign(tcrossprod(data.matrix(transformedTestingSet), t(data.matrix(weightsWithDecay))))
outSampleError <- length(which(testingResult != as.matrix(testingSet[,3])))/length(testingResult)
outSampleError
#0.08


#Q4
k <- 3
lambda <- 10 ^ k
wtDecayTerm1 <-crossprod(as.matrix(weights), (as.matrix(weights)))*(lambda/nrow(trainingSet))
transformedTrainingSetWithDecay1 <- transformedTrainingSet+wtDecayTerm1

transformedTrainingSetWithDecayAsSet1=data.matrix(transformedTrainingSetWithDecay1)
pseudoinverseWithDecay1 = pseudoinverse(as.matrix(transformedTrainingSetWithDecayAsSet1))
weightsWithDecay1 <- data.frame(tcrossprod(pseudoinverseWithDecay1,t(as.matrix(trainingSet[,3]))))

trainingResult <- sign(tcrossprod(data.matrix(transformedTrainingSet), t(data.matrix(weightsWithDecay1))))
inSampleError <- length(which(trainingResult != as.matrix(trainingSet[,3])))/length(trainingResult)
inSampleError
#0.5714286

testingResult <- sign(tcrossprod(data.matrix(transformedTestingSet), t(data.matrix(weightsWithDecay1))))
outSampleError <- length(which(testingResult != as.matrix(testingSet[,3])))/length(testingResult)
outSampleError
#0.528


#Q5
#K=3  0.528
#K=2 0.528
#K=1 0.084
#K=0 0.084
#K=-1 0.056
#K=-2 0.076
#K=-3  0.08

#Q6
#0.056

#Q7 Regularization with Polynomials

#Q8 NN

#q1. b
#q2. a
#q3. d
#q4. e
#q5. d
#q6. b 
#q7. c?
#q8. d
#q9. q
#q10.e
