# Load the libraries
library(LiblineaR)
library(jsonlite)
library(textclean)
library(textmining)
library(htm2txt)
library(tidytext)
library(dplyr)
library(sjmisc)
library(stringr)
library(Rfast)
library(data.table)
library(readr)

# Evaluation metrics
measurements <- function(predSentiment, trueSentiment){
  conf <- confusionMatrix(as.factor(predSentiment[unclassified]), 
                          reference = as.factor(trueSentiment[unclassified]),
                          positive = "1")
  
  Acc <- round(conf$overall["Accuracy"][[1]]*100, digits = 1)
  TPR <- conf$byClass["Sensitivity"][[1]]*100
  TNR <- conf$byClass["Specificity"][[1]]*100
  BA <- round(conf$byClass["Balanced Accuracy"][[1]]*100, digits = 1)
  PPV <- conf$byClass["Pos Pred Value"][[1]]*100
  NPV <- conf$byClass["Neg Pred Value"][[1]]*100
  F1Pos <- 2 * (PPV * TPR)/(PPV + TPR)
  F1Neg <- 2 * (NPV * TNR)/(NPV + TNR)
  F1Macro <- round((F1Pos + F1Neg)/2, digits = 1)
  
  TPR <- round(TPR, digits = 1)
  TNR <- round(TNR, digits = 1)
  PPV <- round(PPV, digits = 1)
  NPV <- round(NPV, digits = 1)
  F1Pos <- round(F1Pos, digits = 1)
  F1Neg <- round(F1Neg, digits = 1)
  
  return(c(Acc, TPR, TNR, BA, PPV, NPV, F1Pos, F1Neg, F1Macro))
}


supervised <- function(prepData){
  # Split data for training and testing
  set.seed(2020)
  x=prepData
  y=stocktwits$Sentiment
  train=sample(1:20000,floor(0.8*20000))
  xTrain=x[c(train, (20000+train)),]
  xTest=x[-c(train, (20000+train)),]
  yTrain=y[c(train, (20000+train))]
  yTest=y[-c(train, (20000+train))]
  
  # Center and scale data
  s=scale(xTrain,center=TRUE,scale=TRUE)
  
  # Find the best model with the best cost parameter via 10-fold cross-validations
  tryTypes=c(0:7)
  tryCosts=c(0.0001, 0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10, 15, 20, 25, 30, 50, 75, 100, 500, 1000)
  bestCost=NA
  bestAcc=0
  bestType=NA
  for(ty in tryTypes){
    for(co in tryCosts){
      begin = as.numeric(Sys.time())
      acc=LiblineaR(data=s,target=yTrain,type=ty,cost=co,bias=1,cross=5,verbose=FALSE)
      cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
      if(acc>bestAcc){
        bestCost=co
        bestAcc=acc
        bestType=ty
      }
    }
  }
  cat("Best model type is:",bestType,"\n")
  cat("Best cost is:",bestCost,"\n")
  cat("Best accuracy is:",bestAcc,"\n")
  
  # Re-train best model with best cost value.
  m=LiblineaR(data=s,target=yTrain,type=bestType,cost=bestCost,bias=1,verbose=FALSE)
  
  # Scale the test data
  s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
  
  # Make prediction
  pr=FALSE
  if(bestType==0 || bestType==7) 
  {pr=TRUE}
  p=predict(m,s2,proba=TRUE,decisionValues=TRUE)
  
  # Display confusion matrix
  res=table(p$predictions,yTest)
  print(res)
  
  return(measurements(p$predictions,yTest))
}

# Get the evaluation metrics
measurementsNew <- data.frame(matrix(0, ncol = 9, nrow = 23))
colnames(measurementsNew) <- c("Acc", "TPR", "TNR", "BA", "PPV", "NPV", "F1Pos", "F1Neg", "F1Macro")
rownames(measurementsNew) <- c("GI", "MPQA", "HL", "NRC-H", "NRC-E", "VADER", "LM", "SM", 
                               "BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                               "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                               "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")
measurementsNew[1,] <- supervised(prepData = prepGI)
measurementsNew[2,] <- supervised(prepData = prepMPQA)
measurementsNew[3,] <- supervised(prepData = prepHL)
measurementsNew[4,] <- supervised(prepData = prepNRC1)
measurementsNew[5,] <- supervised(prepData = prepNRC2)
measurementsNew[6,] <- supervised(prepData = prepVADER)
measurementsNew[7,] <- supervised(prepData = prepFIN)
measurementsNew[8,] <- supervised(prepData = prepSM)

measurementsNew[9,] <- supervised(prepData = prepBTBNew)
measurementsNew[10,] <- supervised(prepData = prepPMINew)
measurementsNew[11,] <- supervised(prepData = prepNPMINew)
measurementsNew[12,] <- supervised(prepData = prepICFNew)
measurementsNew[13,] <- supervised(prepData = prepSNNNew)

measurementsNew[14,] <- supervised(prepData = prepBTBNeg1)
measurementsNew[15,] <- supervised(prepData = prepPMINeg1)
measurementsNew[16,] <- supervised(prepData = prepNPMINeg1)
measurementsNew[17,] <- supervised(prepData = prepICFNeg1)
measurementsNew[18,] <- supervised(prepData = prepSNNNeg1)

measurementsNew[19,] <- supervised(prepData = prepBTBNeg2)
measurementsNew[20,] <- supervised(prepData = prepPMINeg2)
measurementsNew[21,] <- supervised(prepData = prepNPMINeg2)
measurementsNew[22,] <- supervised(prepData = prepICFNeg2)
measurementsNew[23,] <- supervised(prepData = prepSNNNeg2)



 
 
 







