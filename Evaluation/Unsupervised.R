# All libraries
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

# Functions
unsupervised <- function(messages, lexicon, NW = FALSE){
  res <- rep(NA, nrow(messages))
  for (m in 1:nrow(messages)){
    words <- unlist(str_split(messages$Message[m], " "))
    sumSentiment <- 0
    for(w in 1:length(words)){
      
      if(!NW){
        if(startsWith(words[w],"neg_")){
          words[w] <- substring(words[w],5)
          strength <- -1 * (lexicon[words[w]]$Strength)
        }
        else{
          strength <- lexicon[words[w]]$Strength
        }
      }
      else{
        strength <- lexicon[words[w]]$Strength
      }
      
      
      if(!is.na(strength)){
        sumSentiment <- sumSentiment + strength[[1]]
      }
    }
    if (sumSentiment > 0){
      res[m] <- 1
    }
    else if (sumSentiment < 0){
      res[m] <- -1
    }
    else{
      res[m] <- 0
    }
    if (m %% 500 == 0){
      print(m)
    }
  }
  return(res)
}

# Evaluation metrics
measurements <- function(predSentiment, trueSentiment){
  A <- length(trueSentiment)
  U <- length(which(predSentiment == 0))
  C <- A - U
  
  Uncl <- round(U/A*100, digits = 1)
  Acc1 <- round(length(which(trueSentiment == predSentiment))/A*100, digits = 1)
  
  unclassified <- predSentiment != 0
  conf <- confusionMatrix(as.factor(predSentiment[unclassified]), 
                          reference = as.factor(trueSentiment[unclassified]),
                          positive = "1")
  
  Acc2 <- round(conf$overall["Accuracy"][[1]]*100, digits = 1)
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
  
  
  return(c(Acc1, Uncl, Acc2, TPR, TNR, BA, PPV, NPV, F1Pos, F1Neg, F1Macro))
}


# Load lexicons
lexiconBTBNew <- read_csv("lexiconBTBNew.csv", col_types = cols(X1 = col_skip()))
lexiconBTBNeg1 <- read_csv("lexiconBTBNeg1.csv", col_types = cols(X1 = col_skip()))
lexiconBTBNeg2 <- read_csv("lexiconBTBNeg2.csv", col_types = cols(X1 = col_skip()))

lexiconICFNew <- read_csv("lexiconICFNew.csv", col_types = cols(X1 = col_skip()))
lexiconICFNeg1 <- read_csv("lexiconICFNeg1.csv", col_types = cols(X1 = col_skip()))
lexiconICFNeg2 <- read_csv("lexiconICFNeg2.csv", col_types = cols(X1 = col_skip()))

lexiconNPMINew <- read_csv("lexiconNPMINew.csv", col_types = cols(X1 = col_skip()))
lexiconNPMINeg1 <- read_csv("lexiconNPMINeg1.csv", col_types = cols(X1 = col_skip()))
lexiconNPMINeg2 <- read_csv("lexiconNPMINeg2.csv", col_types = cols(X1 = col_skip()))

lexiconPMINew <- read_csv("lexiconPMINew.csv", col_types = cols(X1 = col_skip()))
lexiconPMINeg1 <- read_csv("lexiconPMINeg1.csv", col_types = cols(X1 = col_skip()))
lexiconPMINeg2 <- read_csv("lexiconPMINeg2.csv", col_types = cols(X1 = col_skip()))

lexiconSNNNew <- read_csv("lexiconSNNNew.csv", col_names = FALSE)
lexiconSNNNeg1 <- read_csv("lexiconSNNNeg1.csv", col_names = FALSE)
lexiconSNNNeg2 <- read_csv("lexiconSNNNeg2.csv", col_names = FALSE)[-1,-1]

# Give column names to the SNN lexicons
colnames(lexiconSNNNew) <- colnames(lexiconBTBNew)
colnames(lexiconSNNNeg1) <- colnames(lexiconBTBNeg1)
colnames(lexiconSNNNeg2) <- colnames(lexiconBTBNeg2)


# Create data.tables for faster excess
lexiconBTBNew <- as.data.table(lexiconBTBNew)
setkey(lexiconBTBNew, Word)
lexiconBTBNeg1 <- as.data.table(lexiconBTBNeg1)
setkey(lexiconBTBNeg1, Word)
lexiconBTBNeg2 <- as.data.table(lexiconBTBNeg2)
setkey(lexiconBTBNeg2, Word)

lexiconICFNew <- as.data.table(lexiconICFNew)
setkey(lexiconICFNew, Word)
lexiconICFNeg1 <- as.data.table(lexiconICFNeg1)
setkey(lexiconICFNeg1, Word)
lexiconICFNeg2 <- as.data.table(lexiconICFNeg2)
setkey(lexiconICFNeg2, Word)

lexiconNPMINew <- as.data.table(lexiconNPMINew)
setkey(lexiconNPMINew, Word)
lexiconNPMINeg1 <- as.data.table(lexiconNPMINeg1)
setkey(lexiconNPMINeg1, Word)
lexiconNPMINeg2 <- as.data.table(lexiconNPMINeg2)
setkey(lexiconNPMINeg2, Word)

lexiconPMINew <- as.data.table(lexiconPMINew)
setkey(lexiconPMINew, Word)
lexiconPMINeg1 <- as.data.table(lexiconPMINeg1)
setkey(lexiconPMINeg1, Word)
lexiconPMINeg2 <- as.data.table(lexiconPMINeg2)
setkey(lexiconPMINeg2, Word)

lexiconSNNNew <- as.data.table(lexiconSNNNew)
setkey(lexiconSNNNew, Word)
lexiconSNNNeg1 <- as.data.table(lexiconSNNNeg1)
setkey(lexiconSNNNeg1, Word)
lexiconSNNNeg2 <- as.data.table(lexiconSNNNeg2)
setkey(lexiconSNNNeg2, Word)

lex_GI$Sentiment <- as.numeric(lex_GI$Sentiment)
lex_MPQA$Sentiment <- as.numeric(lex_MPQA$Sentiment)
lex_HL$Sentiment <- as.numeric(lex_HL$Sentiment)
lex_NRC1$Sentiment <- as.numeric(lex_NRC1$Sentiment)
lex_NRC2$Sentiment <- as.numeric(lex_NRC2$Sentiment)
lex_VADER$Sentiment <- as.numeric(lex_VADER$Sentiment)
lex_FIN$Sentiment <- as.numeric(lex_FIN$Sentiment)
lex_SM$Sentiment <- as.numeric(lex_SM$Sentiment)

colnames(lex_GI) <- colnames(lexiconBTBNew)
colnames(lex_MPQA) <- colnames(lexiconBTBNew)
colnames(lex_HL) <- colnames(lexiconBTBNew)
colnames(lex_NRC1) <- colnames(lexiconBTBNew)
colnames(lex_NRC2) <- colnames(lexiconBTBNew)
colnames(lex_VADER) <- colnames(lexiconBTBNew)
colnames(lex_FIN) <- colnames(lexiconBTBNew)
colnames(lex_SM) <- colnames(lexiconBTBNew)

lex_GI <- as.data.table(lex_GI)
setkey(lex_GI, Word)
lex_MPQA <- as.data.table(lex_MPQA)
setkey(lex_MPQA, Word)
lex_HL <- as.data.table(lex_HL)
setkey(lex_HL, Word)
lex_NRC1 <- as.data.table(lex_NRC1)
setkey(lex_NRC1, Word)
lex_NRC2 <- as.data.table(lex_NRC2)
setkey(lex_NRC2, Word)
lex_VADER <- as.data.table(lex_VADER)
setkey(lex_VADER, Word)
lex_FIN <- as.data.table(lex_FIN)
setkey(lex_FIN, Word)
lex_SM <- as.data.table(lex_SM)
setkey(lex_SM, Word)



# Unsupervised results
headMessages <- testStock #microblogs #headlines #stocktwits

for (m in 1:nrow(headMessages)){
  headMessages$Message[m] <- tolower(headMessages$Message[m]) 
  headMessages$Message[m] <- gsub("[[:space:]]{2,}"," ", headMessages$Message[m]) # remove redundant spaces
  headMessages$Message[m] <- gsub("[[:space:]]$","", headMessages$Message[m]) # remove redundant last space
  headMessages$Message[m] <- gsub("^[[:space:]]","", headMessages$Message[m]) # remove redundant first space
}

resultsNew <- data.frame(matrix(0, ncol = 16, nrow = nrow(headMessages)))
colnames(resultsNew)<- c("Sentiment","BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                         "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                         "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")
resultsNew$Sentiment <- headMessages$Sentiment

resultsNew$BTBNew <- unsupervised(headMessages, lexiconBTBNew)
resultsNew$PMINew <- unsupervised(headMessages, lexiconPMINew)
resultsNew$NPMINew <- unsupervised(headMessages, lexiconNPMINew)
resultsNew$ICFNew <- unsupervised(headMessages, lexiconICFNew)
resultsNew$SNNNew <- unsupervised(headMessages, lexiconSNNNew)

resultsNew$BTBNeg1 <- unsupervised(headMessages, lexiconBTBNeg1, NW = TRUE)
resultsNew$PMINeg1 <- unsupervised(headMessages, lexiconPMINeg1, NW = TRUE)
resultsNew$NPMINeg1 <- unsupervised(headMessages, lexiconNPMINeg1, NW = TRUE)
resultsNew$ICFNeg1 <- unsupervised(headMessages, lexiconICFNeg1, NW = TRUE)
resultsNew$SNNNeg1 <- unsupervised(headMessages, lexiconSNNNeg1, NW = TRUE)

resultsNew$BTBNeg2 <- unsupervised(headMessages, lexiconBTBNeg2)
resultsNew$PMINeg2 <- unsupervised(headMessages, lexiconPMINeg2)
resultsNew$NPMINeg2 <- unsupervised(headMessages, lexiconNPMINeg2)
resultsNew$ICFNeg2 <- unsupervised(headMessages, lexiconICFNeg2)
resultsNew$SNNNeg2 <- unsupervised(headMessages, lexiconSNNNeg2)


measurementsNew <- data.frame(matrix(0, ncol = 11, nrow = 15))
colnames(measurementsNew) <- c("Acc1", "Uncl", "Acc2", "TPR", "TNR", "BA", "PPV", "NPV", "F1Pos", "F1Neg", "F1Macro")
rownames(measurementsNew) <- c("BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                               "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                               "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")

measurementsNew[1,] <- measurements(resultsNew$BTBNew, resultsNew$Sentiment)
measurementsNew[2,] <- measurements(resultsNew$PMINew, resultsNew$Sentiment)
measurementsNew[3,] <- measurements(resultsNew$NPMINew, resultsNew$Sentiment)
measurementsNew[4,] <- measurements(resultsNew$ICFNew, resultsNew$Sentiment)
measurementsNew[5,] <- measurements(resultsNew$SNNNew, resultsNew$Sentiment)

measurementsNew[6,] <- measurements(resultsNew$BTBNeg1, resultsNew$Sentiment)
measurementsNew[7,] <- measurements(resultsNew$PMINeg1, resultsNew$Sentiment)
measurementsNew[8,] <- measurements(resultsNew$NPMINeg1, resultsNew$Sentiment)
measurementsNew[9,] <- measurements(resultsNew$ICFNeg1, resultsNew$Sentiment)
measurementsNew[10,] <- measurements(resultsNew$SNNNeg1, resultsNew$Sentiment)

measurementsNew[11,] <- measurements(resultsNew$BTBNeg2, resultsNew$Sentiment)
measurementsNew[12,] <- measurements(resultsNew$PMINeg2, resultsNew$Sentiment)
measurementsNew[13,] <- measurements(resultsNew$NPMINeg2, resultsNew$Sentiment)
measurementsNew[14,] <- measurements(resultsNew$ICFNeg2, resultsNew$Sentiment)
measurementsNew[15,] <- measurements(resultsNew$SNNNeg2, resultsNew$Sentiment)


resultsNew <- data.frame(matrix(0, ncol = 9, nrow = nrow(headMessages)))
colnames(resultsNew)<- c("Sentiment","GI", "MPQA", "HL", "NRC1", "NRC2", "VADER", "LM", "SM")
resultsNew$Sentiment <- headMessages$Sentiment

resultsNew$GI <- unsupervised(headMessages, lex_GI)
resultsNew$MPQA <- unsupervised(headMessages, lex_MPQA)
resultsNew$HL <- unsupervised(headMessages, lex_HL)
resultsNew$NRC1 <- unsupervised(headMessages, lex_NRC1)
resultsNew$NRC2 <- unsupervised(headMessages, lex_NRC2)
resultsNew$VADER <- unsupervised(headMessages, lex_VADER)
resultsNew$LM <- unsupervised(headMessages, lex_FIN)
resultsNew$SM <- unsupervised(headMessages, lex_SM)

measurementsNew <- data.frame(matrix(0, ncol = 11, nrow = 8))
colnames(measurementsNew) <- c("Acc1", "Uncl", "Acc2", "TPR", "TNR", "BA", "PPV", "NPV", "F1Pos", "F1Neg", "F1Macro")
rownames(measurementsNew) <- c("GI", "MPQA", "HL", "NRC-H", "NRC-E", "VADER", "LM", "SM")
measurementsNew[1,] <- measurements(resultsNew$GI, resultsNew$Sentiment)
measurementsNew[2,] <- measurements(resultsNew$MPQA, resultsNew$Sentiment)
measurementsNew[3,] <- measurements(resultsNew$HL, resultsNew$Sentiment)
measurementsNew[4,] <- measurements(resultsNew$NRC1, resultsNew$Sentiment)
measurementsNew[5,] <- measurements(resultsNew$NRC2, resultsNew$Sentiment)
measurementsNew[6,] <- measurements(resultsNew$VADER, resultsNew$Sentiment)
measurementsNew[7,] <- measurements(resultsNew$LM, resultsNew$Sentiment)
measurementsNew[8,] <- measurements(resultsNew$SM, resultsNew$Sentiment)





