# Load libraries
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


# Get the features that are needed for the supervised Logistic
supervisedPrep <- function(messages, lexicon, NW = FALSE){
  res <- data.frame(matrix(0, ncol = 6, nrow = nrow(messages)))
  colnames(res) <- c("NumWords", "SumTotal", "Large", "SumPos", "SumNeg", "Last")
  for (m in 1:nrow(messages)){
    words <- unlist(str_split(messages$Message[m], " "))
    sumSentiment <- 0
    sumPos <- 0
    sumNeg <- 0
    largest <- 0
    counter <- 0
    for(w in 1:length(words)){
      if(!NW){
        if(startsWith(words[w],"neg_")){
          words[w] <- substring(words[w],5)
          strength <- -1 * (lexicon[words[w]]$Strength[1])
        }
        else{
          strength <- lexicon[words[w]]$Strength[1]
        }
      }
      else{
        strength <- lexicon[words[w]]$Strength[1]
      }
      
      if(!is.na(strength)){
        sumSentiment <- sumSentiment + strength
        if (strength != 0){
          counter <- counter + 1
        }
        if (strength > 0)
        {
          sumPos <- sumPos + strength
        }
        else if (strength < 0){
          sumNeg <- sumNeg - strength
        }
        if (abs(strength) > largest){
          largest <- abs(strength)
        }
      }
      if(w == length(words)){
        if(!is.na(strength)){
          res[m,6] <- strength
        }
        else{
          res[m,6] <- 0
        }
      }
    }
    
    res[m,1] <- counter
    res[m,2] <- sumSentiment
    res[m,3] <- largest
    res[m,4] <- sumPos
    res[m,5] <- sumNeg
    
    if (m %% 500 == 0){
      print(m)
    }
  }
  return(res)
}

headMessages <- testStock #microblogs #headlines #stocktwits

for (m in 1:nrow(headMessages)){
  headMessages$Message[m] <- gsub("[[:space:]]{2,}"," ", headMessages$Message[m]) # remove redundant spaces
  headMessages$Message[m] <- gsub("[[:space:]]$","", headMessages$Message[m]) # remove redundant last space
  headMessages$Message[m] <- gsub("^[[:space:]]","", headMessages$Message[m]) # remove redundant first space
}

prepGI <- supervisedPrep(headMessages, lex_GI)
prepMPQA <- supervisedPrep(headMessages, lex_MPQA)
prepHL <- supervisedPrep(headMessages, lex_HL)
prepNRC1 <- supervisedPrep(headMessages, lex_NRC1)
prepNRC2 <- supervisedPrep(headMessages, lex_NRC2)
prepVADER <- supervisedPrep(headMessages, lex_VADER)
prepFIN <- supervisedPrep(headMessages, lex_FIN)
prepSM <- supervisedPrep(headMessages, lex_SM)

prepBTBNew <- supervisedPrep(headMessages, lexiconBTBNew)
prepBTBNeg2 <- supervisedPrep(headMessages, lexiconBTBNeg2)
prepPMINew <- supervisedPrep(headMessages, lexiconPMINew)
prepPMINeg2 <- supervisedPrep(headMessages, lexiconPMINeg2)
prepNPMINew <- supervisedPrep(headMessages, lexiconNPMINew)
prepNPMINeg2 <- supervisedPrep(headMessages, lexiconNPMINeg2)
prepICFNew <- supervisedPrep(headMessages, lexiconICFNew)
prepICFNeg2 <- supervisedPrep(headMessages, lexiconICFNeg2)
prepSNNNew <- supervisedPrep(headMessages, lexiconSNNNew)
prepSNNNeg2 <- supervisedPrep(headMessages, lexiconSNNNeg2)

prepBTBNeg1 <- supervisedPrep(headMessages, lexiconBTBNeg, NW = TRUE)
prepPMINeg1 <- supervisedPrep(headMessages, lexiconPMINeg1, NW = TRUE)
prepNPMINeg1 <- supervisedPrep(headMessages, lexiconNPMINeg1, NW = TRUE)
prepICFNeg1 <- supervisedPrep(headMessages, lexiconICFNeg1, NW = TRUE)
prepSNNNeg1 <- supervisedPrep(headMessages, lexiconSNNNeg1, NW = TRUE)

