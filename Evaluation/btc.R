# Function lexicon in practice

practice <- function(messages, lexicon, NW = FALSE){
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
    res[m] <- sumSentiment
    if (m %% 25 == 0){
      print(m/25)
    }
  }
  return(res)
}

# Unsupervised in practice
headMessages <- btc

# Sum approach
resultsNew <- data.frame(matrix(0, ncol = 25, nrow = nrow(headMessages)))
colnames(resultsNew)<- c("Time", "Day","GI", "MPQA", "HL", "NRC1", "NRC2", "VADER", "LM", "SM", 
                         "BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                         "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                         "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")
resultsNew$Day <- headMessages$Time
resultsNew$Day <- headMessages$Day

resultsNew$GI <- practice(headMessages, lex_GI)
resultsNew$MPQA <- practice(headMessages, lex_MPQA)
resultsNew$HL <- practice(headMessages, lex_HL)
resultsNew$NRC1 <- practice(headMessages, lex_NRC1)
resultsNew$NRC2 <- practice(headMessages, lex_NRC2)
resultsNew$VADER <- practice(headMessages, lex_VADER)
resultsNew$LM <- practice(headMessages, lex_FIN)
resultsNew$SM <- practice(headMessages, lex_SM)

resultsNew$BTBNew <- practice(headMessages, lexiconBTBNew)
resultsNew$PMINew <- practice(headMessages, lexiconPMINew)
resultsNew$NPMINew <- practice(headMessages, lexiconNPMINew)
resultsNew$ICFNew <- practice(headMessages, lexiconICFNew)
resultsNew$SNNNew <- practice(headMessages, lexiconSNNNew)

resultsNew$BTBNeg1 <- practice(headMessages, lexiconBTBNeg1, NW = TRUE)
resultsNew$PMINeg1<- practice(headMessages, lexiconPMINeg1, NW = TRUE)
resultsNew$NPMINeg1 <- practice(headMessages, lexiconNPMINeg1, NW = TRUE)
resultsNew$ICFNeg1 <- practice(headMessages, lexiconICFNeg1, NW = TRUE)
resultsNew$SNNNeg1 <- practice(headMessages, lexiconSNNNeg1, NW = TRUE)

resultsNew$BTBNeg2 <- practice(headMessages, lexiconBTBNeg2)
resultsNew$PMINeg2 <- practice(headMessages, lexiconPMINeg2)
resultsNew$NPMINeg2<- practice(headMessages, lexiconNPMINeg2)
resultsNew$ICFNeg2 <- practice(headMessages, lexiconICFNeg2)
resultsNew$SNNNeg2 <- practice(headMessages, lexiconSNNNeg2)

# Majority vote approach
resultsNew2 <- resultsNew
for (i in 3:ncol(resultsNew)){
  resultsNew2[,i] <- ifelse(resultsNew2[,i] > 0, 1, resultsNew2[,i])
  resultsNew2[,i] <- ifelse(resultsNew2[,i] < 0, -1, resultsNew2[,i])
}

results <- data.frame(matrix(0, nrow = nrow(delta2), ncol = 25))
colnames(results)<- c("Time", "Day","GI", "MPQA", "HL", "NRC1", "NRC2", "VADER", "LM", "SM", 
                         "BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                         "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                         "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")

# Construct Sum approach
results$Day <- 1:nrow(delta2)

for(k in 3:ncol(results)){
  for (i in 1:nrow(delta2)){
    for(j in 1:25){
      results[i,k] <- results[i,k] + resultsNew[(((i-1)*25)+j),k]
    }
  }
}

for (i in 3:ncol(results)){
  results[,i] <- ifelse(results[,i] > 0, 1, results[,i])
  results[,i] <- ifelse(results[,i] < 0, -1, results[,i])
}

results2 <- data.frame(matrix(0, nrow = nrow(delta2), ncol = 25))
colnames(results2)<- c("Time", "Day","GI", "MPQA", "HL", "NRC1", "NRC2", "VADER", "LM", "SM", 
                      "BTBNew", "PMINew", "NPMINew", "ICFNew", "SNNNew", 
                      "BTBNeg1", "PMINeg1", "NPMINeg1", "ICFNeg1", "SNNNeg1",
                      "BTBNeg2", "PMINeg2", "NPMINeg2", "ICFNeg2", "SNNNeg2")

results2$Day <- 1:nrow(delta2)

# Construct Majority vote approach
for(k in 3:ncol(results2)){
  for (i in 1:nrow(delta2)){
    for(j in 1:25){
      results2[i,k] <- results2[i,k] + resultsNew2[(((i-1)*25)+j),k]
    }
  }
}

for (i in 3:ncol(results2)){
  results2[,i] <- ifelse(results2[,i] > 0, 1, results2[,i])
  results2[,i] <- ifelse(results2[,i] < 0, -1, results2[,i])
}

# print results for specific lexicons
# Sum approach
for(i in c(6,10,16,17,22,18,23)){
  cat("#######################","\n")
  cat(names(results)[i], "\n")
  correlation <- cor.test(delta2$DELTA, results[,i])
  cat(correlation$estimate[[1]],"\n")
  cat(correlation[[3]],"\n")
  cat("#######################","\n")
}

# Majority vote approach
for(i in c(6,10,16,17,22,18,23)){
  cat("#######################","\n")
  cat(names(results)[i], "\n")
  correlation <- cor.test(delta2$DELTA, results2[,i])
  cat(correlation$estimate[[1]],"\n")
  cat(correlation[[3]],"\n")
  cat("#######################","\n")
}v