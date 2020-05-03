# Negation words
negationWords <- c("no", "not", "never", "less", "without", "barely", "hardly", "rarely",
                   "scarcely", "seldom", "lack","lacking","lacks","neither","nor", 
                   "rather","nobody","none","nothing","nowhere","no longer","no more",
                   "no way","no where","by no means", "at no time","ain\'t","aren\'t",
                   "can\'t", "cannot", "couldn\'t","didn\'t","doesn\'t","don\'t","hasn\'t",
                   "haven\'t","isn\'t","mightn\'t","mustn\'t","needn\'t","shan\'t","shouldn\'t",
                   "wasn\'t","weren\'t","won\'t","wouldn\'t", "aint","arent",
                   "cant",  "couldnt","didnt","doesnt","dont","hasnt",
                   "havent","isnt","mightnt","mustnt","neednt","shant","shouldnt",
                   "wasnt","werent","wont","wouldnt")

headMessages <- microblogs #microblogs #headlines #stocktwits
dfclean <- data.frame(matrix(ncol = 3, nrow = nrow(headMessages)))
colnames(dfclean)<- c("Message", "Sentiment", "NegatedMessage")

# Small data cleaning
for (m in 1:nrow(headMessages)){
  headMessages$Message[m] <- gsub("'s", " s", headMessages$Message[m]) 
  headMessages$Message[m] <- gsub("'", "", headMessages$Message[m]) 
  headMessages$Message[m] <- gsub("[[:punct:]]", " ", headMessages$Message[m]) # remove all punctuation
  headMessages$Message[m] <- tolower(headMessages$Message[m]) 
  headMessages$Message[m] <- gsub("[[:space:]]{2,}"," ", headMessages$Message[m]) # remove redundant spaces
  headMessages$Message[m] <- gsub("[[:space:]]$","", headMessages$Message[m]) # remove redundant last space
  headMessages$Message[m] <- gsub("^[[:space:]]","", headMessages$Message[m]) # remove redundant first space
}

dfclean$Message <- headMessages$Message
dfclean$Sentiment <- headMessages$Sentiment

#Check for negation
for (i in 1:nrow(dfclean)){
  wordsMessage <- unlist(str_split(dfclean$Message[i], " ")) # get words from message
  wordsNegatedMessage <- wordsMessage
  negationCue <- grep(paste0("\\b(", paste(negationWords, collapse="|"), ")\\b"), wordsMessage, value = FALSE) # check for negation cues
  negationScope <- ""
  if (length(negationCue) > 0){ # in case of negation cues
    for (j in 1:length(negationCue)){
      if ((negationCue[j] + 1)<= length(wordsMessage)){ # to prevent NA
          wordsNegatedMessage[negationCue[j]+1] <- paste(c("NEG_", wordsNegatedMessage[negationCue[j]+1]), collapse = "") # replace first word in scope with NEG_word
        
      }
      if ((negationCue[j] + 2)<= length(wordsMessage)){ # to prevent NAs
          wordsNegatedMessage[negationCue[j]+2] <- paste(c("NEG_", wordsNegatedMessage[negationCue[j]+2]), collapse = "") # replace second word in scope with NEG_word
      }
    }
  }
  dfclean$NegatedMessage[i] <- paste(wordsNegatedMessage, collapse = " ")
  
  dfclean$NegatedMessage[i] <- gsub("[^[:alnum:][:space:]_]", " ", dfclean$NegatedMessage[i]) # remove punctuation except _
  dfclean$NegatedMessage[i] <- gsub("[[:space:]]{2,}"," ", dfclean$NegatedMessage[i]) # remove redundant spaces
  dfclean$NegatedMessage[i] <- gsub("[[:space:]]$","", dfclean$NegatedMessage[i]) # remove redundant last space
  dfclean$NegatedMessage[i] <- gsub("^[[:space:]]","", dfclean$NegatedMessage[i]) # remove redundant first space
}

microblogs2 <- dfclean