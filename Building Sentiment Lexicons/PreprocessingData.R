# Load libraries
library(jsonlite)
library(textclean)
library(textmining)
library(htm2txt)
library(tidytext)
library(dplyr)
library(sjmisc)
library(stringr)

####################
#- Initialization -#
####################

# Set seed

set.seed(2020)

# Define sentinent orientation and sentiment strength fucntions
PMI <- function(Mwc, Mw, M, Mc){
  if (Mwc == 0){
    return(0)
  }
  else{
    res <- (log2(Mwc) + log2(M) - log2(Mw) - log2(Mc))
    return(res)
  }
}

PMIc <- function(Mwc, Mw, M, Mc){
  if (Mwc == 0){
    return(0)
  }
  else{
    res <- (log2(Mwc) + log2(M) - log2(Mw) - log2(Mc))/(-log2(Mwc)+log2(M))
    return(res)
  }
}

SSBTB <- function(nw, nwpos, nwneg){
  res <- (nwpos - nwneg)/nw
  return(res)
}

SSPMI <- function(Mwpos, Mwneg, Mw, M, Mc){
  res <- (Mwpos/Mw) * PMI(Mwpos, Mw, M, Mc) - (Mwneg/Mw) * PMI(Mwneg, Mw, M, Mc)
  return(res)
}

SSNPMI <- function(Mwpos, Mwneg, Mw, M, Mc){
  res <- (Mwpos/Mw) * PMIc(Mwpos, Mw, M, Mc) - (Mwneg/Mw) * PMIc(Mwneg, Mw, M, Mc)
  return(res)
}


SSicfbased <- function(nwpos, nwneg, Mwpos, Mwneg){
  if (Mwpos > 0){
    Npos <- 1
  }
  else{
    Npos <- 0
  }
  if(Mwneg > 0){
    Nneg <- 1
  }
  else{
    Nneg <- 1
  }
  logPos <- log2(2 + (Mwpos/max(1,Mwneg))  *  (2/(1+Npos+Nneg)))
  logNeg<- log2(2 + (Mwneg/max(1,Mwpos))  *  (2/(1+Npos+Nneg)))
  score <- (nwpos*logPos - nwneg*logNeg)/(nwpos*logPos + nwneg*logNeg)
  return(score)
}

# Define the dataframe
MaxMes <- 75000 # Maximum number of messages to read in

dfraw <- data.frame(matrix(ncol = 2, nrow = MaxMes))
colnames(dfraw)<- c("Message", "Sentiment")

# Define stopwords and negation words
stopwords <- c("x", stopwords('english')) # Add x to prevent $btc.x
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

# Counting variable
j = 1;
bearish.counter <- 0

##################
#- Reading Data -#
##################

# Read data
inputFile <- "stocktwits_messages_2019_12"
con  <- file(inputFile, open = "r")
while (length(oneLine <- readLines(con, n = 1, encoding="UTF-8")) > 0) {
  message_json <- fromJSON(oneLine)
  
  # only messages that do not start with @, have a sentiment + sentiment score != 0, and are of type create
  if(!message_json$action == "destroy"){
    if(!is.null(message_json$data$entities$sentiment)){
      if(!is.null(message_json$data$sentiment)){
        if(!message_json$data$sentiment$sentiment_score == 0){
          if(!startsWith(message_json$data$body, "@")){
            try(dfraw$Message[j] <- tolower(htm2txt(message_json$data$body)))
            dfraw$Sentiment[j] <- tolower(message_json$data$entities$sentiment$basic[1])
            if (dfraw$Sentiment[j] == "bearish"){
              bearish.counter <- bearish.counter + 1
            }
            j = j + 1
          }
        }
      }
    }
  }
  
  # stopping condition
  if(bearish.counter == 10500){
    break
  }
} 
close(con)

# Max 10,500 observations each (bearish and bullish), because we need 10,000 each and some will be lost
dfraw <- na.omit(dfraw)   
dfraw <- dfraw %>% group_by(Sentiment) %>% sample_n(size = 10500)


###################
#- Cleaning Data -#
###################

# Create df for cleaned messages
dfclean <- data.frame(matrix(ncol = 4, nrow = nrow(dfraw)))
colnames(dfclean)<- c("Message", "Sentiment", "Negation", "NegatedMessage")
dfclean$Sentiment <- dfraw$Sentiment

for (i in 1:nrow(dfraw)){
  dfclean$Message[i] <- gsub("(http)(s)?(://)\\S+\\s?", "", dfraw$Message[i]) # remove url
  dfclean$Message[i] <- gsub("[$][[:alpha:]]+\\s?","", dfclean$Message[i]) # remove cashtag
  dfclean$Message[i] <- gsub("(@)[[:alnum:]]+\\s?","", dfclean$Message[i]) # remove @username
  dfclean$Message[i] <- gsub("(#)[[:alnum:]]+\\s?","", dfclean$Message[i]) # remove hastag
  dfclean$Message[i] <- gsub("(’)", "'", dfclean$Message[i])  # replace ’ by '
  dfclean$Message[i] <- gsub('\\p{So}|\\p{Cn}', "", dfclean$Message[i], perl = TRUE) # remove emoticons, emojis
  dfclean$Message[i] <- iconv(dfclean$Message[i], "latin1", "ASCII", sub="") # remove other symbols
  
  dfclean$Message[i] <- gsub("[+][[:space:]]*[[:digit:]]+[.]*[[:digit:]]*"," posnum", dfclean$Message[i]) # replace posnum
  dfclean$Message[i] <- gsub("[-][[:space:]]*[[:digit:]]+[.]*[[:digit:]]*"," negnum ", dfclean$Message[i]) # replace negnum
  dfclean$Message[i] <- gsub("[+][[:space:]]*[[:digit:]]+[.]*[[:digit:]]*[[:space:]]*(%)"," posperc", dfclean$Message[i]) # replace posperc
  dfclean$Message[i] <- gsub("[-][[:space:]]*[[:digit:]]+[.]*[[:digit:]]*[[:space:]]*(%)"," negperc", dfclean$Message[i]) # replace negperc
  dfclean$Message[i] <- gsub("[[:digit:]]","", dfclean$Message[i]) # remove digits
  
  dfclean$Message[i] <- gsub("[^[:alnum:][:space:]']", " ", dfclean$Message[i]) #replace punctuation by space except ' 
  dfclean$Message[i] <- gsub("[[:space:]]{2,}"," ", dfclean$Message[i]) # remove redundant spaces
  dfclean$Message[i] <- gsub("[[:space:]]$","", dfclean$Message[i]) # remove redundant last space
  dfclean$Message[i] <- gsub("^[[:space:]]","", dfclean$Message[i]) # remove redundant first space
  
  #Check for negation
  wordsMessage <- unlist(str_split(dfclean$Message[i], " ")) # get words from message
  wordsNegatedMessage <- wordsMessage
  negationCue <- grep(paste0("\\b(", paste(negationWords, collapse="|"), ")\\b"), wordsMessage, value = FALSE) # check for negation cues
  negationScope <- ""
  if (length(negationCue) > 0){ # in case of negation cues
    for (j in 1:length(negationCue)){
      if ((negationCue[j] + 1)<= length(wordsMessage)){ # to prevent NAs
        if (!wordsMessage[(negationCue[j]+1)] %in% stopwords){ # to prevent stopwords 
          negationScope <- c(negationScope, wordsMessage[(negationCue[j]+1)]) # get first word in scope
          wordsNegatedMessage[negationCue[j]+1] <- paste(c("NEG_", wordsNegatedMessage[negationCue[j]+1]), collapse = "") # replace first word in scope with NEG_word
        }
      }
      if ((negationCue[j] + 2)<= length(wordsMessage)){ # to prevent NAs
        if (!wordsMessage[(negationCue[j]+2)] %in% stopwords){ # to prevent stopwords
          negationScope <- c(negationScope, wordsMessage[(negationCue[j]+2)]) # get second word in scope
          wordsNegatedMessage[negationCue[j]+2] <- paste(c("NEG_", wordsNegatedMessage[negationCue[j]+2]), collapse = "") # replace second word in scope with NEG_word
        }
      }
    }
    negationScope <- negationScope[-1] # remove first empty entry
  }
  dfclean$Negation[i] <- paste(negationScope, collapse = " ")
  dfclean$Negation[i] <- removeWords(dfclean$Negation[i], stopwords) # remove stopwords from negation scope
  dfclean$NegatedMessage[i] <- paste(wordsNegatedMessage, collapse = " ")
  
  # Clean negation part
  dfclean$Negation[i] <- gsub("[[:punct:]]", "", dfclean$Negation[i]) # remove all punctuation
  dfclean$Negation[i] <- gsub("[[:space:]]{2,}"," ", dfclean$Negation[i]) # remove redundant spaces
  dfclean$Negation[i] <- gsub("[[:space:]]$","", dfclean$Negation[i]) # remove redundant last space
  dfclean$Negation[i] <- gsub("^[[:space:]]","", dfclean$Negation[i]) # remove redundant first space
  
  # Clean message part
  dfclean$Message[i] <- removeWords(dfclean$Message[i], stopwords) # remove stopwords
  dfclean$Message[i] <- removeWords(dfclean$Message[i], negationWords) # remove negation cues
  dfclean$Message[i] <- gsub("[[:punct:]]", "", dfclean$Message[i]) # remove all punctuation
  dfclean$Message[i] <- gsub("[[:space:]]{2,}"," ", dfclean$Message[i]) # remove redundant spaces
  dfclean$Message[i] <- gsub("[[:space:]]$","", dfclean$Message[i]) # remove redundant last space
  dfclean$Message[i] <- gsub("^[[:space:]]","", dfclean$Message[i]) # remove redundant first space
  
  # Clean negated message part
  dfclean$NegatedMessage[i] <- removeWords(dfclean$NegatedMessage[i], stopwords) # remove stopwords
  dfclean$NegatedMessage[i] <- removeWords(dfclean$NegatedMessage[i], negationWords) # remove negation cues
  dfclean$NegatedMessage[i] <- gsub("[^[:alnum:][:space:]_]", " ", dfclean$NegatedMessage[i]) # remove punctuation except _
  dfclean$NegatedMessage[i] <- gsub("[[:space:]]{2,}"," ", dfclean$NegatedMessage[i]) # remove redundant spaces
  dfclean$NegatedMessage[i] <- gsub("[[:space:]]$","", dfclean$NegatedMessage[i]) # remove redundant last space
  dfclean$NegatedMessage[i] <- gsub("^[[:space:]]","", dfclean$NegatedMessage[i]) # remove redundant first space
  
  
}

dfclean <- subset(dfclean, nchar(dfclean$Message) > 0) #remove empty messages
dfclean <- dfclean %>% group_by(Sentiment) %>% sample_n(size = 10000)

save(dfclean, file = "dfcleanDec2019.RData")
write.csv(dfclean, file = "dfcleanDec2019.csv")



dfclean <- rbind(dfcleanApr2019, dfcleanAug2019, dfcleanDec2019, dfcleanFeb2019, dfcleanJan2019, dfcleanJul2019, dfcleanJun2019, dfcleanMar2019, dfcleanMay2019, dfcleanNov2019, dfcleanOct2019, dfcleanSep2019)
dfclean <- dfclean[,-1]

bull <- dfclean[dfclean$Sentiment == "bullish",]
bear <- dfclean[dfclean$Sentiment == "bearish",]

sampleBull <- sample(nrow(bull), 100000)
bullTrain <- bull[sampleBull,]
bullTest <- bull[-sampleBull,]

sampleBear <- sample(nrow(bear), 100000)
bearTrain <- bear[sampleBear,]
bearTest <- bear[-sampleBear,]

train <- rbind(bullTrain, bearTrain)
test <- rbind(bullTest, bearTest)


#################
#- No negation -#
#################

M <- nrow(train) # Number of messages, M (for now maximum number of messages that has been)
Mc <- M/2 # Number of messages of each sentiment class c

# The tdm matrix is too large to handle at once. Switch to second script for partial tdm matrices!!

# # Create TDM matrix
#messages <- data.frame(doc_id=seq(1:nrow(train)),text=train$Message)
#corpus <- VCorpus(DataframeSource(messages))
#tdm<-TermDocumentMatrix(corpus,control=list(weighting=weightTf))
# tdm.messages <- as.matrix(tdm)
# 
# # Minimal 5 occurrences 
# nw <- rowSums(tdm.messages)
# min.5.occur <- (nw > 4)
# tdm.messages.new <- tdm.messages[min.5.occur,]
# 
# # Get nw and Mw
# nw.all <- rowSums(tdm.messages.new)
# mw.all <- rowSums(tdm.messages.new > 0)
# 
# # Split bullish and bearish
# bullish <- (train$Sentiment == 'bullish')
# bearish <- (train$Sentiment == 'bearish')
# tdm.messages.neg <- tdm.messages.new[,bearish]
# tdm.messages.pos <- tdm.messages.new[,bullish]
# 
# # Get nwc and Mwc
# nw.neg <- rowSums(tdm.messages.neg)
# nw.pos <- rowSums(tdm.messages.pos)
# mw.neg <- rowSums(tdm.messages.neg > 0)
# mw.pos <- rowSums(tdm.messages.pos > 0)
# 
# # Create overview with all the needed measures
# overview <- data.frame(matrix(ncol = 7, nrow = sum(min.5.occur)))
# colnames(overview)<- c("Word","Mw", "Mwpos", "Mwneg", "nw", "nwpos", "nwneg")
# overview$Word <- rownames(tdm.messages.new)
# overview$Mw <- mw.all
# overview$Mwpos <- mw.pos
# overview$Mwneg <- mw.neg
# overview$nw <- nw.all
# overview$nwpos <- nw.pos
# overview$nwneg <- nw.neg

overview <- combined.overview # combined.overview from TDMExtra.R
overview <- overview[overview$nw > 4,]

# Create lexicons
lexiconBTB <- data.frame(matrix(ncol = 2, nrow = nrow(overview)))
colnames(lexiconBTB)<- c("Word","Strength")
lexiconBTB$Word <- overview$Word
lexiconPMI <- data.frame(matrix(ncol = 2, nrow = nrow(overview)))
colnames(lexiconPMI)<- c("Word","Strength")
lexiconPMI$Word <- overview$Word
lexiconNPMI <- data.frame(matrix(ncol = 2, nrow = nrow(overview)))
colnames(lexiconNPMI)<- c("Word","Strength")
lexiconNPMI$Word <- overview$Word
lexiconICF <- data.frame(matrix(ncol = 2, nrow = nrow(overview)))
colnames(lexiconICF)<- c("Word","Strength")
lexiconICF$Word <- overview$Word 
for(k in 1:nrow(overview)){
  lexiconBTB$Strength[k] <- SSBTB(overview$nw[k], overview$nwpos[k], overview$nwneg[k])
  lexiconPMI$Strength[k] <- max(min(SSPMI(overview$Mwpos[k], overview$Mwneg[k], overview$Mw[k], M, Mc),1),-1)
  lexiconNPMI$Strength[k] <- SSNPMI(overview$Mwpos[k], overview$Mwneg[k], overview$Mw[k], M, Mc)
  lexiconICF$Strength[k] <- SSicfbased(overview$nwpos[k], overview$nwneg[k], overview$Mwpos[k], overview$Mwneg[k])
}

################
#- Negation 2 -#
################

subsetNegation <- subset(train$Negation, nchar(train$Negation) > 0)

# Create TDM matrix
neg2.messages <- data.frame(doc_id=seq(1:length(subsetNegation)),text=subsetNegation)
neg2.corpus <- VCorpus(DataframeSource(neg2.messages))
neg2.tdm<-TermDocumentMatrix(neg2.corpus,control=list(weighting=weightTf))
neg2.tdm.messages <- as.matrix(neg2.tdm)

# Get nw and Mw
neg2.nw.all <- rowSums(neg2.tdm.messages)
neg2.mw.all <- rowSums(neg2.tdm.messages > 0)

# Split bullish and bearish
neg2.sentiment <- na.omit(train$Sentiment[nchar(train$Negation) > 0])

neg2.bullish <- (neg2.sentiment == 'bullish')
neg2.bearish <- (neg2.sentiment == 'bearish')
neg2.tdm.messages.neg <- neg2.tdm.messages[,neg2.bearish]
neg2.tdm.messages.pos <- neg2.tdm.messages[,neg2.bullish]

# Get nwc and Mwc
neg2.nw.neg <- rowSums(neg2.tdm.messages.neg)
neg2.nw.pos <- rowSums(neg2.tdm.messages.pos)
neg2.mw.neg <- rowSums(neg2.tdm.messages.neg > 0)
neg2.mw.pos <- rowSums(neg2.tdm.messages.pos > 0)

# Create temp overview with all the needed measures of the words that are negated
temp.overview <- data.frame(matrix(ncol = 7, nrow = nrow(neg2.tdm.messages)))
colnames(temp.overview)<- c("Word","Mw2", "Mwpos2", "Mwneg2", "nw2", "nwpos2", "nwneg2")
temp.overview$Word <- rownames(neg2.tdm.messages)
temp.overview$Mw2 <- neg2.mw.all
temp.overview$Mwpos2 <- neg2.mw.pos
temp.overview$Mwneg2 <- neg2.mw.neg
temp.overview$nw2 <- neg2.nw.all
temp.overview$nwpos2 <- neg2.nw.pos
temp.overview$nwneg2 <- neg2.nw.neg

# Join the dataframes to adjust the original overview for the negation
neg2.overview <- merge(overview,temp.overview,by="Word", all = TRUE)
neg2.overview <- neg2.overview[!is.na(neg2.overview$Mw),]

for (p in 1:nrow(neg2.overview)){
  if (!is.na(neg2.overview$Mw2[p])){
    neg2.overview$Mwpos[p] <- neg2.overview$Mwpos[p] - neg2.overview$Mwpos2[p] + neg2.overview$Mwneg2[p]
    neg2.overview$Mwneg[p] <- neg2.overview$Mwneg[p] + neg2.overview$Mwpos2[p] - neg2.overview$Mwneg2[p]
    neg2.overview$nwpos[p] <- neg2.overview$nwpos[p] - neg2.overview$nwpos2[p] + neg2.overview$nwneg2[p]
    neg2.overview$nwneg[p] <- neg2.overview$nwneg[p] + neg2.overview$nwpos2[p] - neg2.overview$nwneg2[p]
  }
}

neg2.overview <- neg2.overview[,c(1:7)]

# Create lexicons
neg2.lexiconBTB <- data.frame(matrix(ncol = 2, nrow = nrow(neg2.overview)))
colnames(neg2.lexiconBTB)<- c("Word","Strength")
neg2.lexiconBTB$Word <- neg2.overview$Word
neg2.lexiconPMI <- data.frame(matrix(ncol = 2, nrow = nrow(neg2.overview)))
colnames(neg2.lexiconPMI)<- c("Word","Strength")
neg2.lexiconPMI$Word <- neg2.overview$Word
neg2.lexiconNPMI <- data.frame(matrix(ncol = 2, nrow = nrow(neg2.overview)))
colnames(neg2.lexiconNPMI)<- c("Word","Strength")
neg2.lexiconNPMI$Word <- neg2.overview$Word
neg2.lexiconICF <- data.frame(matrix(ncol = 2, nrow = nrow(neg2.overview)))
colnames(neg2.lexiconICF)<- c("Word","Strength")
neg2.lexiconICF$Word <- neg2.overview$Word 
for(k in 1:nrow(overview)){
  neg2.lexiconBTB$Strength[k] <- SSBTB(neg2.overview$nw[k], neg2.overview$nwpos[k], neg2.overview$nwneg[k])
  neg2.lexiconPMI$Strength[k] <- max(min(SSPMI(neg2.overview$Mwpos[k], neg2.overview$Mwneg[k], neg2.overview$Mw[k], M, Mc),1),-1)
  neg2.lexiconNPMI$Strength[k] <- SSNPMI(neg2.overview$Mwpos[k], neg2.overview$Mwneg[k], neg2.overview$Mw[k], M, Mc)
  neg2.lexiconICF$Strength[k] <- SSicfbased(neg2.overview$nwpos[k], neg2.overview$nwneg[k], neg2.overview$Mwpos[k], neg2.overview$Mwneg[k])
}

rm(neg2.tdm.messages)
rm(neg2.tdm.messages.pos)
rm(neg2.tdm.messages.neg)

################
#- Negation 1 -#
################

# # Create TDM matrix
# neg1.messages <- data.frame(doc_id=seq(1:nrow(train)),text=train$NegatedMessage)
# neg1.corpus <- VCorpus(DataframeSource(neg1.messages))
# neg1.tdm<-TermDocumentMatrix(neg1.corpus,control=list(weighting=weightTf))
# neg1.tdm.messages <- as.matrix(neg1.tdm)
# 
# # Minimal 5 occurrences 
# neg1.nw <- rowSums(neg1.tdm.messages)
# neg1.min.5.occur <- (neg1.nw > 4)
# neg1.tdm.messages.new <- neg1.tdm.messages[neg1.min.5.occur,]
# 
# # Get nw and Mw
# neg1.nw.all <- rowSums(neg1.tdm.messages.new)
# neg1.mw.all <- rowSums(neg1.tdm.messages.new > 0)
# 
# # Split bullish and bearish
# neg1.tdm.messages.neg <- neg1.tdm.messages.new[,bearish]
# neg1.tdm.messages.pos <- neg1.tdm.messages.new[,bullish]
# 
# # Get nwc and Mwc
# neg1.nw.neg <- rowSums(neg1.tdm.messages.neg)
# neg1.nw.pos <- rowSums(neg1.tdm.messages.pos)
# neg1.mw.neg <- rowSums(neg1.tdm.messages.neg > 0)
# neg1.mw.pos <- rowSums(neg1.tdm.messages.pos > 0)
# 
# # Create overview with all the needed measures
# neg1.overview <- data.frame(matrix(ncol = 7, nrow = nrow(neg1.tdm.messages.new)))
# colnames(neg1.overview)<- c("Word","Mw", "Mwpos", "Mwneg", "nw", "nwpos", "nwneg")
# neg1.overview$Word <- rownames(neg1.tdm.messages.new)
# neg1.overview$Mw <- neg1.mw.all
# neg1.overview$Mwpos <- neg1.mw.pos
# neg1.overview$Mwneg <- neg1.mw.neg
# neg1.overview$nw <- neg1.nw.all
# neg1.overview$nwpos <- neg1.nw.pos
# neg1.overview$nwneg <- neg1.nw.neg

neg1.overview <- combined.overview # combined.overview from TDMExtra.R
neg1.overview <- neg1.overview[neg1.overview$nw > 4,]

# Create lexicons
neg1.lexiconBTB <- data.frame(matrix(ncol = 2, nrow = nrow(neg1.overview)))
colnames(neg1.lexiconBTB)<- c("Word","Strength")
neg1.lexiconBTB$Word <- neg1.overview$Word
neg1.lexiconPMI <- data.frame(matrix(ncol = 2, nrow = nrow(neg1.overview)))
colnames(neg1.lexiconPMI)<- c("Word","Strength")
neg1.lexiconPMI$Word <- neg1.overview$Word
neg1.lexiconNPMI <- data.frame(matrix(ncol = 2, nrow = nrow(neg1.overview)))
colnames(neg1.lexiconNPMI)<- c("Word","Strength")
neg1.lexiconNPMI$Word <- neg1.overview$Word
neg1.lexiconICF <- data.frame(matrix(ncol = 2, nrow = nrow(neg1.overview)))
colnames(neg1.lexiconICF)<- c("Word","Strength")
neg1.lexiconICF$Word <- neg1.overview$Word 
for(k in 1:nrow(neg1.overview)){
  neg1.lexiconBTB$Strength[k] <- SSBTB(neg1.overview$nw[k], neg1.overview$nwpos[k], neg1.overview$nwneg[k])
  neg1.lexiconPMI$Strength[k] <- max(min(SSPMI(neg1.overview$Mwpos[k], neg1.overview$Mwneg[k], neg1.overview$Mw[k], M, Mc),1),-1)
  neg1.lexiconNPMI$Strength[k] <- SSNPMI(neg1.overview$Mwpos[k], neg1.overview$Mwneg[k], neg1.overview$Mw[k], M, Mc)
  neg1.lexiconICF$Strength[k] <- SSicfbased(neg1.overview$nwpos[k], neg1.overview$nwneg[k], neg1.overview$Mwpos[k], neg1.overview$Mwneg[k])
}
