### proc_lexicons.R file ###
library(tm) # load R package tm: framework for text mining applications
library(SnowballC) # load SnowballC: An R interface to the C libstemmer library that implements Porter's word stemming algorithm
library(XLConnect) # load XLConnect package: Manipulation of Excel files
library(stringi)

# process financial lexicon (FIN) created by Loughran and McDonald
FIN_n<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Negative.csv", stringsAsFactors = FALSE)[,1] # read negative word list
FIN_p<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Positive.csv", stringsAsFactors = FALSE)[,1] # read positive word list
lex_FIN<-as.data.frame(rbind(cbind(FIN_p,rep(1,length(FIN_p))), cbind(FIN_n,rep(-1,length(FIN_n)))),stringsAsFactors=FALSE) # create a data frame with all words and respective sentiment label (1 for positive and -1 for negative)
colnames(lex_FIN)<-c("Word","Sentiment")
lex_FIN$Word<-tolower(lex_FIN$Word) # convert to lowercase
write.csv(lex_FIN,"FIN.csv",row.names=FALSE) # write the processed FIN lexicon to a CSV file

# process Harvard General Inquirer (GI) lexicon
download.file(url="http://www.wjh.harvard.edu/~inquirer/inquirerbasic.xls", destfile="inquirerbasic.xls", mode="wb") # download GI lexicon
GI_book = loadWorkbook("inquirerbasic.xls") # load XLS file containing GI lexicon
GI_sheet = readWorksheet(GI_book,sheet=1,header=TRUE) # read lexicon sheet
GI_n<-GI_sheet$Entry[!is.na(GI_sheet$Negativ)] # select all words from "Negativ" category
GI_p<-GI_sheet$Entry[!is.na(GI_sheet$Positiv)] # select all words from "Positiv" category
lex_GI<-as.data.frame(rbind(cbind(GI_p,rep(1,length(GI_p))), cbind(GI_n,rep(-1,length(GI_n)))),stringsAsFactors=FALSE) # create a data frame with all words and respective sentiment label (1 for positive and -1 for negative)
colnames(lex_GI)<-c("Word","Sentiment")
lex_GI$Word <-tolower(lex_GI$Word) # convert to lowercase
lex_GI$Word<-gsub("#(.*)$","",lex_GI$Word) # process repeated words (e.g., ABOUT#1, ABOUT#2)
lex_GI<-unique(lex_GI) # remove duplicated elements
lex_GI<-lex_GI[!(lex_GI$Word %in% lex_GI$Word[duplicated(lex_GI$Word)]),] # exclude stems appearing in both lists
write.csv(lex_GI,"GI.csv",row.names=FALSE) # write the processed GI lexicon to a CSV file

# NRC Hashtag
lex_NRC1 <- read.delim("NRCHashtag.txt", stringsAsFactor = FALSE, header = FALSE)
lex_NRC1 <- lex_NRC1[,c(1,2)]
colnames(lex_NRC1) <- c("Word", "Sentiment")
lex_NRC1$Word<-gsub("[#@](.*)$","",lex_NRC1$Word) 
lex_NRC1 <- lex_NRC1[!stri_isempty(lex_NRC1$Word),]

# NRC Emoticon
lex_NRC2 <- read.delim("NRCSentiment140.txt", stringsAsFactor = FALSE, header = FALSE)
lex_NRC2 <- lex_NRC2[,c(1,2)]
colnames(lex_NRC2) <- c("Word", "Sentiment")
lex_NRC2$Word<-gsub("[#@](.*)$","",lex_NRC2$Word) 
lex_NRC2 <- lex_NRC2[!stri_isempty(lex_NRC2$Word),]

#Hu and Liu
HLPos <- read.delim("HLPos.txt", stringsAsFactor = FALSE, header = FALSE)
HLPos <- cbind(HLPos,rep(1,nrow(HLPos)))
colnames(HLPos) <- c("Word", "Sentiment")
HLNeg <- read.delim("HLNeg.txt", stringsAsFactor = FALSE, header = FALSE)
HLNeg <- cbind(HLNeg,rep(-1,nrow(HLNeg)))
colnames(HLNeg) <- c("Word", "Sentiment")
lex_HL <- rbind(HLPos, HLNeg)

# Stock market
lex_SM <- read_csv("StockMarket.csv", col_types = cols(Neg_Score = col_skip(), POS = col_skip()))
colnames(lex_SM) <- c("Word", "Sentiment")

# MPQA
MPQA <- read.table("MPQA.txt", stringsAsFactor = FALSE, header = FALSE, sep = " ")
lex_MPQA <- MPQA[,c(1,3,6)]
lex_MPQA <- lex_MPQA[!(lex_MPQA$V6 == "both"), ]
for(i in 1:nrow(lex_MPQA)){
  if (lex_MPQA$V6[i] == "negative"){
    if(lex_MPQA$V1[i] == "weaksubj"){
      lex_MPQA$Sentiment[i] <- -0.5
    }
    else{
      lex_MPQA$Sentiment[i] <- -1
    }
  }
  else if (lex_MPQA$V6[i] == "positive"){
    if(lex_MPQA$V1[i] == "weaksubj"){
      lex_MPQA$Sentiment[i] <- 0.5
    }
    else{
      lex_MPQA$Sentiment[i] <- 1
    }
  }
  else{
    lex_MPQA$Sentiment[i] <- 0
  }
}
lex_MPQA <- lex_MPQA[,c(2,4)]
colnames(lex_MPQA) <- c("Word", "Sentiment")
lex_MPQA <- unique(lex_MPQA)

# VADER
VADER <- read.delim("VADER.txt", stringsAsFactor = FALSE, header = FALSE)
lex_VADER <- VADER[,c(1,2)]
colnames(lex_VADER) <- c("Word", "Sentiment")