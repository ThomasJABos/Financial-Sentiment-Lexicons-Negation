# Use this code if the TDM matrix is too large to be stored (split the TDM matrix by using this file)


# Create TDM matrix
messages1 <- data.frame(doc_id=seq(180001:200000),text=train$NegatedMessage[180001:200000])
corpus1 <- VCorpus(DataframeSource(messages1))
tdm1<-TermDocumentMatrix(corpus1,control=list(weighting=weightTf))
tdm.messages1 <- as.matrix(tdm1)

# Get nw and Mw
nw.all1 <- rowSums(tdm.messages1)
mw.all1 <- rowSums(tdm.messages1 > 0)

# Split bullish and bearish
bullish <- (train$Sentiment[180001:200000] == 'bullish')
bearish <- (train$Sentiment[180001:200000] == 'bearish')
tdm.messages.neg1 <- tdm.messages1[,bearish]
tdm.messages.pos1 <- tdm.messages1[,bullish]

# Create overview with all the needed measures
overview1 <- data.frame(matrix(ncol = 7, nrow = nrow(tdm.messages1)))
colnames(overview1)<- c("Word","Mw", "Mwpos", "Mwneg", "nw", "nwpos", "nwneg")
overview1$Word <- rownames(tdm.messages1)


rm(tdm.messages1) 

# Get nwc and Mwc
nw.neg1 <- rowSums(tdm.messages.neg1)
nw.pos1 <- rowSums(tdm.messages.pos1)
mw.neg1 <- rowSums(tdm.messages.neg1 > 0)
mw.pos1 <- rowSums(tdm.messages.pos1 > 0)

overview1$Mw <- mw.all1
overview1$Mwpos <- mw.pos1
overview1$Mwneg <- mw.neg1
overview1$nw <- nw.all1
overview1$nwpos <- nw.pos1
overview1$nwneg <- nw.neg1

part10 <- overview1
rm(tdm.messages.neg1)
rm(tdm.messages.pos1)


######
# Join the seperate parts
combined.overview <- merge(combined.overview,part10,by="Word", all = TRUE) #first combine part1 and part2
combined.overview <- replace_na(combined.overview,value = 0)

for (p in 1:nrow(combined.overview)){
  if (!is.na(combined.overview$Mw.y[p])){
    combined.overview$Mw.x[p] <- combined.overview$Mw.x[p] + combined.overview$Mw.y[p]
    combined.overview$Mwpos.x[p] <- combined.overview$Mwpos.x[p] + combined.overview$Mwpos.y[p] 
    combined.overview$Mwneg.x[p] <- combined.overview$Mwneg.x[p] + combined.overview$Mwneg.y[p]
    combined.overview$nw.x[p] <- combined.overview$nw.x[p] + combined.overview$nw.y[p]
    combined.overview$nwpos.x[p] <- combined.overview$nwpos.x[p] + combined.overview$nwpos.y[p] 
    combined.overview$nwneg.x[p] <- combined.overview$nwneg.x[p] + combined.overview$nwneg.y[p]
  }
}

combined.overview <- combined.overview[,c(1:7)]
colnames(combined.overview)<- c("Word","Mw", "Mwpos", "Mwneg", "nw", "nwpos", "nwneg")

