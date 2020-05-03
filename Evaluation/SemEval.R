# Load the package required to read JSON files.
library("rjson")

# Headlines
headl <- rjson::fromJSON(file = "headlines.json")
headlines <- data.frame(do.call("rbind", headl))
nrow(headlines)
headlines <- headlines[!(headlines$sentiment < 0.05 & headlines$sentiment > -0.05),] # remove 83 headlines
nrow(headlines[headlines$sentiment>0,])
nrow(headlines[headlines$sentiment<0,])
headlines <- headlines[,c(3,4)]
headlines$sentiment <- ifelse(headlines$sentiment > 0, 1, -1)
colnames(headlines) <- c("Message", "Sentiment")


# microblog
micro <- rjson::fromJSON(file = "microblog.json")
microblogs <- data.frame(do.call("rbind", micro))
for (i in 1:nrow(microblogs)){
  microblogs$spans[[i]] <- paste(microblogs$spans[[i]], collapse = " ")
}
nrow(microblogs)
microblogs <- microblogs[!sapply(microblogs$spans, is_empty),]
nrow(microblogs)
microblogs <- microblogs[!(microblogs$sentiment < 0.05 & microblogs$sentiment > -0.05),] # remove 83 microblogs
nrow(microblogs)
nrow(microblogs[microblogs$sentiment>0,])
nrow(microblogs[microblogs$sentiment<0,])

microblogs <- microblogs[,c(5,3)]
colnames(microblogs) <- c("Message", "Sentiment")
microblogs$Sentiment <- ifelse(microblogs$Sentiment > 0, 1, -1)

# StockTwits
stocktwits <- test[,c(1,2)]
stocktwits$Sentiment <- ifelse(stocktwits$Sentiment == 'bullish', 1, -1)

stocktwits2 <- cbind(test[,3], stocktwits$Sentiment)
colnames(stocktwits2) <- c("Message", "Sentiment")
