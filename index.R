#README
###########################
# this script was used to create an index of part of the training corpus
# you have to edit the path to your traing corpus if you want to try it
# the output is saved to two files: all.output and predictors
# the predictors file contains just the n-gram tables used by the app 

pathToTraiiningCorpus <- "final/en_US/all.txt"

source("mylib.R")
require(stringr)
require(data.table)


#take samples and pre-process
beginTime <- Sys.time()
startTime <- Sys.time()
set.seed(1234)
samplingRate = 0.4
makeSampleFile(fileIn="final/en_US/all.txt", fileOut="all.sample.txt", fileOut2="all.not-sample.txt", sr=samplingRate)
preProcessFile(fileIn="all.sample.txt", fileOut="all.processed.txt")
prepTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
prepTime
fileIn="all.processed.txt"

#convert file to string
fi <- file(description=fileIn, open="r")
thisLine <- readLines(con=fi)
#convert string to vector
tokens <- unlist(strsplit(thisLine, split="[[:space:]]"))
tokens <- tokens[str_length(tokens) > 0] #remove zero length strings if any
theseUniqueTokens <- unique(tokens) #immediate (0.5 sec) for 2 million tokens
#cleanup
rm(thisLine)
close(fi)

stopTokens <- c("_linestart_","_lineend_", "_sentencestart_", "_period_", "_question_", "_exclamation_")


startTime <- Sys.time()
#####
#generate all unigrams
tokens.df <- data.frame(tokens=tokens, freq=1, stringsAsFactors=FALSE)
#remove all stop tokens
tokens.df <- tokens.df[!(tokens.df$tokens %in% stopTokens), ]


#aggregating tokens using aggregate - this uses a linear vector scan 
startTime <- Sys.time()
aggTokens.df <- aggregate(tokens.df$freq, by=list(tokens.df$tokens), sum)
timeCount1 <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
timeCount1 #16 minutes for 47M tokens !!!!!!!


#alternative - aggregating tokens using the data.table package - here the aggregate function uses a binary search
library(data.table)
tokens.dt <- data.table(tokens.df)
startTime <- Sys.time()
#setkey(tokens.dt, tokens)
aggTokens.dt <- tokens.dt[,sum(freq),by=tokens]
timeCount2 <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
timeCount2 #3.1 sec for 47M tokens !!!!!!!!!  <---------------------------------- this is a 300 fold speed up

colnames(aggTokens.df) <- c("tokens", "freq")
#remove rows with stop tokens or frequency less than 2
aggTokens.df <- aggTokens.df[(aggTokens.df[,2] > 1),]
#sort
sortOrder <- order(aggTokens.df[,2], decreasing=TRUE)
aggTokens.df <- aggTokens.df[(sortOrder),]
#save a list of unique unigrams occuring more than once that will be extended in bigrams
theseTokensOnly <- aggTokens.df[,1]
#prediction
defaultPrediction <- aggTokens.df[1,1]
#####
unigramTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
unigramTime



startTime <- Sys.time()
#####
#generate all bigrams
tokens1 <- tokens[1:length(tokens)-1]
tokens2 <- tokens[2:length(tokens)]
bigrams.df <- data.frame(tokens1,tokens2, 1, stringsAsFactors=FALSE) 
names(bigrams.df) <- c("tokens1", "tokens2", "freq")
#remove rows with stop tokens 
bigrams.df <- bigrams.df[!(bigrams.df$tokens1 %in% stopTokens | bigrams.df$tokens2 %in% stopTokens), ]
#remove rows that dont contain unigrams occurring more than once
bigrams.df <- bigrams.df[(bigrams.df[,1] %in% theseTokensOnly & bigrams.df[,2] %in% theseTokensOnly), ]
#count
aggBigrams.df <- aggregate(bigrams.df$freq, list(token1=bigrams.df$tokens1, token2=bigrams.df$tokens2), sum)
names(aggBigrams.df) <- c("tokens1", "tokens2", "freq")
#remove rows with frequency less than 2
aggBigrams.df <- aggBigrams.df[(aggBigrams.df$freq > 1),]
#sort
sortOrder <- order(aggBigrams.df$freq, decreasing=TRUE)
aggBigrams.df <- aggBigrams.df[sortOrder,]
#save a list of unique bigrams occuring more than once that will be extended in trigrams
theseBigramsOnly <- paste(aggBigrams.df[,1], aggBigrams.df[,2], sep=" ")
#select best predictions
# 1 - order by first (predictive) token then by bigram frequency
aggBigrams.df<-aggBigrams.df[order(aggBigrams.df$tokens1, aggBigrams.df$freq, decreasing=TRUE),] 
# 2 - match will retrieve the first matching row and hence the best because of the above sort order
theseBest<-match(theseTokensOnly, aggBigrams.df$tokens1) 
theseBest <- theseBest[!(is.na(theseBest))]
unigramPredictors.df <- aggBigrams.df[theseBest,]   
unigramPredictors.df[unigramPredictors.df$tokens1=="yourself",] #test
################
bigramTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
bigramTime



startTime <- Sys.time()
#####
#generate all trigrams
tokens1 <- tokens[1:(length(tokens)-2)]
tokens2 <- tokens[2:(length(tokens)-1)]
tokens3 <- tokens[3:(length(tokens)-0)]
trigrams.df <- data.frame(tokens1,tokens2, tokens3, 1, stringsAsFactors=FALSE) 
names(trigrams.df) <- c("tokens1", "tokens2", "tokens3", "freq")
#remove rows with stop tokens 
trigrams.df <- trigrams.df[!
                             (trigrams.df$tokens1 %in% stopTokens | 
                                trigrams.df$tokens2 %in% stopTokens |
                                trigrams.df$tokens3 %in% stopTokens
                             ), ]
#remove rows that dont contain bigrams occurring more than once
bigramsHere <- paste(trigrams.df[,1], trigrams.df[,2], sep=" ")
followThese <- bigramsHere %in% theseBigramsOnly
trigrams.df <- trigrams.df[followThese, ]
#count
aggTrigrams.df <- aggregate(trigrams.df$freq, list(token1=trigrams.df$tokens1, token2=trigrams.df$tokens2, token3=trigrams.df$tokens3), sum)
names(aggTrigrams.df) <- c("tokens1", "tokens2", "tokens3", "freq")
#remove rows with frequency less than 2
aggTrigrams.df <- aggTrigrams.df[(aggTrigrams.df$freq > 1),]
#sort
sortOrder <- order(aggTrigrams.df$freq, decreasing=TRUE)
aggTrigrams.df <- aggTrigrams.df[sortOrder,]
#save a list of unique trigrams occuring more than once that will be extended in trigrams
theseTrigramsOnly <- paste(aggTrigrams.df[,1], aggTrigrams.df[,2], aggTrigrams.df[,3], sep=" ")
#select best predictions
# 0 - #add the bigrams to the data frame 
aggTrigrams.df <- cbind(aggTrigrams.df, bigrams=paste(aggTrigrams.df[,1], aggTrigrams.df[,2], sep=" "))
# 1 - order by first (predictive) token then by bigram frequency
aggTrigrams.df<-aggTrigrams.df[order(aggTrigrams.df$bigrams, aggTrigrams.df$freq, decreasing=TRUE),] 
# 2 - match will retrieve the first matching row and hence the best because of the above sort order
theseBest<-match(theseBigramsOnly, aggTrigrams.df$bigrams) 
theseBest <- theseBest[!(is.na(theseBest))]
bigramPredictors.df <- aggTrigrams.df[theseBest,]   
bigramPredictors.df[which(bigramPredictors.df$bigrams=="of fresh"),] #test
################
trigramTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
trigramTime




startTime <- Sys.time()
#####
#generate all quadragrams
tokens1 <- tokens[1:(length(tokens)-3)]
tokens2 <- tokens[2:(length(tokens)-2)]
tokens3 <- tokens[3:(length(tokens)-1)]
tokens4 <- tokens[4:(length(tokens)-0)]
quadragrams.df <- data.frame(tokens1, tokens2, tokens3, tokens4, 1, stringsAsFactors=FALSE) 
names(quadragrams.df) <- c("tokens1", "tokens2", "tokens3","tokens4",  "freq")
#remove rows with stop tokens 
quadragrams.df <- quadragrams.df[!
                                   (quadragrams.df$tokens1 %in% stopTokens | 
                                      quadragrams.df$tokens2 %in% stopTokens |
                                      quadragrams.df$tokens3 %in% stopTokens |
                                      quadragrams.df$tokens4 %in% stopTokens
                                   ), ]
#remove rows that dont contain bigrams occurring more than once
trigramsHere <- paste(quadragrams.df[,1], quadragrams.df[,2], quadragrams.df[,3], sep=" ")
followThese <- trigramsHere %in% theseTrigramsOnly
quadragrams.df <- quadragrams.df[followThese, ]
#count
aggQuadragrams.df <- aggregate(quadragrams.df$freq, list(token1=quadragrams.df$tokens1, token2=quadragrams.df$tokens2, token3=quadragrams.df$tokens3, token4=quadragrams.df$tokens4), sum)
names(aggQuadragrams.df) <- c("tokens1", "tokens2", "tokens3", "tokens4", "freq")
#remove rows with frequency less than 2
aggQuadragrams.df <- aggQuadragrams.df[(aggQuadragrams.df$freq > 1),]
#sort
sortOrder <- order(aggQuadragrams.df$freq, decreasing=TRUE)
aggQuadragrams.df <- aggQuadragrams.df[sortOrder,]
#save a list of unique quadragrams occuring more than once that will be extended in pentagrams
theseQuadragramsOnly <- paste(aggQuadragrams.df[,1], aggQuadragrams.df[,2], aggQuadragrams.df[,3], aggQuadragrams.df[,4], sep=" ")
#select best predictions
# 0 - #add the bigrams to the data frame 
aggQuadragrams.df <- cbind(aggQuadragrams.df, trigrams=paste(aggQuadragrams.df[,1], aggQuadragrams.df[,2], aggQuadragrams.df[,3], sep=" "))
# 1 - order by first (predictive) token then by bigram frequency
aggQuadragrams.df<-aggQuadragrams.df[order(aggQuadragrams.df$trigrams, aggQuadragrams.df$freq, decreasing=TRUE),] 
# 2 - match will retrieve the first matching row and hence the best because of the above sort order
theseBest<-match(theseTrigramsOnly, aggQuadragrams.df$trigrams) 
theseBest <- theseBest[!(is.na(theseBest))]
trigramPredictors.df <- aggQuadragrams.df[theseBest,]   
trigramPredictors.df[which(trigramPredictors.df$trigrams=="in the best"),] #test
################
quadragramTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
quadragramTime







startTime <- Sys.time()
#generate all pentagrams
tokens1 <- tokens[1:(length(tokens)-4)]
tokens2 <- tokens[2:(length(tokens)-3)]
tokens3 <- tokens[3:(length(tokens)-2)]
tokens4 <- tokens[4:(length(tokens)-1)]
tokens5 <- tokens[5:(length(tokens)-0)]
pentagrams.df <- data.frame(tokens1, tokens2, tokens3, tokens4, tokens5, 1, stringsAsFactors=FALSE) 
names(pentagrams.df) <- c("tokens1", "tokens2", "tokens3","tokens4", "tokens5", "freq")
#remove rows with stop tokens 
pentagrams.df <- pentagrams.df[!
                                 (pentagrams.df$tokens1 %in% stopTokens | 
                                    pentagrams.df$tokens2 %in% stopTokens |
                                    pentagrams.df$tokens3 %in% stopTokens |
                                    pentagrams.df$tokens4 %in% stopTokens |
                                    pentagrams.df$tokens5 %in% stopTokens
                                 ), ]
#remove rows that dont contain bigrams occurring more than once
quadragramsHere <- paste(pentagrams.df[,1], pentagrams.df[,2], pentagrams.df[,3], pentagrams.df[,4], sep=" ")
followThese <- quadragramsHere %in% theseQuadragramsOnly
pentagrams.df <- pentagrams.df[followThese, ]
#count
aggPentagrams.df <- aggregate(pentagrams.df$freq, list(token1=pentagrams.df$tokens1, token2=pentagrams.df$tokens2, token3=pentagrams.df$tokens3, token4=pentagrams.df$tokens4, tokens5=pentagrams.df$tokens5), sum)
names(aggPentagrams.df) <- c("tokens1", "tokens2", "tokens3", "tokens4", "tokens5", "freq")
#remove rows with frequency less than 2
aggPentagrams.df <- aggPentagrams.df[(aggPentagrams.df$freq > 1),]
#sort
sortOrder <- order(aggPentagrams.df$freq, decreasing=TRUE)
aggPentagrams.df <- aggPentagrams.df[sortOrder,]
#save a list of unique quadragrams occuring more than once that will be extended in pentagrams
thesePentagramsOnly <- paste(aggPentagrams.df[,1], aggPentagrams.df[,2], aggPentagrams.df[,3], aggPentagrams.df[,4], aggPentagrams.df[,5], sep=" ")
#select best predictions
# 0 - #add the bigrams to the data frame 
aggPentagrams.df <- cbind(aggPentagrams.df, quadragrams=paste(aggPentagrams.df[,1], aggPentagrams.df[,2], aggPentagrams.df[,3], aggPentagrams.df[,4], sep=" "))
# 1 - order by first (predictive) token then by bigram frequency
aggPentagrams.df<-aggPentagrams.df[order(aggPentagrams.df$quadragrams, aggPentagrams.df$freq, decreasing=TRUE),] 
# 2 - match will retrieve the first matching row and hence the best because of the above sort order
theseBest<-match(theseQuadragramsOnly, aggPentagrams.df$quadragrams) 
theseBest <- theseBest[!(is.na(theseBest))]
quadragramPredictors.df <- aggPentagrams.df[theseBest,]   
quadragramPredictors.df[which(quadragramPredictors.df$quadragrams=="the end of the"),] #test
################
pentagramTime <- format(difftime(Sys.time(), startTime), format="%H:%M:%S")
pentagramTime

totalTime <- format(difftime(Sys.time(), beginTime), format="%H:%M:%S")
totalTime


save(list=c("samplingRate", "prepTime", "tokens.df", "stopTokens", "preProcessFile",
            "aggTokens.df", "defaultPrediction", "unigramTime",
            "aggBigrams.df", "unigramPredictors.df", "bigramTime",
            "aggTrigrams.df", "bigramPredictors.df", "trigramTime",
            "aggQuadragrams.df", "trigramPredictors.df", "quadragramTime",
            "aggPentagrams.df", "quadragramPredictors.df", "pentagramTime",
            "totalTime" ), 
     file="all.output")

save(list=c("defaultPrediction", "unigramPredictors.df","bigramPredictors.df","trigramPredictors.df","quadragramPredictors.df"), file="predictors")




thisExperiment <- c(samplingRate, length(tokens), totalTime, 
                    sum(quadragramPredictors.df$freq > 4),
                    sum(quadragramPredictors.df$freq > 9),
                    sum(quadragramPredictors.df$freq > 19),
                    sum(quadragramPredictors.df$freq > 39)  )

names(thisExperiment) <- c("samplingRate", "ntokens", "time", "gt4", "gt9", "gt19", "gt39")

thisExperiment



