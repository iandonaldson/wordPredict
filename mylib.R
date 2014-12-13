# makeSampleFile
# Given a path to a text file (fileIn), n lines will be randomly sampled and written to a file (fileOut, default=sample.txt).
# Alternatively a sampling rate (sr, in the range 0:1) can be specified instead of n number of lines.
# Lines not chosen for the sample are all written to a second file (fileOut2, default=notSample.txt) so they can be later used
#for validation or testing
makeSampleFile <- function(fileIn, fileOut="sample.txt", fileOut2="notSample.txt", sr, n=1){
  #prep
  if (is.null(sr)) {
    cmd <- paste("wc -l", fileIn)  
    nLines<-as.numeric(unlist(strsplit(system(cmd, intern=TRUE), split=" ", fixed=TRUE))[3])
    sr <- n/nLines
  }
  fi <- file(description=fileIn, open="r")
  fo <- file(description=fileOut, open="w")
  fo2 <- file(description=fileOut2, open="w")
  
  #do it
  thisLine <- readLines(con=fi, n=1)
  while (length(thisLine) != 0) {
    if (rbinom(1,1,sr)){
      writeLines(thisLine, con=fo)
    } else {
      writeLines(thisLine, con=fo2)
    }
    thisLine <- readLines(con=fi, n=1)
  }
  close(fi)
  close(fo)
  close(fo2)
}

# preProcessFile
# Given a text file (fileIn), pre-process the file and write to a new text file (fileOut, default=preprocessed).
preProcessFile <- function(fileIn, fileOut) {
  
  fi <- file(description=fileIn, open="r")
  fo <- file(description=fileOut, open="w")
  
  thisLine <- readLines(con=fi, n=1)
  while (length(thisLine) != 0) {
    
    #all the fun stuff is in this function
    thisLine <- preProcessLine(thisLine)
    
    #write it
    writeLines(thisLine, con=fo)
    
    #next line
    thisLine <- readLines(con=fi, n=1)
  }
  #clean up
  close(fi)
  close(fo)
  
}

preProcessLine <- function(thisLine) {
  
  #initial clean up and establish sentence/line boundaries 
  thisLine <- gsub(pattern="(\")", replacement=" ", x=thisLine) #remove double quotes
  thisLine <- gsub(pattern="\u2019", replacement="'", x=thisLine) #replace troublesome u2019 quote with regular straight quote
  thisLine <- gsub(pattern="(^|\\W)'", replacement="\\1 ", x=thisLine) #remove single quotes preceded by non-word characters or beginning of line
  thisLine <- gsub(pattern="'(\\W|$)", replacement=" \\1", x=thisLine) #remove single quotes succeded by non-word characters or end of line
  
  thisLine <- gsub(pattern="(^[^A-Za-z]*)", replacement=" ", x=thisLine) #remove non word characters at beginning
  thisLine <- gsub(pattern="([\\.\\?\\!]{0,1})[^A-Za-z0-9]*$", replacement="\\1", x=thisLine) #remove non word characters at end
  thisLine <- gsub(pattern="([^\\.\\?\\!]$)", replacement="\\1 _lineEnd_", x=thisLine) #add a lineEnd if no .!?
  thisLine <- gsub(pattern="(^[^A-Z])", replacement="_lineStart_ \\1", x=thisLine) #line is not beginning of sentence
  thisLine <- gsub(pattern="(^[A-Z])", replacement="_sentenceStart_ \\1", x=thisLine) #line is beginning of sentence
  thisLine <- gsub(pattern="(\\.+ +)([A-Z])", replacement=" _period_  _sentenceStart_ \\2", x=thisLine) #sentence start in middle of line
  thisLine <- gsub(pattern="(\\!+ +)([A-Z])", replacement=" _exclamation_  _sentenceStart_ \\2", x=thisLine) #sentence start in middle of line
  thisLine <- gsub(pattern="(\\?+ +)([A-Z])", replacement=" _question_  _sentenceStart_ \\2", x=thisLine) #sentence start in middle of line
  
  
  # . ! and ? are treated as end of sentence character - remove duplicates
  thisLine <- gsub(pattern="\\.+( |$)", replacement=" _period_ ", x=thisLine) #period
  thisLine <- gsub(pattern="\\!+( |$)", replacement=" _exclamation_ ", x=thisLine) #exclamation
  thisLine <- gsub(pattern="\\?+( |$)", replacement=" _question_ ", x=thisLine)#question mark
  
  #look for and deal with all other types of punctuation
  thisLine <- gsub(pattern=" @ ", replacement=" at ", x=thisLine)#@ as the word at
  thisLine <- gsub(pattern=" \\w+\\.*\\w+@\\w+\\.\\w+ ", replacement=" _email_ ", x=thisLine)#recognize email
  thisLine <- gsub(pattern=" # ", replacement=" number ", x=thisLine)# # as the word number
  thisLine <- gsub(pattern=" #\\w+\\S* ", replacement=" _hashTag_ ", x=thisLine)# # as a hash tag
  thisLine <- gsub(pattern=" [\\$£]\\d+\\S* ", replacement=" _price_ ", x=thisLine)# # recognize prices
  thisLine <- gsub(pattern=" \\d+\\.{0,1}\\d*% ", replacement=" _percentage_  ", x=thisLine)# # recognize prices #bug - subseququent %'s dont work e.g. 100% 50%
  thisLine <- gsub(pattern="\\+", replacement=" plus ", x=thisLine) # + as a word
  thisLine <- gsub(pattern="(\\w| )=(\\w| )", replacement="\\1 equals \\2", x=thisLine) # = as a word
  thisLine <- gsub(pattern=" \\d+ ", replacement=" _number_ ", x=thisLine) # = recognize numbers
  thisLine <- gsub(pattern=" \\d+\\.\\d ", replacement=" _float_ ", x=thisLine) # = recognize floats
  thisLine <- gsub(pattern="\\(|\\)|\\{|\\}|\\[|\\]", replacement=" ", x=thisLine) #remove braces
  thisLine <- gsub(pattern="\\^", replacement=" ", x=thisLine) #remove hats
  thisLine <- gsub(pattern="[^[:alnum:]|_|'|\u2019]", replacement=" ", x=thisLine) ##remove all remaining punctuation and non-word characters except _ and '    
  
  #lastly, remove any double spaces
  thisLine <- gsub(pattern="\\s{2,}", replacement=" ", x=thisLine)
  
  #all lower case
  thisLine <- tolower(thisLine)
  
  #return
  return(thisLine)
  
}




# makeHashMaps
# Given a test file (fileIn), create two hash maps.  
# p2t positions2tokens is a hash map of the entire text 
# with keys as positions in the text and values are the token strings at that position. 
# t2p (token2positions) is a hash map with keys corresponding to each distinct token found 
# in the text and the value for each is a vector of postions where that token is found in 
# the text.
makeHashMaps <- function(fileIn, fileOut) {
  require(stringr) #for str_length
  require(hash)
  
  #initiate file in ptr and empty hashes
  fi <- file(description=fileIn, open="r")
  thisLine <- readLines(con=fi, n=1)
  p2t <- hash()
  t2p <- hash()
  offset <- 0
  
  while (length(thisLine) != 0) {
    #process the line - to be moved to a separate function
    
    #split the line by spaces and get their indices in document
    tokens <- unlist(strsplit(thisLine, split="[[:space:]]"))
    tokens <- tokens[str_length(tokens) > 0] #remove zero length strings if any
    indices <- c((1+offset):(length(tokens)+offset))
    
    #fill p2t
    i <- 1 #position in tokens 
    for (index in indices) {
      p2t[ index ] <- tokens[i]
      i <- i + 1
    }
    #p2t
    
    #fill t2p
    i <- 1 #position in indices
    for (token in tokens) {
      if (has.key(token, t2p)){
        t2p[ token ] <- c(t2p[[ token ]], indices[i])
      }
      else {
        t2p[ token ] <- indices[i]
      }
      i <- i + 1
    }
    #t2p
    #next line
    thisLine <- readLines(con=fi, n=1)
    offset <- offset + length(tokens)
  }
  
  
  #this might be faster
  #t2p2 <- invert(p2t)
  #h <- hash( a=1, b=1:2, c=1:3 )
  #invert(h)
  
  #save the hash maps to disk for later use if a fileout name was specified
  if (exists("fileOut")){
    fileName <- paste(fileOut, "map", sep=".")
    save(p2t, t2p, file=fileName)
  }
  
  #clean up
  close(fi)
  
  #return
  return(list("p2t"=p2t, "t2p"=t2p))
  
}




#getNextGramMap
#given a position to token map (p2t) of a corpus and a ngram to position map (t2p: where n can be 1,2,3...)
#return a (n+1)gram2position map.
#the returned gram2pos map is a hash with keys that are n-grams (strings) and the value is a vector of 
#positions where the n-gram ends 
#test
getNextGramMap <- function(p2t, t2p){
  require(hash)
  
  #prepare
  maxPosition <- max(as.integer(keys(p2t)))
  stopTokens <- c("_linestart_","_lineend_", "_sentencestart_", "_period_", "_question_", "_exclamation_")
  gram2pos <- hash()
  
  #thisToken <- "this"
  for (thisToken in keys(t2p)) {
    
    #ignore stop tokens
    if (thisToken %in% stopTokens) {next}
    
    #determine positions of next tokens that are valid
    startPositions <- t2p[[ thisToken ]]  
    #nextPositions <- as.integer(startPositions) + 1 #<---this caused hours of fun
    #x<-"200000"
    #x<-as.integer(x) + 1
    #x
    #[1] 200001 <--looks ok but...
    #str(x)
    #num 2e+05 <---this, when cast as a string, will be '2e+05' which is not a valid key in our hash
    #the next line solves the problem by casting the entire expression as.integer (not position of brackets) -fuckR
    nextPositions <- as.integer(startPositions + 1)
    nextPositions <- nextPositions[nextPositions <= maxPosition] #in range
    nextTokens <- values(p2t[ as.character(nextPositions) ]) #<----see note above - keys to hashes must be charcter vectors 
    onlyThese <- !(nextTokens %in% stopTokens) #not a stop token
    if (sum(onlyThese)==0) next
    
    #make lists of next tokens and their positions and (bi,tri,n-grams)
    startPositions <- startPositions[onlyThese]
    nextPositions <- nextPositions[onlyThese]
    nextTokens <- nextTokens[onlyThese]
    grams<-unname(sapply(nextTokens, function(x) paste(thisToken, x, sep=" "))) #<---------------continue here
    
    #add to a gram2pos hash - the end position of the gram is used to record its position
    for (i in 1:length(grams)) {
      
      if (has.key(grams[i], gram2pos)){
        gram2pos[ grams[i] ] <- c(gram2pos[[ grams[i] ]], nextPositions[i])
      } 
      else {
        gram2pos[ grams[i] ] <- nextPositions[i]  
      }
    }
    #gram2pos[ keys(gram2pos) ]
    
  }  
  
  return(gram2pos)
  
}


#given a token to position map (t2p, where a token could be a unigram, bigram,...ngram), return a 
#hash of token to frequency in the corpus (gram2pos), a table of grams sorted by frequency and 
#with percentage/cumulative percentage of corpus taken by that token (grams.df) and the 
#number of tokens (nt90) or the percentage of tokens (pt90) that make up 90% of the corpus
gramAnalysis <- function(t2p){
  #create a hash of gram frequencies
  gramFreqs <- hash()
  for (thisKey in keys(t2p)){
    gramFreqs[ thisKey ] <- length(t2p[[ thisKey ]])
  }  
  
  head(values(t2p))
  
  #create a table
  grams.df <- data.frame(token=keys(gramFreqs), freq=as.integer(values(gramFreqs)), stringsAsFactors=FALSE, row.names=NULL)
  grams.df <- grams.df[order(grams.df$freq, decreasing=TRUE),]
  colnames(grams.df) <- c("token", "freq")
  rownames(grams.df) <- NULL
  percent <- grams.df$freq/sum(grams.df$freq)*100
  cumulative <- cumsum(percent)
  grams.df <- cbind(grams.df, percent, cumulative)
  
  #what number/percent of the tokens make up 90% of the text
  nt90 <- sum(cumulative < 90) 
  pt90 <- sum(cumulative < 90)/sum(grams.df$freq) #0.24
  
  return(list("gramFreqs"=gramFreqs, "grams.df"=grams.df, "nt90"=nt90, "pt90"=pt90))
  
}




predictNextWord <- function(inputString, cutoff=5, all=FALSE) {
  
  #set up the dataframe of possible results and the default answer
  iSay <- data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(iSay) <- c("next", "freq")
  defaultAns <- as.data.frame(list("the", (cutoff + 1) ), stringsAsFactors=FALSE)
  colnames(defaultAns) <- c("next", "freq")
  
  #prepare the query from the inputString
  len <- length(inputString)
  if (len == 0){
    return("Try typing something.")
  }
  #preprocess the input in the same way the corpus was preprocessed - remove the _lineend_ introduced by preProcessLne
  inputString <- preProcessLine(inputString)
  inputString <- gsub(pattern="_lineend_", replacement="", x=inputString) 
  #split
  inputVector <- unlist(strsplit(inputString, split="[[:space:]]"))
  len <- length(inputVector)
  #check
  if (len == 0){
    return("Try typing something else with words in it.")
  }
  
  #if the last 4 words of the input phrase are defined, use them to predict the next word using quadragramPredictors
  if (sum(is.na(tail(inputVector,4)))==0){
    youSay <- paste(tail(inputVector,4), collapse=" ")
    possible <- quadragramPredictors.df[quadragramPredictors.df$quadragrams==youSay, 5:6]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  if (sum(is.na(tail(inputVector,3)))==0){
    youSay <- paste(tail(inputVector,3), collapse=" ")
    possible <- trigramPredictors.df[trigramPredictors.df$trigrams==youSay, 4:5]   
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  
  if (sum(is.na(tail(inputVector,2)))==0){
    youSay <- paste(tail(inputVector,2), collapse=" ")
    possible <- bigramPredictors.df[bigramPredictors.df$bigrams==youSay, 3:4] 
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  
  
  if (sum(is.na(tail(inputVector,1)))==0){
    youSay <- paste(tail(inputVector,1), collapse=" ")
    possible <- unigramPredictors.df[unigramPredictors.df[,1]==youSay, 2:3]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  #add the default answer last
  iSay <- rbind(iSay, defaultAns[1,]) 
  
  
  #select the best word - the word that was predicted by the longest ngram and meets the cutoff 
  iSay$freq <- as.integer(iSay$freq)
  predict <- iSay[which(iSay$freq > cutoff)[1], 1]
  
  if (all == TRUE){
    return(iSay)
  } else {
    return(predict)
  }  
}



predictNextWord2 <- function(inputVector, cutoff=5, all=FALSE) {
  iSay <- data.frame(matrix(vector(), 0, 2), stringsAsFactors=F)
  colnames(iSay) <- c("next", "freq")
  
  if (sum(is.na(tail(inputVector,4)))==0){
    youSay <- tail(inputVector,4)
    possible <- quadragramPredictors.df[quadragramPredictors.df$tokens1==youSay[1] & 
                                          quadragramPredictors.df$tokens2==youSay[2] & 
                                          quadragramPredictors.df$tokens3==youSay[3] & 
                                          quadragramPredictors.df$tokens4==youSay[4], 5:6]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  
  if (length(youSay) > 2) {
    possible <- trigramPredictors.df[trigramPredictors.df$tokens1==youSay[1] & 
                                       trigramPredictors.df$tokens2==youSay[2] & 
                                       trigramPredictors.df$tokens3==youSay[3], 4:5]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  if (length(youSay) > 1) {
    possible <- bigramPredictors.df[bigramPredictors.df$tokens1==youSay[1] & 
                                      bigramPredictors.df$tokens2==youSay[2], 3:4]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  
  if (length(youSay) > 0) {
    possible <- unigramPredictors.df[unigramPredictors.df$tokens1==youSay[1], 2:3]  
    if (length(possible) > 0){
      colnames(possible) <- c("next", "freq")
      iSay <- rbind(iSay, possible)
    }
  }
  iSay
  
  #add the default word
  iSay <- rbind(iSay, c("the", (cutoff + 1)))
  
  #select the best word - the word that was predicted by the longest ngram and meets the cutoff 
  iSay$freq <- as.integer(iSay$freq)
  predict <- iSay[which(iSay$freq > cutoff)[1], 1]
  
  if (all == TRUE){
    return(iSay)
  } else {
    return(predict)
  }  
}



testFunction <- function(){
  x<-list("a"=1,"b"=2)
  return(x)
  
}



