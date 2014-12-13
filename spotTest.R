#README
######################
# this script is used to spot text new indices
# its not meant to be exhaustive or quantitative




source("mylib.R")

loaded<-load("predictors")


#test preprocessing
thisLine <- "'remove' ’remove’ there’s there's :'remove': :’remove’: ... “surrounded by double angle quotes” 'remove' ’remove’"
preProcessLine(thisLine)


#test predictions
inputString <- ""; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "\""; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "12 345 % @"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "1"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "the"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "a few at a "; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "something is"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "what the"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "do you want to "; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "let's"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "lets"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "their used to be"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "there used to "; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "i"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "am"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "all of "; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "shake the"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "go fuck"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "shit"; predictNextWord(inputString, cutoff=5, all=TRUE) #<---------very slow
inputString <- "all of the king's"; predictNextWord(inputString, cutoff=5, all=TRUE)

inputString <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"; predictNextWord(inputString, cutoff=5, all=TRUE) #the
inputString <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"; predictNextWord(inputString, cutoff=5, all=TRUE) #world - should be beer
inputString <- "Hey sunshine, can you follow me and make me the"; predictNextWord(inputString, cutoff=5, all=TRUE) #most should be happiest
inputString <- "Very early observations on the Bills game: Offense still struggling but the"; predictNextWord(inputString, cutoff=5, all=TRUE) #fact - should be defense
inputString <- "go on a romatic date at the"; predictNextWord(inputString, cutoff=5, all=TRUE) #next - should be beach
inputString <- "garage I'll dust them off and be on my"; predictNextWord(inputString, cutoff=5, all=TRUE) #own - should be way
inputString <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"; predictNextWord(inputString, cutoff=5, all=TRUE) #time - correct
inputString <- "wet hair out of his eyes with his little"; predictNextWord(inputString, cutoff=5, all=TRUE) #brother - should be fingers
inputString <- "Be grateful for the good times and keep the faith during the"; predictNextWord(inputString, cutoff=5, all=TRUE) #day - should be bad
inputString <- "If this isn't the cutest thing you've ever seen, then you must be"; predictNextWord(inputString, cutoff=5, all=TRUE) #a - should be insane
inputString <- "If this isn't the cutest thing you've ever seen, then you must be a"; predictNextWord(inputString, cutoff=5, all=TRUE) #a - should be insane
inputString <- "doesnt really make a "; predictNextWord(inputString, cutoff=5, all=TRUE) 
inputString <- "illion"; predictNextWord(inputString, cutoff=5, all=TRUE) 

inputString <- "hjgsf"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "hjgsf"; predictNextWord(inputString, cutoff=5, all=FALSE)
inputString <- "na"; predictNextWord(inputString, cutoff=5, all=TRUE)
inputString <- "illion"; predictNextWord(inputString, cutoff=5, all=TRUE)




