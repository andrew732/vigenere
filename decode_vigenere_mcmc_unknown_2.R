message <- "coincidences in general are great stumbling blocks in the way of that class of thinkers who have been educated to know nothing of the theory of probabilities that theory to which the most glorious objects of human research are indebted for the most glorious of illustrations edgar allen poe the murders in the rue morgue morpheus this is your last chance after this there is no turning back you take the blue pill the story ends you wake up in your bed and believe whatever you want to believe you take the red pill you stay in wonderland and i show you how deep the rabbit hole goes"
codeword <- "lemon"
message <- gsub(" ", "", message, fixed = TRUE)

setwd('/Users/andrewhuang/Documents/Sophomore2018/CIS519/STAT433FinalProject')
mat <- read.table("AustenCount.txt",header=F)
logmat <- log(mat + 1)

# Computes the score of the decoded message using the given code
score <- function(code)
{  
  p <- 0
  for (i in 1:(nchar(message)-1)){
    p <- p + logmat[charIndex(substr(code, i, i)),charIndex(substr(code, i+1, i+1))]
  }
  p
}

# ascii(char) returns the numerical ascii value for char
ascii <- function(char)
{ 
  strtoi(charToRaw(char),16L) #get 'raw' ascii value
} 

# charIndex takes in a character and returns its 'char value'
charIndex <- function(char)
{
  aValue <- ascii(char)
  if (aValue == 32)
  {
    27
  } else
  {
    aValue - 96 
  }
}

# Encrypts code according to codeword
encrypt <- function(code, codeword)
{
  out <- code
  index <- 1
  # for each character in the message, encode it according to the codeword
  for (i in 1:nchar(message))
  {
    charInd <- charIndex(substr(code,i,i))
    codewordInd <- charIndex(substr(codeword,index,index))
    if (charInd < 27)
    {
      # change the ith character to the character determined by the codeword
      substr(out,i,i) <- rawToChar(as.raw(((charInd + codewordInd - 2) %% 26 + 97)))
      index <- index + 1
      if (index > nchar(codeword)){
        index = 1
      }
    }
  }
  out
}

# Decrypts code according to curFunc	
decrypt <- function(code, testword)
{  	
  out <- code
  index <- 1
  # for each character in the message, decode it according to the testword
  for (i in 1:nchar(message))
  {
    charInd <- charIndex(substr(code,i,i))
    codewordInd <- charIndex(substr(testword,index,index))
    if (charInd < 27)
    {
      # change the ith character to the character determined by the testword
      substr(out,i,i) <- rawToChar(as.raw(((charInd - codewordInd) %% 26 + 97)))
      index <- index + 1
      if (index > nchar(testword)){
        index = 1
      }
    }
  }
  out
}

# codemess holds the scrambled message
codemess <- encrypt(message, codeword)

# calculate factors
start_time <- Sys.time()
factorList <- c()
index <- 1
threeLetterMap <- new.env(hash=T, parent=emptyenv())
codemessNoSpace <- gsub(" ", "", codemess, fixed = TRUE)

# find repeated strings of length 3
for (i in 1:nchar(codemessNoSpace))
{
  if(i < nchar(codemessNoSpace)-1){
    seq <- substr(codemessNoSpace,i,i+2)
    if (exists(paste(seq, collapse =''), threeLetterMap)){
      seen <- threeLetterMap[[paste(seq, collapse ='')]]
      threeLetterMap[[paste(seq, collapse = '')]] <- seen+1
    } else
    {
      threeLetterMap[[paste(seq, collapse = '')]] <- 1
    }
  }
}

for (item in ls(threeLetterMap)) {
  if(threeLetterMap[[item]] > 1){
    factorList[index] <- item
    index <- index + 1
  }
}

# find the indexes of repeated strings of length 3
threeLetterMap <- new.env(hash=T, parent=emptyenv())
for (i in 1:nchar(codemessNoSpace))
{
  if(i < nchar(codemessNoSpace)-1){
    seq <- substr(codemessNoSpace,i,i+2)
    if (seq %in% factorList){
      if (exists(paste(seq, collapse =''), threeLetterMap)){
        list <- threeLetterMap[[paste(seq, collapse ='')]]
        threeLetterMap[[paste(seq, collapse = '')]] <- c(list, i)
      } else
      {
        threeLetterMap[[paste(seq, collapse = '')]] <- c(i)
      }
    }
  }
}

# find the difference in indices
factorList <- c()
index <- 1
for (item in ls(threeLetterMap)) {
  previousElement <- 0
  for(element in threeLetterMap[[item]]){
    if(previousElement != 0){
      factorList[index] <- element - previousElement
      index <- index + 1
    }
    previousElement <- element
  }
}

# Factor an integer x; code for this function only is obtained from link below
# https://stackoverflow.com/questions/6424856/r-function-for-returning-all-factors
factor <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  factors <- list(neg = -factors, pos = factors)
  return(factors)
}

# find the intersection of their factors
factors <- c()
for (item in factorList) {
  if(length(factors) != 0){
    factors <- c(factors, factor(item)$pos)
  } else {
    factors <- factor(item)$pos
  }
}
numberOfOnes <- sort(table(factors),decreasing=TRUE)[2]
factors <- as.numeric(names(which(sort(table(factors),decreasing=TRUE) > numberOfOnes/3)))

# print time to process factors
print(Sys.time() - start_time)

# calculate the score for curFunc and store it in the map
curFunc <- "a"
oldScore <- score(decrypt(codemess,curFunc))
bestWord <- "a"
bestScore <- oldScore

for(i in sort(factors, decreasing=FALSE)){
  map <- new.env(hash=T, parent=emptyenv())
  curFunc <- strrep("a",i)
  oldScore <- score(decrypt(codemess,curFunc))
  for(iteration in 1:(150*i)){
    # sample two letters to swap
    oldFunc <- curFunc
    swap <- sample(1:26,1)
    index <- sample(1:nchar(curFunc), 1)
    substr(curFunc,index,index) <- rawToChar(as.raw(swap + 96))
    
    # if we have already scored this decoding,
    # retrieve score from our map
    if (exists(paste(curFunc, collapse =''), map)){
      newScore <- map[[paste(curFunc, collapse ='')]]
    } else
    {
      newScore <- score (decrypt(codemess,curFunc))
      map[[paste(curFunc, collapse = '')]] <- newScore
    }
    
    # decide whether to accept curFunc or to revert to oldFunc
    if (runif(1) > exp(newScore-oldScore))
    {
      curFunc <- oldFunc
    } else 
    {
      oldScore <- newScore
    }
  }
  if(bestScore*(1.2) < oldScore){
    print(c(i,decrypt(codemess,curFunc)))
    print(Sys.time() - start_time)
    print(curFunc)
    print(oldScore)
    break
  } else if(bestScore < oldScore){
    bestScore <- oldScore
    bestWord <- curFunc
  }
}