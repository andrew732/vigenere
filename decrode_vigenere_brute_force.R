# decode_vigenere_brute_force.R
# decode with brute force

message <- "coincidences in general are great stumbling blocks in the way of that class of thinkers who have been educated to know nothing of the theory of probabilities that theory to which the most glorious objects of human research are indebted for the most glorious of illustrations edgar allen poe the murders in the rue morgue morpheus this is your last chance after this there is no turning back you take the blue pill the story ends you wake up in your bed and believe whatever you want to believe you take the red pill you stay in wonderland and i show you how deep the rabbit hole goes"
codeword <- "lem"

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

# instantiate a map to hold previously computed codes' scores
map <- new.env(hash=T, parent=emptyenv())

# we begin with a basic (a->a, z->z) function for decrypting the codemess
curFunc <- strrep("a",3)

# calculate the score for curFunc and store it in the map
oldScore <- score(decrypt(codemess,curFunc))

bestLetter1 <- 1
bestLetter2 <- 1
bestLetter3 <- 1

start_time <- Sys.time()
for(test1 in 1:26){
  for(test2 in 1:26){
    for(test3 in 1:26){
      substr(curFunc,1,1) <- rawToChar(as.raw(test1 + 96))
      substr(curFunc,2,2) <- rawToChar(as.raw(test2 + 96))
      substr(curFunc,3,3) <- rawToChar(as.raw(test3 + 96))
      
      newScore <- score (decrypt(codemess,curFunc))
      if (newScore > oldScore){
        oldScore <- newScore
        bestLetter1 <- test1
        bestLetter2 <- test2
        bestLetter3 <- test3
      }
    }
  }
}

substr(curFunc,1,1) <- rawToChar(as.raw(bestLetter1 + 96))
substr(curFunc,2,2) <- rawToChar(as.raw(bestLetter2 + 96))
substr(curFunc,3,3) <- rawToChar(as.raw(bestLetter3 + 96))

if(decrypt(codemess,curFunc) == message)
{
  print(Sys.time() - start_time)
}