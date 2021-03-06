# decode_vigenere_mcmc.R
# decode with mcmc

message <- "coincidences in general are great stumbling blocks in the way of that class of thinkers who have been educated to know nothing of the theory of probabilities that theory to which the most glorious objects of human research are indebted for the most glorious of illustrations edgar allen poe the murders in the rue morgue morpheus this is your last chance after this there is no turning back you take the blue pill the story ends you wake up in your bed and believe whatever you want to believe you take the red pill you stay in wonderland and i show you how deep the rabbit hole goes"
codeword <- "lemon"

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
curFunc <- strrep("a",nchar(codeword))

# calculate the score for curFunc and store it in the map
oldScore <- score(decrypt(codemess,curFunc))
map[[paste(curFunc, collapse='')]] <- oldScore

start_time <- Sys.time()
for(iteration in 1:800){
  # sample two letters to swap
  oldFunc <- curFunc
  swap <- sample(1:26,1)
  index <- sample(1:nchar(codeword), 1)
  
  substr(curFunc,index,index) <- rawToChar(as.raw(swap + 96))
  
  # if we have already scored this decoding,
  # retrieve score from our map
  if (exists(paste(curFunc, collapse =''), map)){
    newScore <- map[[paste(curFunc, collapse ='')]]
  } else
    # if we have not already scored this decoding,
    # calculate it and store it in the map
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
  # print out time elapsed
  # if(decrypt(codemess,curFunc) == message)
  # {
  #   print(Sys.time() - start_time)
  #   print(c(iteration,decrypt(codemess,curFunc)))
  #   print(curFunc)
  #   break
  # }
}
if(decrypt(codemess,curFunc) == message)
{
  print(Sys.time() - start_time)
  print(c(iteration,decrypt(codemess,curFunc)))
  print(curFunc)
}