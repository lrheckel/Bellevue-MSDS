# libraries to use
#library(ggplot2)
# library(readxl)
#library(DataExplorer)
# library(ggm)
library(readr)
library(dplyr)
# # library(stringr)
# # library(psych)
# # library(janitor)
# library(car)
# library(QuantPsyc)
# # library(pastecs)
# # library(sqldf)
#library(esquisse)
#library(FNN)
# library(caret)
# # library(class)
# library(factoextra)
# library(purrr)
# library(farff)
# library(MLmetrics)

# options(scipen=100)
# options(digits=5)

# read the data file
wordsFile <- read_delim("en_full.txt", delim=" ", col_names = FALSE)
# set the column names
names(wordsFile)[1] <- "word"
names(wordsFile)[2] <- "count"
glimpse(wordsFile)

# add column for word probability
pctWord <- wordsFile$count / sum(wordsFile$count) #this gives the % occurence

wordsFile <- cbind(wordsFile, pctWord)
glimpse(wordsFile)

#create a vector of the ordered words
wordList <- wordsFile$word

#This code is in the article, but the word file already comes to us like this
# # Make the text lowercase and split it up creating a huge vector of word tokens.
# split_text <- strsplit(tolower(raw_text), "[^a-z]+")
# # Count the number of different type of words.
# word_count <- table(split_text)
# # Sort the words and create an ordered vector with the most common type of words first.
# sorted_words <- names(sort(word_count, decreasing = TRUE))

rightWord <- function(inputWord) {
  # Calculate the edit distance between the word and all other words in the word file list.
  editDist <- adist(inputWord, wordList)
  editWordDist <- c(wordList, editDist)
  write(editWordDist, file = "data.txt",
        ncolumns = 2,
        append = FALSE, sep = " ")
  
  #glimpse(editDist)
  # Calculate the minimum edit distance to find a word that exists in the word list
  # with a limit of two edits.
  minEditDist <- min(editDist, 2)
  #glimpse(minEditDist)
  #print("a")
  # Generate a vector with all words with this minimum edit distance.
  # Since wordList is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposalsByProb <- c(wordList[editDist <= min(editDist, 2)])
  print(proposalsByProb)
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposalsByProb <- c(proposalsByProb, inputWord)
  #print("2")
  #print(proposalsByProb)
  # ... and return the first / most probable word in the vector.
  c(proposalsByProb[1], proposalsByProb[2], proposalsByProb[3])
}
# theWords <- rightWord("colum")
# theWords
# theWords <- rightWord("heirarchy")
# theWords
# theWords <- rightWord("knowlege")
# theWords
# theWords <- rightWord("adres")
# theWords
# theWords <- rightWord("papr")
# theWords
# theWords <- rightWord("laptoop")
# theWords
theWords <- rightWord("sceince")
theWords





# good above here


#this crashes the laptop out of memory
# totWordCount <- sum(wordsFile$count)
# fileLength <- nrow(wordsFile)

# wordCountVector <- wordsFile$count
# succRateEst <- c(1:fileLength)
# for (i in 1:fileLength)
# {
# succRateEst[i] <- binom.test(wordCountVector[i], totWordCount, pctWord[i])$estimate
# 
# }

