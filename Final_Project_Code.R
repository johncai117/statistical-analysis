rm(list = ls())
library(e1071) # For svm
library(Rsafd)
library(keras)
library(dplyr)

## PRE-DEFINED USEFUL FUNCTIONS SECTION

#Thresholding function
threshold <- function(x){
  if (x >0.5){
    newVar <- 1
  }
  else {
    newVar <- 0
  }
  newVar #Return Value
}

#Function to compute accuracy
compute_accuracy <- function(vec1,vec2){
  count = 0
  if (length(vec1) != length(vec2)) {
    print("Warning: Length mismatch")
  }
  for (i in seq(1,length(vec1))){
    if (vec1[i] == vec2[i]){
      count = count + 1
    }
  }
  accuracy = count/ length(vec1)
  accuracy #Return accuracy
}


## START OF REST OF THE CODE
