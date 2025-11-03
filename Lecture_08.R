rm(list=ls()) #clean, clc, close all
# ***************
# R version 4.4.2 / win
# author: Nikola Jordánová
# *************

# Path
setwd('V:/MPA-PRG/exercise_08') # set working directory

library(Biostrings)

# Task 1
FindSorted <- function(vector){
  
  for (i in 1:length(vector)){
    if (vector[i] != (i - 1)) {
      return(i)
    }
  }
  return(length(vector))
}


FindSorted(c(0,1,2,4,3,5,6))

# Task 2
IndicateAscending <- function(vector){
  descend_ascend <- numeric(length(vector))
  descend_ascend[1] <- 1
  descend_ascend[length(vector)] <- 1
  
  for (i in 2:(length(vector)-1)){
    if (vector[i] == vector[i-1] + 1){
      descend_ascend[i] <- 1
      descend_ascend[i-1] <- 1
    }
  
  }
  
  return(descend_ascend)
  
}

IndicateAscending(c(0,5,8,7,1,2,4,6,3,9))

# Task 3
BreakpointSort <- function(vector){
  
  vector <- c(0, vector, length(vector) + 1)
  
  start <- FindSorted(vector)
  
  while (start < length(vector)) {
    mark <- IndicateAscending(vector)
    
    descending_values <- vector[mark == 0]
    
    if (length(descending_values) == 0) {
      break
    }
    
    min_desc_value <- min(descending_values)
    end <- which(vector == min_desc_value)[1]
    
    
    if (is.na(end) || end <= start) break
    
    vector[start:end] <- rev(vector[start:end])
    
    start <- FindSorted(vector)
  }
  
  sorted <- vector[-c(1, length(vector))]
  return(sorted)
  
}


BreakpointSort(c(5,8,7,1,2,4,6,3))
