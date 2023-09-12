# a function to find a string in a csv file
find <- function(file_name, keyWord){ # inputs are a csv file and the chosen string

  setwd(".") # changes working directory to the user's for clarity
  f <- read.csv(file_name) # reads in given file as a csv
  in_matrix <- as.matrix(f) # transforms csv into a matrix for easier looping

  # sets a counter for instances of the key word and a vector to hold the instances
  counter <- 0
  vector <- c()

  # for each term in the matrix, compares to key word
  for(i in in_matrix){
     if(i == keyWord){ # increases counter and adds instance to vector if a match
       counter <- counter+1
       vector <- append(vector, i)
     }
  }

  # returns number of instances of the keyword and a csv with the instances
  print(paste0(keyWord, ": ", counter))
  write.csv(vector, paste0(keyWord,".csv"))
}
