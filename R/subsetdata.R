# Jon Peder Lindemann, 2019-08-05
# divides a dataset (x) in to a list of subsets of a given number(y). The output is a list of dataframes.
# Parameters:
# x | a dataframe
# y | an integer value specifying the number of subsets to split input data (x) into

subsetdata <- function(x,y){
  # List-variable
  endv <- list()
  # Subset-variable
  subv <- data.frame()
  # List count. Index to count every time a new subset is added to the list
  index_list <- 1
  # Subset count. Index to count every iteration until reaching y
  index1 <- 1
  # Total count. Index to count every iteration until the end of the dataset
  index2 <- 1
  # Iterate through number of rows in the dataset
  for (i in 1:nrow(x)){
    # For each iteration, add the current row from dataset to Subset-variable
    subv <- rbind(subv,x[i,])
    index1 <- index1 + 1
    index2 <- index2 + 1
    # When reching the end of the last iteration, add the last subset to the list
    if (index2 == (nrow(x)+1)){
      endv[[index_list]] <- subv
    }
    # When reaching y, add subset to list and start on a new subset (Subset-variable and subset count are reset).
    else if (index1 > y){
      endv[[index_list]] <- subv
      subv <- data.frame()
      subv <- subv[1,]
      index1 <- 1
      index_list <- index_list + 1
    }
    # Continue to next iteration
    else {
      next
    }
  }
  # Return list of subsets (pruduct)
  endv
}
