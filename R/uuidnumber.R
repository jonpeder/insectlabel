# Jon Peder Lindemann, 2019-08-05
# produces a given number (x) of UUIDs. The product is a data frame of UUIDs.
# Parameters:
# x | an integer value specifying the number of output UUIDs


uuidnumber <- function(x){
  matrix_uuid <- data.frame()
  matrix_uuid <- UUIDgenerate()
  for (i in 1:(x-1)){
    matrix_uuid <- rbind(matrix_uuid, UUIDgenerate())
  }
  matrix_uuid
}
