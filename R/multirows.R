# Jon Peder Lindemann, 2019-08-08
# multiplies rows in an input dataframe
# Parameters:
# x | a dataframe with first column consisting of integer values specifying number copies to be created of each corresponding row (1 creates no copies, 2 creates 1 copy, 3 creates 2 copies, etc.)

multirows <- function(x){
  y <- x[1,]
  for (i in 1:nrow(x)) {
    for (t in 1:x[i,1]) {
      y <- rbind(y, x[i,])
    }
  }

  y <- y[-c(1),]
  y <- y[,-c(1)]
  y
}
