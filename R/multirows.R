# multirows, Jon Peder Lindemann, 23.02.2020
# multiply rows in data frame
# Parameters:
# x | dataframe
# n | integer or vector of integers specifying number/numbers each row should be multiplied by

multirows <- function(x, n) {
  if (length(n) == 1) {
    n <- rep(n, nrow(x))
  }
  df_out <- NULL
  for (i in 1:nrow(x)) {
    for (t in 1:n[i]) {
      df_out <- rbind(df_out, x[i,])
    }
  }
  df_out
}
