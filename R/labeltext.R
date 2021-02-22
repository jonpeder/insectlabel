# Jon Peder Lindemann, 2019-08-05
# adds text to a "label plot", and controls the text parameters.
# Parameters:
# x | a dataframe with a single row of label data
# text_lines | a list of integers, and/or vectors of integers, indicationg what objects in the input dataframe (x) that will be plotted, and in what order.
# y | a numerical value specifying the font size
# z | a numerica value specifying the distance between the upper and lower text-line
# ox | a numerical value specifying x-offset
# oy | a numerical value specifying y-offset
# f | a string value specifying one of the default font families, or other PostScript emulator fonts

labeltext <- function(x = NULL, text_lines = NULL, y = NULL, z = NULL, ox = NULL, oy = NULL, f = NULL, b = NULL){

  # Collapse label objects that are specified to be on same text lines
  subv1 <- x[1,]
  endv <- c()
  for (i in 1:ncell(text_lines)){
    subv2 <- paste(subv1[c(text_lines[[i]])], collapse = " ")
    endv[i] <- subv2
  }

  x <- NULL
  x <- as.data.frame(rbind(endv))
  # Calculate the distance between each row of text (x.dist), and the position of the lowest row (x.lim)
  x.dist <- z/(ncol(x)-1)
  x.lim <- 0.5-(z/2)
  # Add offset to x.lim
  x.lim <- x.lim + (0.01*oy)
  # Assign list variable with number 1 to six (number of text rows)
  x.list <- list(1:ncol(x))
  # Iterate through the list and add calculated distance measures
  for (i in x.list){
    x.list[ncol(x)-(i-1)] <- x.lim + (x.dist*(i-1))
  }

  # Function to repete font (b), family (f) and cex (y), if only a single value is specified
  if (length(b) == 1) {
    b <- rep(b, ncol(x))
  }
  if (length(f) == 1) {
    f <- rep(f, ncol(x))
  }
  if (length(y) == 1) {
    y <- rep(y, ncol(x))
  }

  # The text function is used to add text to plot. Use the distance measures from the list to position each row.
  for (i in 1:ncol(x)){
    text(ox, x.list[i], adj = c(0,0.5), cex = y[i], labels = x[1,i], xpd=NA, font = b[i], family = f[i])
  }
}
