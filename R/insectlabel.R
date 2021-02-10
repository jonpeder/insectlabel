#' Generates labels for insect specimens, and add UUIDs within QR codes
#' @param z an object of class 'dataframe', the first column consisting of intiger values, indicating the number of labels to be plotted for each row. This will usually be a table of collecting event data, or determination data.
#' @param x an integer specifying number of rows of labeles on each page
#' @param y an integer specifying number of columns of labels on each page
#' @param text a list of integers, and/or vector of integers, specifying which data to include in what order. Each element of the list represent one label-line, chronologically ordered. E.g. list(c(1, 2),c(3,4),c(5,6), 7, 8, 9), where 1 is first column of input data, 2 is second etc.
#' @param QR a value specifying wether or not to add QR codes, and what data to include. QR code absent (NULL), UUID only (1), or specifiy which data to include in specific order with a single column-number, or a list of column numbers, e.g. c(1,2,4,3,6,7), where 1 is UUID, 2 is first column of input data, 3 is second etc.
#' @param family a string specifying one of the default font families (or other PostScript emulator fonts)
#' @param fontsize a numerical value specifying font size
#' @param linedist a numerical value specifying line distance
#' @param tx a numerical value specifying text offset on the x plane
#' @param ty a numerical value specifying text offset on the y plane
#' @param QRd a numerical value specifying distance between QR-codes (This will also determine QR code size)
#' @param QRx a numerical value specifying QR code offset on the x plane
#' @param QRy a numerical value specifying QR code offset on the y plane
#' @param qrlevel an integer value between 0 and 3  specifying the QR error correction level
#' @param delim a string specifying a delimiter between data segments in the QR codes (if more than one)
#' @param file a string specifying the output PDF file name
#' @param width a numerical value specifying PDF width (inches)
#' @param height a numerical value specifying the PDF height (inches)
#' @param exp a logical value. If TRUE, a data matrix with UUIDs will be exported
#' @examples example_input <- data.frame(
#' @examples                label_copies = c(20,15,25),
#' @examples                col_ID = c("JPL_001", "JPL_002", "JPL_003"),
#' @examples                country = c("Norway,", "Norway,", "Norway,"),
#' @examples                county = c("Nordland,", "Troms,", "Troms,"),
#' @examples                municipality = c("Grane:", "Tromsø:", "Målselv:"),
#' @examples                locality1 =c("Auster Vefsna", "Tønsvik", "Dividalen"),
#' @examples                locality2 = c("Stilleelva", "Tverrelva", "Kvennelva"),
#' @examples                lat = c(65.532188, 69.736644, 68.792052),
#' @examples                lon = c(13.728252, 19.178893, 19.695974),
#' @examples                habitat = c("River-shore", "River-shore", "River-shore"),
#' @examples                method = c("Sweep-net:", "Sweep-net:", "Sweep-net:"),
#' @examples                date = c("2018-07-29", "2018-08-01", "2018-08-10"),
#' @examples                leg = c("Leg. J. Kjærandsen", "Leg. J.P.Lindemann", "Leg. J.P.Lindemann")
#' @examples                )
#' @examples insectlabel(
#' @examples                z = example_input,
#' @examples                text = list(c(2, 3, 4), c(5, 6), c(7, 8), c(9), c(10, 11), 12),
#' @examples                x = 55,
#' @examples                y = 15,
#' @examples                QR = c(2,1),
#' @examples                fontsize = 3.3,
#' @examples                linedist = 0.95,
#' @examples                tx = 11,
#' @examples                ty = -1,
#' @examples                QRd = 4,
#' @examples                QRx =  4.4,
#' @examples                file = "example_labels.pdf"
#' @examples                )
#' @import raster
#' @import stringr
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par
#' @importFrom utils write.table
#' @importFrom uuid UUIDgenerate
#' @importFrom qrencoder qrencode_raw
#' @export

insectlabel <- function (z = NULL, text = NULL, x = 30, y = 8, QR = 1, fontsize = 4, linedist = 1,
                       family = "Helvetica",tx = 12, ty = 0, QRd = 4, QRx = 4, QRy = 0, delim = ";",
                       file = "insect_labels.pdf", width = 8.27, height = 11.69, qrlevel = 3, exp = FALSE){
  if (is.null(z)) {
    print("Data not specified")
  }
  if (is.null(text)) {
    print("Number of rows in each label (text) not specified")
  }
  # Open pdf handle
  pdf(file = file, width = width, height = height)
  # Multiply each row in input data by the corresponding number in column 1
  label_data <- multirows(z)
  # Remove spaces after strings in dataset
  label_data <- sapply(label_data, stringr::str_trim)
  # Add UUIDs to dataset
  label_data <- as.data.frame(cbind(uuidnumber(nrow(label_data)), label_data), stringsAsFactors = FALSE)
  # Eventually export data with UUIDs
  if (exp == TRUE) {
    write.table(label_data, "uuid.txt", sep = ";", row.names = FALSE, quote = FALSE)
  }
  # Devide dataset into subsets in a list. The number of subsets will depend on number of labels on each sheat, so that each subset will be printed on a separate page.
  data_table <- subsetdata(as.data.frame(label_data, stringsAsFactors = FALSE), x*y)
  # Loop over each subset in the table
  for (i in data_table){
    subset_data <- i
    # Data will be included in QR codes according to if statements.
    # Do not concatenate any cells
    if (ncell(QR) == 1){
      qr_df <- subset_data[,QR[1]]
    }
    # Concatenate specified cells on each row, according to QR variable
    if (ncell(QR) > 1){
      qr_df <- subset_data[,QR[1]]
      for (i in 1:(length(QR)-1)) {
        qr_df <- paste(qr_df, subset_data[,QR[i+1]], sep = delim)
      }
    }
    # Plot text and eventually QR codes. Divide page into x rows and y columns.
    par(mar=c(QRd/y,0,(QRd+QRy)/y,(QRx*10)/y), mfrow = c(x, y))
    # Loop over each row in the subset
    for (i in 1:nrow(subset_data)){
      # If QR is not 0, plot QR codes
      if (sum(QR) != 0){
        qr_temp <- qrencode_raw(qr_df[i], version = 0L, level = qrlevel, hint = 2L, caseinsensitive = 1L)
        image(qr_temp, asp=1, col=c("white", "black"), axes=FALSE, xlab="", ylab="")
        # If QR is 0, plot only text
      } else {
        plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
      }
      # labeltext adds label text to a plot, and controls the text parameters.Input parameters are data (x), text size (y, default 0.3), and the line distance (z, default 1.5).
      labeltext(x = subset_data[i,2:ncol(subset_data)], text_lines = text, y = fontsize/y , z= linedist+(linedist*0.05*(2.26^(QRd-2))), oy = ty, ox = tx/10, f = family)
    }
  }
  dev.off()
}
