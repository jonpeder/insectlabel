#' Generates labels for insect specimens, and add UUIDs within QR codes
#' @param label_df dataframe, specifying label data, usually associated with collecting-event or determination. Each row should include data for each unique label.
#' @param n intiger vector, indicating the number of labels to be plotted for each row in the dataframe
#' @param filename a string specifying the output PDF file name
#' @param x an integer specifying number of rows of labeles on each page
#' @param y an integer specifying number of columns of labels on each page
#' @param text_order a list of integers, and/or vector of integers, specifying which data to include in what order. Each element of the list represent one label-line, chronologically ordered. E.g. list(c(1, 2),c(3,4),c(5,6), 7, 8, 9), where 1 is first column of input data, 2 is second etc.
#' @param QR_data an integer or vector of integers specifying what data to store in QR-codes, where 1 is UUID, 2 is first column of input data, 3 is second etc. E.g. c(1,2,8)
#' @param family a string, or a vector of strings, specifying one of the default font families (or other PostScript emulator fonts). A vector have to correspond in length to the number of label text-lines
#' @param fontsize a numerical value, or a vecotor of numerical values, specifying font size. A vector have to correspond in length to the number of label text-lines
#' @param font an integer, or a vector of integers, specifying font type of each label line, where 1 is  plain, 2  bold, 3 italic and 4 is bold-italic. A vector have to correspond in length to the number of label text-lines
#' @param linedist a numerical value specifying line distance
#' @param tx a numerical value specifying text offset on the x plane
#' @param ty a numerical value specifying text offset on the y plane
#' @param QRd a numerical value specifying distance between QR-codes (This will also determine QR code size)
#' @param QRx a numerical value specifying QR code offset on the x plane
#' @param QRy a numerical value specifying QR code offset on the y plane
#' @param qrlevel an integer value between 0 and 3  specifying the QR error correction level
#' @param delim a string specifying a delimiter between data segments in the QR codes (if more than one)
#' @param width a numerical value specifying PDF width (inches)
#' @param height a numerical value specifying the PDF height (inches)
#' @examples example_input <- data.frame(
#' @examples            col_ID = c("JPL_001", "JPL_002", "JPL_003"),
#' @examples            country = c("Norway,", "Norway,", "Norway,"),
#' @examples            county = c("Nordland,", "Troms,", "Troms,"),
#' @examples            municipality = c("Grane:", "Tromsø:", "Målselv:"),
#' @examples            locality1 =c("Auster Vefsna", "Tønsvik", "Dividalen"),
#' @examples            locality2 = c("Stilleelva", "Tverrelva", "Kvennelva"),
#' @examples            lat = c(65.532188, 69.736644, 68.792052),
#' @examples            lon = c(13.728252, 19.178893, 19.695974),
#' @examples            habitat = c("River-shore", "River-shore", "River-shore"),
#' @examples            method = c("Sweep-net:", "Sweep-net:", "Sweep-net:"),
#' @examples            date = c("2018-07-29", "2018-08-01", "2018-08-10"),
#' @examples            leg = c("Leg. J. Kjærandsen", "Leg. J.P.Lindemann", "Leg. J.P.Lindemann")
#' @examples            )
#' @examples insectlabel(
#' @examples            label_df = example_input,
#' @examples            n = c(20,15,25),
#' @examples            filename = "example_labels.pdf",
#' @examples            text_order = list(c(2, 3, 4), c(5, 6), c(7, 8), c(9), c(10, 11), 12),
#' @examples            x = 55,
#' @examples            y = 15,
#' @examples            QR_data = c(2,1),
#' @examples            fontsize = 3.3,
#' @examples            linedist = 0.95,
#' @examples            tx = 11,
#' @examples            ty = -1,
#' @examples            QRd = 4,
#' @examples            QRx =  4.4,
#' @examples            )
#' @import raster
#' @import stringr
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par
#' @importFrom utils write.table
#' @importFrom uuid UUIDgenerate
#' @importFrom qrencoder qrencode_raw
#' @export

insectlabel <- function (label_df = NULL, n = 1, filename = "insectlabels.pdf", text_order = NULL, x = 30, y = 8, QR_data = 1,
                         fontsize = 4, linedist = 1, family = "sans", font = 0, tx = 12, ty = 0, QRd = 4,
                         QRx = 4, QRy = 0, delim = ";", width = 8.27, height = 11.69, qrlevel = 3
                         ){
  if (is.null(label_df)) {
    stop("ERROR: Data (label_df) not specified")
  }
  if (is.null(text_order)) {
    stop("Label text order (text_order) not specified")
  }
  # Open pdf handle
  pdf(file = filename, width = width, height = height)
  # Remove spaces after strings in dataset
  #label_df <- sapply(label_df, stringr::str_trim)
  # Multiply each row in input data by the corresponding number in column 1
  label_df <- multirows(label_df, n)
  # Add UUIDs to dataset
  label_df <- data.frame(uuid = uuid::UUIDgenerate(n = nrow(label_df)), label_df)
  # Create subsets of label-data to be plotted on each separate page. Add subsets to a list.
  label_list <- subsetdata(label_df, x*y)
  # Loop over each subset in the table
  for (i in label_list){
    label_subset <- i
    # Data will be included in QR codes according to if statements.
    # Do not concatenate any cells
    if (ncell(QR_data) == 1){
      qr_df <- label_subset[,QR_data[1]]
    }
    # Concatenate data to be stored in QR-codes, and add to new dataframe
    if (ncell(QR_data) > 1){
      qr_df <- label_subset[,QR_data[1]]
      for (i in 2:(length(QR_data))) {
        qr_df <- paste(qr_df, label_subset[,QR_data[i]], sep = delim)
      }
    }
    # Plot text and eventually QR codes. Divide page into x rows and y columns.
    par(mar=c(QRd/y,0,(QRd+QRy)/y,(QRx*10)/y), mfrow = c(x, y))
    # Loop over each row in the subset
    for (i in 1:nrow(label_subset)){
      # Encode QR-codes
      qr_temp <- qrencode_raw(qr_df[i], version = 0L, level = qrlevel, hint = 2L, caseinsensitive = 1L)
      # Plot QR-codes
      image(qr_temp, asp=1, col=c("white", "black"), axes=FALSE, xlab="", ylab="")
      # Add label-text
      labeltext(x = label_subset[i,2:ncol(label_subset)], text_lines = text_order, y = fontsize/y , z= linedist+(linedist*0.05*(2.26^(QRd-2))), oy = ty, ox = tx/10, f = family, b = font)
    }
  }
  dev.off()
}
