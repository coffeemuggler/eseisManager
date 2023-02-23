#' Create a UID (unique identifier) for a seismic pool item.
#'
#' The function creates a UDI based on the current time stamp and
#' a 6 digit random number. This UID is supposed to be added to the
#' base data of each item.
#'
#' @param label \code{Logical} value, option to create PDF with device
#' information and QR code to be taped onto the registered item. Default
#' is \code{TRUE}
#'
#' @param path \code{Character} value, optional path to
#' PDF document with label. If omitted, the file will be created in the
#' current working directory.
#'
#' @param file \code{Character} value, optional file name of the PDF
#' document to create. If omitted, the ID will be used as file name.
#'
#' @param line1 \code{Character} value, content of first label line, should
#' be host institution name. Default is \code{"Uni Göttingen, Geogr. Inst."}.
#'
#' @param line2 \code{Character} value, content of second label line, should
#' be organisation unit. Default is \code{"Geophysical Device Pool"}.
#'
#' @param line3 \code{Character} value, content of third label line, should
#' be item name. If omitted, a dummy entry will be added:
#' \code{Item: unknown}.
#'
#' @param line4 \code{Character} value, content of fourth label line, should
#' be ID in plane text. If omitted, the ID will be added automatically.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#'
#' create_uid()
#'
#' }
#'
#' @export create_uid

create_uid <- function(

  label = TRUE,
  path,
  file,
  line1 = "Uni Goettingen, Geogr. Inst.",
  line2 = "Geophysical Device Pool",
  line3,
  line4

) {

  ## get current time stamp
  t <- format(Sys.time(),
              format = "%Y-%m-%d")

  ## generate random number
  ID <- as.character(round(stats::runif(n = 1, min = 1e6, max = 1e7)))

  ## truncate to 6 digit number
  ID <- substr(x = ID, start = 2, stop = 7)

  ## paste time and ID
  time_ID <- paste(t, ID, sep = "-")

  ## check if label PDF shall be generated
  if(label == TRUE) {

    ## generate QR code
    qr <-  qrcode::qr_code(x = ID)

    ## check/set path
    if(missing(path) == TRUE) {

      path <- getwd()
    }

    ## check/set file name
    if(missing(file) == TRUE) {

      file <- paste0(path, "/", time_ID, ".pdf")
    }

    ## open PDF device
    pdf(file = file, width = 2, height = 0.7, paper = "a4")

    ## set margins to minimum
    par(mar = c(0.1, 0.1, 0.1, 0.1))

    ## define 3-col layout
    layout(mat = t(c(1, 1, 2)))

    ## create empty plot to place text in
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)

    ## check/set third line
    if(missing(line3) == TRUE) {

      line3 <- "Item: unknown"
    }

    ## check/set fourth line
    if(missing(line4) == TRUE) {

      line4 <- paste("ID:", time_ID)
    }

    ## add first text line
    text(x = 0, y = 0.90, labels = line1, adj = c(0, 1), cex = 0.9)
    text(x = 0, y = 0.70, labels = line2, adj = c(0, 1), cex = 0.9)
    text(x = 0, y = 0.43, labels = line3, adj = c(0, 1), cex = 1)
    text(x = 0, y = 0.23, labels = line4, adj = c(0, 1))

    ## add QR code
    plot(qr)

    ## draw frame around label
    par(new = TRUE)
    layout(mat = t(c(1, 1, 1)))
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)
    box()

    ## close PDF device
    dev.off()
  }

  return(paste(t, ID, sep = "-"))
}

