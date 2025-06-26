#' PlotBasePage
#'
#' Sets up a blank PDF canvas with given page size and orientation.
#'
#' @param path File path for output PDF
#' @param pagesize  Paper size ("A4" or "A3")
#' @param orientation "portrait" or "landscape"
#' @param DEBUG Logical; draw axes if TRUE and label page format
#' @export
PlotBasePage <- function(path = "",pagesize = "A4",orientation = "portrait",DEBUG = FALSE) {
  plotdate=Sys.Date()-1
  dims=switch(pagesize,
                 A4 = list(width = 8.27, height = 11.69),
                 A3 = list(width = 11.69, height = 16.54),
                 stop("Unsupported paper size"))

  if (orientation == "landscape") { dims = list(width = dims$height, height = dims$width)}

  cairo_pdf(filename = path,
            width = dims$width,
            height = dims$height,
            family = "Arial",
            bg = colWhite)


  par(mar = c(3, 2, 1, 2) + 0.02)
  plot(c(0, 1), c(0, 2), type = "n",
       xlim = c(0, 1), ylim = c(0, 2),
       axes = DEBUG, xlab = "", ylab = "",
       cex = 3, fg = colLtGray, col.axis = colGray)

  if (DEBUG) {
    label = sprintf("DEBUG MODE: %s %s", pagesize, tools::toTitleCase(orientation))
    text(0.5, 1.9, label, col = colGray, cex = 1.4)
  }
}

#' PlotPageHeader
#'
#' Draws the page header with the website and page number, adjusting for orientation.
#'
#' @param pi Page index (integer)
#' @param colT Colour for text (e.g., colGray)
#' @param paper Paper size ("A4" or "A3")
#' @param orientation "portrait" or "landscape"
#' @export
PlotPageHeader <- function(pi, colT, paper = "A4", orientation = "portrait") {
  dims = switch(paper,
                 A4 = c(width = 8.27, height = 11.69),
                 A3 = c(width = 11.69, height = 16.54),
                 stop("Unsupported paper size"))

  if (orientation == "landscape") {
    dims = rev(dims)
  }


  yTop = 2

  #if (pi %% 2 == 1) {
    text(0.00, yTop, "www.watersafetynz.org", col = paste0(colT,"80"), font = 1, cex = 0.95,adj=0)
  #}

  text(0.95, yTop, "P.", col = colT, font = 1, cex = 1, adj = c(1, 0.5))
  text(0.99, yTop, pi, col = colT, font = 2, cex = 1, adj = c(1, 0.5))
}

#' PlotPageFooter
#'
#' Finalises the current PDF graphics page by calling dev.off().
#' Intended to be used after all drawing is complete.
#'
#' @export
PlotPageFooter <- function() {
  dev.off()
}
