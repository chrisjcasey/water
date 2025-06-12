#' Draw Drowning Funnel Plot
#'
#' Creates a funnel-style plot of drowning data across regions and funnel stages.
#'
#' @param pi Page index number (integer).
#' @param path File path for the PDF output.
#' @param df Data frame of drowning records.
#' @param title Title text
#' @param subtitle Subtitle text
#' @param groupVar Name of the variable for grouping (e.g., "RegionDesc1").
#' @param funnelLabels Character vector of 3 filter names.
#' @param funnelVars Character vector of 3 variables to filter progressively.
#' @param funnelVals List of values (same length as funnelVars) used for filtering.
#' @param colours Named list of colours, including `highlight`, `text1`, `text2`, `hitext`, `borders`.
#' @param years Numeric vector of years to include.
#' @param groupGap Numeric, vertical gap between groups in units of row height (e.g., 0.5 = half a row height).
#' @param DEBUG Logical flag for debug mode.
#'
#' @export
DrawDrowningFunnel <- function(
    pi,
    path,
    df,
    title,
    subtitle,
    groupVar,
    funnelLabels,
    funnelVars,
    funnelVals,
    colours = list(highlight = "#BF0000"),
    years = 2000:2024,
    groupGap = 0,
    DEBUG = FALSE
) {

  textcols=c(colours$text1,colours$text1, colours$text2,colours$hitext)
  PlotBasePage(path = path, pagesize = "A3", orientation = "portrait", DEBUG = DEBUG)
  PlotPageHeader(pi, colT = colours$text1)

  groups = unique(df[[groupVar]])
  if(groupVar=="RegionDesc1")groups=c("Northland","Auckland","Gisborne","Bay of Plenty",
    "Waikato","Hawke's Bay","Taranaki","Manawatu-Wanganui","Wellington","Tasman","Marlborough",
    "Canterbury","West Coast","Otago","Southland",
    "Oceanic & Islands")
  nGroups = length(groups)
  nYears = length(years)

  headerLines = 4
  nVars = length(funnelVars) + 1
  totalRows = nGroups * (nVars + groupGap) + headerLines
  ygap = 2 / totalRows

  xgap = 1 / (nYears+2)

  leftadj=0.11


  text(0.00,2,title,font=2,col=colours$text1,adj=0,cex=0.90)
  text(0.00,1.975,subtitle,font=3,col=colours$text1,adj=0,cex=0.90)

  for (g in seq_along(groups)) {
    region = groups[g]
    datRegion = df[df[[groupVar]] == region, ]
    for (i in seq_along(years)) {
      yr = years[i]
      datYr = datRegion[datRegion$Year == yr, ]
      y0 = 2 - (g - 1) * 4 * ygap - (g - 1) * groupGap * ygap - headerLines *ygap
      x0 = (i - 1) * xgap + leftadj
      counts = rep(NA, 4)
      counts[1] = nrow(datYr)
      for (j in 1:3) {
        datYr = datYr[datYr[[funnelVars[j]]] %in% funnelVals[[j]], ]
        counts[j + 1] = nrow(datYr)
      }
      for (j in 1:4) {
        yy = y0 - (j - 1) * ygap

        rect(x0, yy - ygap, x0 + xgap, yy,
             border = colours$borders , lwd = 1)

        if(counts[j])
        {
          fontval = if (j == 1) 2 else 1  # 2 = bold, 1 = normal
          textcol = textcols[j]

          rect(x0, yy - ygap, x0 + xgap, yy,
               col = ifelse(j == 4, colours$highlight1, "white"),
               border = colours$borders , lwd = 1)

          text(x0 + xgap/2, yy - ygap/2, labels = counts[j],
               col = textcol, cex = 1, font = fontval)
        }
      }
    }
    # Draw region label to the left

    yy = y0

    rect(-0.10, yy - ygap, -0.01+leftadj, yy,
          col = colours$highlight2, border=NA, lwd = 1)

    text(-0.01+leftadj-0.005,  yy - ygap/2,
         labels = region, col= colours$hitext, xpd = TRUE, cex = 0.75,font=2, adj=1)

    for (vi in 1:nVars)
    {
      yy = y0 - (vi) * ygap

      text(-0.01+leftadj,  yy - ygap/2,
                   labels = funnelLabels[vi], xpd = TRUE, cex = 0.85,font=1, adj=1)
    }
  }

  # Draw year labels at top
  for (i in seq_along(years)) {
    x0 = (i - 1) * xgap
    rect(x0+leftadj, 2 + 0.002- headerLines *ygap, x0 + xgap+leftadj, 2 + 0.04 - headerLines *ygap, col = "black", border = NA)
    text(x0 + xgap/2+leftadj, 2 + 0.02- headerLines *ygap, labels = years[i], adj = c(0.5, 0.5), col = "white", cex = 0.7, xpd = TRUE,font=2)
  }

  PlotPageFooter()
}
