#' Draw Drowning Funnel Plot
#'
#' Creates a funnel-style plot of drowning data across regions and funnel stages.
#'
#' @param pi Page index number (integer).
#' @param path File path for the PDF output.
#' @param df Data frame of drowning records.
#' @param groupVar Name of the variable for grouping (e.g., "RegionDesc1").
#' @param funnelVars Character vector of 3 variables to filter progressively.
#' @param funnelVals List of values (same length as funnelVars) used for filtering.
#' @param colours Named list of colours, including `highlight`.
#' @param years Numeric vector of years to include.
#' @param DEBUG Logical flag for debug mode.
#' @export
DrawDrowningFunnel <- function(
    pi,
    path,
    df,
    groupVar,
    funnelVars,
    funnelVals,
    colours = list(highlight = "#BF0000"),
    years = 2000:2024,
    DEBUG = FALSE
) {
  PlotBasePage(path = path, pagesize = "A3", orientation = "landscape", DEBUG = DEBUG)
  PlotPageHeader(pi, colT = colours$highlight)

  groups = unique(df[[groupVar]])
  nGroups = length(groups)
  nYears = length(years)

  xgap = 1 / nYears
  ygap = 2 / (nGroups * 4)

  for (g in seq_along(groups)) {
    region = groups[g]
    datRegion = df[df[[groupVar]] == region, ]
    for (i in seq_along(years)) {
      yr = years[i]
      datYr = datRegion[datRegion$Year == yr, ]
      y0 = 2 - (g - 1) * 4 * ygap
      x0 = (i - 1) * xgap
      counts = rep(NA, 4)
      counts[1] = nrow(datYr)
      for (j in 1:3) {
        datYr = datYr[datYr[[funnelVars[j]]] %in% funnelVals[[j]], ]
        counts[j + 1] = nrow(datYr)
      }
      for (j in 1:4) {
        yy = y0 - (j - 1) * ygap
        if(counts[j])
        {
          fontVal = if (j == 1) 2 else 1  # 2 = bold, 1 = normal

          rect(x0, yy - ygap, x0 + xgap, yy,
               col = ifelse(j == 4, colours$highlight, "white"),
               border = colours$borders , lwd = 1)

          text(x0 + xgap/2, yy - ygap/2, labels = counts[j],
               col = ifelse(j == 4, "white", "black"), cex = 0.7, font = fontVal)
        }
      }
    }
    # Draw region label to the left
    text(-0.01, 2 - (g - 1) * 4 * ygap - 2 * ygap,
         labels = region, adj = 1, xpd = TRUE, cex = 0.7)
  }

  # Draw year labels at top
  for (i in seq_along(years)) {
    x0 = (i - 1) * xgap
    rect(x0, 2 + 0.002, x0 + xgap, 2 + 0.04, col = "black", border = NA)
    text(x0 + xgap/2, 2 + 0.02, labels = years[i], adj = c(0.5, 0.5), col = "white", cex = 0.8, xpd = TRUE)
  }

  PlotPageFooter()
}
