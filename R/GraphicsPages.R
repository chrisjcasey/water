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
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom graphics par rect text
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
  totalRows = (nGroups+1) * (nVars + groupGap) + headerLines
  ygap = 2 / totalRows
  xgap = 1 / (nYears + 3 + 1)  # +1 for total, +1 for gap
  leftadj=0.11

  text(0.00,1.975,title,font=2,col=colours$text1,adj=0,cex=0.90)
  text(0.00,1.950,subtitle,font=3,col=colours$text1,adj=0,cex=0.90)

  drawFunnelBlock <- function(datRegion, regionName, y0base, rectcol, highlight=FALSE) {
    for (i in seq_along(years)) {
      yr = years[i]
      datYr = datRegion[datRegion$Year == yr, ]
      x0 = (i - 1) * xgap + leftadj
      y0 = y0base

      counts = rep(NA, 4)
      counts[1] = nrow(datYr)
      for (j in 1:3) {
        datYr = datYr[datYr[[funnelVars[j]]] %in% funnelVals[[j]], ]
        counts[j + 1] = nrow(datYr)
      }
      for (j in 1:4) {
        yy = y0 - (j - 1) * ygap
        rect(x0, yy - ygap, x0 + xgap, yy, border = colours$borders , lwd = 1)
        if(counts[j]) {
          fontval = if (j == 1) 2 else 1
          textcol = textcols[j]
          rect(x0, yy - ygap, x0 + xgap, yy,
               col = ifelse(j == 4 && highlight, colours$highlight1, "white"),
               border = colours$borders , lwd = 1)
          text(x0 + xgap/2, yy - ygap/2, labels = counts[j],
               col = textcol, cex = 1, font = fontval)
        }
      }
    }

    # Region Totals
    x0 = length(years) * xgap +1/2*xgap + leftadj
    datTotal = datRegion
    counts = rep(NA, 4)
    counts[1] = nrow(datTotal)
    for (j in 1:3) {
      datTotal = datTotal[datTotal[[funnelVars[j]]] %in% funnelVals[[j]], ]
      counts[j + 1] = nrow(datTotal)
    }
    for (j in 1:4) {
      yy = y0base - (j - 1) * ygap
      rect(x0, yy - ygap, x0 + xgap, yy,
           col = ifelse(j == 4 && counts[j]>0 && highlight, colours$highlight1, "white"),
           border = colours$borders , lwd = 1)
      if(counts[j]) {
        textcol = textcols[j]
        text(x0 + xgap/2, yy - ygap/2, labels = counts[j], col = textcol, cex = 1, font = if (j == 1) 2 else 1)
      }
    }

    # Region name and funnel labels
    yy = y0base
    rect(-0.10, yy - ygap, -0.01+leftadj, yy, col = rectcol, border=NA, lwd = 1)
    text(-0.01+leftadj-0.005, yy - ygap/2, labels = regionName, col= colours$hitext, xpd = TRUE, cex = 0.75,font=2, adj=1)
    for (vi in 1:nVars) {
      yy = y0base - (vi) * ygap
      text(-0.01+leftadj,  yy - ygap/2, labels = funnelLabels[vi], xpd = TRUE, cex = 0.85,font=1, adj=1)
    }
  }

  for (g in seq_along(groups)) {
    region = groups[g]
    datRegion = df[df[[groupVar]] == region & df$Year %in% years, ]
    y0 = 2 - (g - 1) * 4 * ygap - (g - 1) * groupGap * ygap - headerLines *ygap
    if(DEBUG)cat(region,nrow(datRegion),"\n")
    drawFunnelBlock(datRegion, region, y0, colours$header, highlight=TRUE)
  }

  # Draw NZ Totals at bottom
  datNZ = df[df$Year %in% years, ]
  y0 = 2 - (nGroups) * 4 * ygap - (nGroups) * groupGap * ygap - headerLines *ygap - 4/2 * ygap
  x0 = (length(years) + 1/2) * xgap + leftadj
  yy = y0 - 3 * ygap
  #rect(x0-0.005, yy - ygap-.005, x0 + xgap+.005, yy+3*ygap+.005,col=paste0(colGray,"80"), border = colours$highlight, lwd = 3)
  drawFunnelBlock(datNZ, "New Zealand", y0, colours$text1, highlight=TRUE)

  # Add box around bottom-right NZ totals cell
  #x0 = (length(years) + 1/2) * xgap + leftadj
  #yy = y0 - 3 * ygap
  #rect(x0-0.005, yy - ygap-.005, x0 + xgap+.005, yy+3*ygap+.005,col=paste0(colGray,"20"), border = colours$highlight, lwd = 2)

  # Draw year labels
  for (i in seq_along(years)) {
    x0 = (i - 1) * xgap + leftadj
    rect(x0, 2-4*ygap, x0+xgap, 2-3*ygap, col = "black", border = NA)
    text(x0 + xgap/2, 2-7/2*ygap, labels = years[i], adj = c(0.5, 0.5), col = "white", cex = 0.7, xpd = TRUE,font=2)
    rect(x0, y0 + ygap , x0 + xgap, y0, col = "black", border = NA)
    text(x0 + xgap/2,  y0+1/2*ygap, labels = years[i], adj = c(0.5, 0.5), col = "white", cex = 0.7, xpd = TRUE,font=2)
  }
  # Label for total column
  x0 = length(years)* xgap+ 1/2*xgap+leftadj
  rect(x0, 2-4*ygap, x0 + xgap, 2-3*ygap, col = "black", border = NA)
  text(x0 + xgap/2, 2-7/2*ygap, labels = "Total", adj = c(0.5, 0.5), col = "white", cex = 0.7, xpd = TRUE,font=2)

  x0 = length(years) * xgap +1/4*xgap + leftadj
  rect(x0, y0 - 4* ygap, x0, 2-3*ygap)

  rect(-0.1, y0 + 3/2* ygap, 1.03, y0 + 3/2* ygap)

  PlotPageFooter()
}

#' Draw Drowning 1 Var Split
#'
#' Creates a grid and XLS of Drownings by Ages
#'
#' @param pi Page index number (integer).
#' @param path File path for the PDF and XLS output.
#' @param df Data frame of drowning records.
#' @param title Title text
#' @param subtitle Subtitle text
#' @param groupVar Name of the variable for grouping (e.g., "AgeBand").
#' @param colours Named list of colours, including `highlight`, `text1`, `text2`, `hitext`, `borders`.
#' @param years Numeric vector of years to include.
#' @param groupGap Numeric, vertical gap between groups in units of row height (e.g., 0.5 = half a row height).
#' @param DEBUG Logical flag for debug mode.
#'
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom graphics par rect text
#' @export
DrawDrowning1VarSplit <- function(
    pi,
    path,
    df,
    title,
    subtitle,
    groupVar,
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
  if(groupVar=="AgeGroup")groups=c("00-04","05-14","15-24","25-34","35-44","45-54","55-64","65-74","75+")
  if(groupVar=="Age")groups=0:max(dfd[dfd$Year %in% years,]$Age,na.rm=T)

  s=paste0(groupVar,",",paste0(years,collapse=","),"\n")

  nGroups = length(groups)
  nYears = length(years)



  lineHeight = min(0.035,2/(nGroups+4))
cat("lineHeight:",lineHeight,"\n")
  textSc = (lineHeight/0.035)^(1/7)
  headerLines = 4
  nVars = 1
  totalLines = nGroups * nVars + groupGap*(nGroups+1) + headerLines

  colWidth = 1 / (nYears + 1 + 3)  # add margin
  leftadj=0.11

  text(0.00,1.965,title,font=2,col=colours$text1,adj=0,cex=0.90)
  text(0.00,1.965,subtitle,font=3,col=colours$text1,adj=0,cex=0.90)

  for(gi in seq_along(groups))
  {
    levelName = groups[gi]
    datLevel= df[df[[groupVar]] == levelName & df$Year %in% years, ]
    if(DEBUG)cat(levelName,nrow(datLevel),"\n")
    y0 = 2 -(lineHeight*((gi-1)*nVars+groupGap*(gi-1) + headerLines))
    s=paste0(s,levelName,",")
    for (yi in seq_along(years))
    {
      yr = years[yi]
      datY = datLevel[datLevel$Year == yr, ]
      x0 = (yi - 1) * colWidth + leftadj

      counts = nrow(datY)

      rect(x0, y0 - lineHeight, x0 + colWidth, y0, border = colours$borders , lwd = 1)
      #if(counts)
      {
        fontval = 1
        textcol = textcols[1]
        if(counts==0) textcol="White"
        rect(x0, y0 - lineHeight, x0 + colWidth, y0,
             col =  "white",
             border = colours$borders , lwd = 1)
        text(x0 + colWidth/2, y0 - lineHeight/2, labels = counts,
             col = textcol, cex = 1*textSc, font = fontval)

        s=paste0(s,counts,",")
      }
      #s=substr(s, 1, nchar(s)-1)


    }
    s=paste(s,"\n")
    #Level Totals
    x0 = length(years) * colWidth +1/2*colWidth + leftadj
    datTotal = datLevel
    counts = nrow(datTotal)


    rect(x0, y0 - lineHeight, x0 + colWidth, y0,
         col =  "white",
         border = colours$borders , lwd = 1)
    #if(counts)
    {
      textcol = textcols[1]
      text(x0 + colWidth/2, y0 - lineHeight/2, labels = counts, col = textcol, cex = 1*textSc, font = 2)
    }

    # level name
    txtWidth = max(strwidth(c("Total",unique(groups))))*1.5
    x0 =  leftadj-0.01
    rect(x0-txtWidth,   y0,             x0, y0-lineHeight, col=colours$header, border="white", lwd = 1)
    text(x0-txtWidth/2, y0-lineHeight/2, labels=levelName, col=colours$hitext, xpd = TRUE, cex = 1.0*textSc, font=2)
    #yy = y0 - lineHeight
    #text(-0.01+leftadj,  yy - lineHeight/2, labels = levelName, xpd = TRUE, cex = 0.85,font=1, adj=1)
  }
  # level name

  x0 =  leftadj-0.01
  y0=y0-3/2*lineHeight
  rect(x0-txtWidth,   y0,             x0, y0-lineHeight, col=colours$header, border="white", lwd = 1)
  text(x0-txtWidth/2, y0-lineHeight/2, labels="Total", col=colours$hitext, xpd = TRUE, cex = 1.0*textSc, font=2)

  # Draw NZ Totals at bottom
  for (yi in seq_along(years))
  {
    datY = df[df$Year %in% years[yi], ]
    x0 = (yi - 1) * colWidth + leftadj
    y0 = 2 -(lineHeight*((nGroups)*nVars+groupGap*(nGroups-1) + headerLines+ 1/2) )

    counts = nrow(datY)

    yy = y0
    rect(x0, yy - lineHeight, x0 + colWidth, yy, border = colours$borders , lwd = 1)
    #if(counts)
    {
      fontval = 1
      textcol = textcols[1]
      rect(x0, yy - lineHeight, x0 + colWidth, yy,
           col =  "white",
           border = colours$borders , lwd = 1)
      text(x0 + colWidth/2, yy - lineHeight/2, labels = counts,
           col = colours$text1, cex = 1*textSc, font = 2)
    }
  }

  datAll = df[df$Year %in% years, ]
  x0 = (length(years) +1/2) * colWidth + leftadj
  y0 = 2 -(lineHeight*((nGroups)*nVars+groupGap*(nGroups-1) + headerLines+ 1/2) )

  counts = nrow(datAll)

  yy = y0
  rect(x0, yy - lineHeight, x0 + colWidth, yy, border = colours$borders , lwd = 1)
  #if(counts)
  {
    rect(x0, yy - lineHeight, x0 + colWidth, yy,
         col =  "white",
         border = colours$borders , lwd = 1)
    text(x0 + colWidth/2, yy - lineHeight/2, labels = counts,
         col = colours$text2, cex = 1*textSc, font = 2)
  }

  # Add box around bottom-right NZ totals cell
  #x0 = (length(years) + 1/2) * colWidth + leftadj
  #yy = y0 - 3 * ygap
  #rect(x0-0.005, yy - ygap-.005, x0 + colWidth+.005, yy+3*ygap+.005,col=paste0(colGray,"20"), border = colours$highlight, lwd = 2)

  # Draw year labels
  for (i in seq_along(years)) {
    x0 = (i - 1) * colWidth + leftadj
    rect(x0, 2-4*lineHeight, x0+colWidth, 2-3*lineHeight, col = "black", border = NA)
    text(x0 + colWidth/2, 2-7/2*lineHeight, labels = years[i], adj = c(0.5, 0.5), col = "white", cex = 0.7*textSc, xpd = TRUE,font=2)
  }

  # Label for total column
  x0 = length(years)* colWidth+ 1/2*colWidth+leftadj
  rect(x0, 2-4*lineHeight, x0 + colWidth, 2-3*lineHeight, col = "black", border = NA)
  text(x0 + colWidth/2, 2-7/2*lineHeight, labels = "Total", adj = c(0.5, 0.5), col = "white", cex = 0.7*textSc, xpd = TRUE,font=2)


  # Label for total column
  x0 = length(years)* colWidth+ 1/2*colWidth+leftadj
  rect(x0, 2-4*lineHeight, x0 + colWidth, 2-3*lineHeight, col = "black", border = NA)
  text(x0 + colWidth/2, 2-7/2*lineHeight, labels = "Total", adj = c(0.5, 0.5), col = "white", cex = 0.7*textSc, xpd = TRUE,font=2)


  PlotPageFooter()

  write(s,file=paste0(groupVar,".csv"))
}
