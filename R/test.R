devtools::document()   # Updates NAMESPACE + help files
devtools::load_all()   # Reloads your package into your session
#CacheData()
load("../Data/data.RData")

# DrawDrowningFunnel(
#   pi = 1,
#   path = "CraftLJ_U16_Funnel.pdf",
#   df = dfd,
#   title = "Breakdown of NZ Drownings 2000-2024 by Region, Filtering by (Craft), then (Craft without Lifejackets), then (Craft without Lifejackets and U16)",
#   subtitle = "e.g. Northland 2002: 14 drownings, 5 using craft, of which all 5 weren't wearing lifejackets, and 1 of those was U16",
#   groupVar = "RegionDesc1",
#   funnelLabels = c("Craft", "No Lifejackets", "Under16"),
#   funnelVars = c("DReportActivity", "LJ", "Under16"),
#   funnelVals = list("Craft", 0, TRUE),
#   colours = list(text1=colBlack, text2=colBlue, hitext = colWhite,
#                  header = colTeal, highlight1 = colBrRed,
#                  highlight2 = colTeal, borders = colDemiLtGray),
#   years = 2000:2025,
#   groupGap = 0.5,
#   DEBUG = F
# )

DrawDrowning1VarSplit(
  pi = 1,
  path = "AgeBands.pdf",
  df = dfd,
  title = "Breakdown of NZ Drownings 2000-2024 by Age Band",
  subtitle = "",
  groupVar = "AgeGroup",
  colours = list(text1=colBlack, text2=colBlue, hitext = colWhite,
                 header = colTeal, highlight1 = colBrRed,
                 highlight2 = colTeal, borders = colDemiLtGray),
  years = 2000:2024,
  groupGap = 0,
  DEBUG = F
)



DrawDrowning1VarSplit(
  pi = 2,
  path = "Ages.pdf",
  df = dfd,
  title = "Breakdown of NZ Drownings 2000-2024 by Age",
  subtitle = "",
  groupVar = "Age",
  colours = list(text1=colBlack, text2=colBlue, hitext = colWhite,
                 header = colTeal, highlight1 = colBrRed,
                 highlight2 = colTeal, borders = colDemiLtGray),
  years = 2000:2024,
  groupGap = 0,
  DEBUG = F
)

# funnelLabels = c("Craft", "NZ European", "Over34"),
# funnelVars = c("DReportActivity", "EthnicityDesc", "Over34"),
# funnelVals = list("Craft","NZ European" , TRUE),
shell.exec("Ages.pdf")
