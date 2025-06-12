devtools::document()   # Updates NAMESPACE + help files
devtools::load_all()   # Reloads your package into your session

DrawDrowningFunnel(
    pi = 1,
    path = "CraftLJ_U16_Funnel.pdf",
    df = dfd,
    groupVar = "RegionDesc1",
    funnelVars = c("DReportActivity", "LJ", "Under16"),
    funnelVals = list("Craft", 0, TRUE),
    colours = list(header = colTeal, highlight = colBrRed, borders = colLtGray),
    years = 2000:2024,
    DEBUG = TRUE
)
