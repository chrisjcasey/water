#' Graphics Style Constants for the water package
#'
#' These colour and icon definitions are used across all graphical outputs.
#' Colours follow a themed scheme around water, safety, and severity.
#'

# Core colours
colBlack   = "#000000"
colWhite   = "#FFFFFF"
colTeal    = "#1F5362"
colFadTeal = "#BFD3D2"
colLtTeal  = "#D1DCE0"
colDkGray  = "#3F3F3F"
colGray    = "#808080"
colLtrGray = "#C0C0C0"
colSemiLtGray = "#D8D8D8"
colDemiLtGray = "#E8E8E8"
colLtGray  = "#F2F2F2"
colRed     = "#BF0000"
colBrRed   = "#FF0000"
colBrOrange = "#FCB520"
colBrYellow = "#EFDF00"
colBlue    = "#0000FF"
colDkBlue  = "#000080"
colLtGreen = "#009F00"
colBrown   = "#966919"
colCharcoal = "#1D1D1C"
colDarkBG  = "#D1DCE0"

# Debug colours
colDEBUG = colBrRed

# Categorical / thematic
colViolet  = "#7921B1"
colIndigo  = "#5050FF"
colMidTeal = "#40F0D0"
colGreen   = "#306F30"
colYellow  = "#D0D020"
colOrange  = "#DCA500"
colMaroon  = "#D00020"

# Seasonal palette
colSpring  = "#77DD77"
colSummer  = "#FFB347"
colAutumn  = "#C1440E"
colWinter  = "#89CFF0"
colSeasons = c(colSpring, colSummer, colAutumn, colWinter)

# Main categorical palette
colPalette = c(colViolet, colIndigo, colMidTeal, colLtGreen, colYellow, colOrange, colMaroon)

# Severity palette
colSevere  = c(colBlack, colBrYellow, colBrOrange, colBrRed)

# Super palette (slightly darkened, plus grays and teals)
# NOTE: darken() must be defined elsewhere if used
# For now, placeholder:
superpal = c(colPalette, colTeal, colFadTeal, colLtGray, colDkGray, colBlack)

# Load SVG picture icons (assumes SVGs have already been converted to cairo-compatible versions)
SVGpicUparrow   = grImport2::readPicture("c:/Graphics/uparrow-cairo.svg")
SVGpicDownarrow = grImport2::readPicture("c:/Graphics/downarrow-cairo.svg")
SVGpicNochange  = grImport2::readPicture("c:/Graphics/nochange-cairo.svg")

SVGarrows = list(
  down = SVGpicDownarrow,
  same = SVGpicNochange,
  up   = SVGpicUparrow
)

SVGpicMale   = grImport2::readPicture("c:/Graphics/male-cairo.svg")
SVGpicFemale = grImport2::readPicture("c:/Graphics/female-cairo.svg")

SVGsex = list(
  female = SVGpicFemale,
  male   = SVGpicMale
)
