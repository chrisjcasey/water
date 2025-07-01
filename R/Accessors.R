# Internal utility: gets an object from the package namespace
getObjectFromNamespace <- function(name, label = name) {
  env = parent.env(environment())
  if (!exists(name, envir = env, inherits = FALSE)) {
    stop(paste0("❌ ", label, " not loaded. Did the cache fail on load?"))
  }
  get(name, envir = env, inherits = FALSE)
}

#' Get the cached drowning data
#' @return A data.frame containing drownings
#' @export
GetDrowningData <- function() {
  getObjectFromNamespace("dfd", "Drowning data")
}

#' Get group summaries (grps)
#' @return A data.frame of groups
#' @export
GetGrps <- function() {
  getObjectFromNamespace("grps", "Groups")
}

#' Get drowning points (pts)
#' @return A data.frame of drowning points
#' @export
GetPts <- function() {
  getObjectFromNamespace("pts", "Drowning points")
}

#' Get TA  data (tas)
#' @return A data.frame for TAs
#' @export
GetTAs <- function() {
  getObjectFromNamespace("tas", "TA data")
}

#' Get region-level data (regs)
#' @return A data.frame for regions
#' @export
GetRegs <- function() {
  getObjectFromNamespace("regs", "Region data")
}

#' Get road data (roads)
#' @return A data.frame of roads
#' @export
GetRoads <- function() {
  getObjectFromNamespace("roads", "Road network")
}

#' Get DN dataset
#' @return A data.frame named dn
#' @export
GetDN <- function() {
  getObjectFromNamespace("dn", "DN dataset")
}

#' Get LOI dataset
#' @return A data.frame of LOIs
#' @export
GetLOI <- function() {
  getObjectFromNamespace("LOI", "Locations of interest")
}

#' Get geometry/polygon data (geoms)
#' @return An sf object
#' @export
GetGeoms <- function() {
  getObjectFromNamespace("geoms", "Geometry polygons")
}
