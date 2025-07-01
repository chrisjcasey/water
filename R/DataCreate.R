#' Tabulate Drownings Inside BLACKSPOT Polygons
#'
#' This function identifies which drowning events fall inside polygons labeled as "BLACKSPOT"
#' from the Geometry and GeometryPoints tables in your database.
#'
#' @param geometry Polygon data. List of Blackspot polygons and points
#' @param dfd A data.frame of drowning records containing at least `DrowningID`, `Latitude`, and `Longitude`.
#' @param tableName Optional. Name of the database table to write the results to (default: `"BlackSpotDrownings"`).
#' @param writeToDB Logical. If TRUE, the function writes the output to the SQL database (default: `TRUE`).
#'
#' @return A data.frame of drowning events that fall within BLACKSPOT polygons. Includes DrowningID,
#' Latitude, Longitude, GeometryID, polygon Name, and Label.
#'
#' @importFrom DBI dbGetQuery dbWriteTable
#' @importFrom sf st_as_sf st_within st_polygon st_coordinates st_sfc
#' @export
BlackspotDrowningTable<-function(tableName="BlackSpotDrownings", writeToDB=TRUE){

  geometry=GetGeoms()
  dfd=GetDrowningData()
  geometry_sf = st_as_sf(geometry, coords = c("Longitude", "Latitude"), crs = 4326)

  geometryIDs = unique(geometry$GeometryID)
  polygonList = list()
  geomList = list()

  for(i in seq_along(geometryIDs)) {
    gid = geometryIDs[i]
    rows = geometry[geometry$GeometryID == gid, ]
    coords = as.matrix(rows[, c("Longitude", "Latitude")])
    if(!all(coords[1, ] == coords[nrow(coords), ])) {
      coords = rbind(coords, coords[1, ])
    }
    polygon_geom = st_polygon(list(coords))
    polygonList[[i]] = polygon_geom
    geomList[[i]] = geometry[match(gid, geometry$GeometryID), 1:6]
  }

  polygons_sf = st_sf(do.call(rbind, geomList), geometry = st_sfc(polygonList, crs = 4326))

  dfd_clean = dfd[!is.na(dfd$Latitude) & !is.na(dfd$Longitude), ]
  drownings_sf = st_as_sf(dfd_clean, coords = c("Longitude", "Latitude"), crs = 4326)

  bs = st_within(drownings_sf, polygons_sf, sparse = FALSE)
  pairs = which(bs, arr.ind = TRUE)

  blackspotDrownings = data.frame(
    DrowningID = drownings_sf$DrowningID[pairs[, 1]],
    Latitude   = st_coordinates(drownings_sf)[pairs[, 1], 2],
    Longitude  = st_coordinates(drownings_sf)[pairs[, 1], 1],
    GeometryID = polygons_sf$GeometryID[pairs[, 2]],
    Name       = polygons_sf$Name[pairs[, 2]],
    Label      = polygons_sf$Labels[pairs[, 2]]
  )

  if(writeToDB){
    dbWriteTable(GetWSNZConnection(), tableName, blackspotDrownings, overwrite=TRUE)
  }

  return(blackspotDrownings)
}
