#' Création couche urbain
#'
#' @description Création de la couche urbain à partir de la BDTOPO.
#'
#' @return La fonction retourne un SpatialPolygonDataFrame.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @import sp
#' @import maptools
#' @import rgdal
#' @import rgeos
#'
#' @examples
#' \dontrun{
#'
#'}
#'
#' @export

CreateUrbain <- function(repDataBrutes, repProjet, perim, depts, buffer=5) {
  Liste <- c("GARE",
             "AIRE_TRIAGE",
             "CIMETIERE",
             "CONSTRUCTION_LEGERE",
             "SURFACE_ROUTE",
             "POSTE_TRANSFORMATION",
             "CONSTRUCTION_SURFACIQUE",
             "PISTE_AERODROME",
             "TERRAIN_SPORT",
             "BATI_INDUSTRIEL",
             "BATI_REMARQUABLE",
             "BATI_INDIFFERENCIE")
  zone  <- gBuffer(perim, width=200)

  k=0
  for (j in 1:length(depts)){
    rep <- paste(repDataBrutes,"BDTOPO", depts[j], sep="/")
    for (i in 1:length(Liste)) {
      nom <- Liste[[i]]
      shp <- readOGR(dsn=rep, layer = nom, stringsAsFactors=FALSE)
      shp@data <- data.frame(Nature = rep(nom, dim(shp)[1]))
      shp <- spTransform(shp, CRS("+init=epsg:2154"))
      shp <- shp[perim, ] # plus rapide que la fonction crop du package raster
      k=k+1
      if (k==1) {
        urbain <- shp
      } else {
        urbain <- rbind(urbain, shp, makeUniqueIDs = TRUE)
      }
    }
  }

  shp1 <- gBuffer(urbain, width=buffer, capStyle = "SQUARE")
  tab <- data.frame(ID= 1)
  row.names(tab) <- "buffer"
  Urban <- SpatialPolygonsDataFrame(shp1, data=tab)
  writeOGR(Urban, dsn = repProjet, layer="Urbain", driver="ESRI Shapefile", overwrite_layer=TRUE)
  return(Urban)
}
