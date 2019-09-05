#' Fusion de vecteurs et découpe au périmètre d'étude.
#'
#' @description Import des données brutes par département, fusion et découpe selon le périmètre de la zone d'étude.
#'
#' @return La fonction exporte le vecteur résultat dans un dossier dont le nom est choisi par l'opérateur.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @param repDataBrutes = répertoire contenant les données brutes à traiter.
#' @param dept = liste des départements.
#' @param theme = thème à traiter. En pratique nom du sous-dossier du répertoire repDataBrutes.
#' Dans ce sous-dossier les données par département doivent être rangées dans des dossiers dont
#' le nom correspond au numéro de département.
#' @param Buffer = taille du buffer à utiliser pour la découpe de la zone d'étude.
#' @param dirOut = nom du répertoire de stockage des résultats de la fonction
#' @param FichOut = nom du fichier en sortie
#' @param var = liste des variables à retenir pour la table attribtaire du fichier de sortie.
#'
#' @import sp
#' @import rgdal
#' @import raster
#' @import rgeos
#'
#' @export

############### Fonction fusion et découpe ###############
FusionDecoupe <- function(repDataBrutes, dept, theme, couche, Buffer, dirOut, FichOut, var=NULL) {
  rep1 = paste(repDataBrutes, theme, sep="/")
  reptemp = paste(rep1, dept[1], sep="/")
  # shp <- readOGR(dsn=reptemp, layer = couche, stringsAsFactors=FALSE,
  #                use_iconv = TRUE, encoding = "CP1252")
  shp <- readOGR(dsn=reptemp, layer = couche, stringsAsFactors=FALSE)

  if (length(dept)>1) {
    for (i in 2:length(dept)){
      reptemp = paste(rep1, dept[i], sep="/")
      shp1 <- readOGR(dsn=reptemp, layer = couche)
      shp <- rbind(shp, shp1)
    }
  }
  shp <- spTransform(shp, CRS("+init=epsg:2154"))
 if (Buffer) {
   shp <- raster::crop(shp, zone)
 } else {
   shp <- raster::crop(shp, perim)
 }
  if (!is.null(var)) {
    shp <- subset(shp, select=var)
  }
  reptemp <- paste(repDataProjet, dirOut, sep="/")
  dir.create(reptemp, showWarnings=F)
  writeOGR(shp, dsn = reptemp, layer=FichOut, driver="ESRI Shapefile", overwrite_layer=TRUE)
  # return(shp)
}





