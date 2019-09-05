
# --- Adresse classeur contenant les paramètres des rasters à combiner :
ParamSIG_FILE <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Out/Excel/Parametres_SIG_bis_V2.xlsx"

# --- Import des rasters :
load("Tables/RasterFIN.Rdata") # normalement raster final (base SIG) contenu dans cette table
ListSheets <- getSheetNames(ParamSIG_FILE)

for (sheet in 2:length(ListSheets)) {
  raster_FILE <- paste0(rep_Ilots,"/Out/Raster/",sheet,"/",sheet,".tif")
  raster_RAS <- raster(raster_FILE)
  # plot(raster1_RAS)

  val_max <- max(abs(values(raster_RAS)),na.rm=T)
  raster_RAS <- scale(raster_RAS, center=FALSE, scale=val_max)

}
