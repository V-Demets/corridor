#' Liste des champs dans les différentes tables attributaires.
#'
#' @description Liste des champs dans les couches recensées par la fonction ListInfos.
#'
#' @return La fonction exporte un fichier Excel contenant le liste des couches avec leurs tables attributaires.
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @import tcltk
#' @import stringr
#' @import tools
#' @import dplyr
#' @import reshape2
#' @import openxlsx
#' @import foreign
#'
#' @examples
#' \dontrun{
#' res <- ListInfos()
#' tab <- ListChamps(res$rep, res$tableau)
#'}
#'
#' @export

############### Liste des couches vectorielles ###############
ListChamps1 <- function(rep1, rep2, df_Infos) {
  setwd(rep2)
  Shp_FILES <- unique(df_Infos$Source)
  names(Shp_FILES) <- df_Infos$Id[match(Shp_FILES, df_Infos$Source)] # Identifiant

  # Initialisation boucle
  List_SHP <- c()
  df_BASE <- data.frame(Id=character(),
                        shp=character(),
                        Attributs=character(),
                        stringsAsFactors=F)

  pb <- tkProgressBar("Progression", "Importation des shapes (%)", 0, 100, width=500)

  for (i in 1:length(Shp_FILES)) {
    Name_file <- names(Shp_FILES)[i]
    fich <- paste0(str_sub(Shp_FILES[[i]], 1, -4),"dbf")
    tab <- read.dbf(fich)
    df1 <- data.frame(Id=rep(Name_file, dim(tab)[2]),
                      Source=rep(fich, dim(tab)[2]),
                      shp=rep(file_path_sans_ext(basename(fich)), dim(tab)[2]),
                      Attributs=names(tab),
                      stringsAsFactors=F)
    df_BASE <- rbind(df_BASE,df1)
    info <- round(i/length(Shp_FILES)*100)
    print(fich)
    setTkProgressBar(pb, info, paste0("Lecture des tables attributaires en cours : (",info," %)"),
                     paste0(info,"% done"))
  } #length(Shp_FILES)

  close(pb)

  # Ici, intégrer une sélection par défaut des champs des tables attributaires
  df_BASE <- mutate(df_BASE,
                    Inclure_SHP="Non",
                    Union_Champ=NA,
                    Encodage=NA,
                    Thematique=NA, # Voir si à supprimer par la suite
                    Commentaires=NA)


  # Tableau paramétrage pour la construction du raster :
  df_BASE2 <- dplyr::select(df_BASE,
                     Id,Source,shp) %>%
    distinct() %>%
    mutate(Encodage=NA,
           Inclure_RASTER="Non",
           Intitule_ChampRaster=NA,
           Valeur_ChampRaster=NA,
           Buffer=0,
           Thematique=NA, # Voir si à supprimer par la suite
           Commentaires=NA)


  # ----- Ecriture du tableau permettant de paramétrer les shapes à réécrire ou à intégrer dans le raster
  Answer2 <- tk_messageBox(type="yesno",
                           message="Enregistrer le classeur listant les champs des tables attributaires\n(permet de choisir les champs à inclure)?")
  if (Answer2=="yes") {
    # Création des dossiers nécessaires
    dir.create("Out",
               showWarnings=F)
    dir.create("Out/Excel",
               showWarnings=F)

    # Création du classeur et des feuilles nécessaires
    wb <- createWorkbook()
    addWorksheet(wb,"Reecriture_SHP")
    addWorksheet(wb,"Creation_Raster")

    # Création des styles de cellules
    Style1 <- createStyle(fontName="Arial", fontColour="black", border= "TopBottomLeftRight",
                          fgFill="lightskyblue", textDecoration="bold", wrapText=F,
                          valign="center",halign="center",
                          textRotation=0)
    Style2 <- createStyle(fontName="Arial", wrapText=T,
                          valign="center", halign="center")

    # Ecriture des données dans les feuilles correspondantes
    addStyle(wb,
             sheet="Reecriture_SHP", Style1, rows=1, cols=1:dim(df_BASE)[2],
             gridExpand=T)
    addStyle(wb,
             sheet="Reecriture_SHP", Style2, rows=1+(1:dim(df_BASE)[1]), cols=1:dim(df_BASE)[2],
             gridExpand=T)
    addStyle(wb,
             sheet="Creation_Raster", Style1, rows=1, cols=1:dim(df_BASE2)[2],
             gridExpand=T)
    addStyle(wb,
             sheet="Creation_Raster", Style2, rows=1+(1:dim(df_BASE2)[1]), cols=1:dim(df_BASE2)[2],
             gridExpand=T)
    writeData(wb, "Reecriture_SHP", df_BASE)
    writeData(wb, "Creation_Raster", df_BASE2)
    removeColWidths(wb, sheet = "Reecriture_SHP", cols = 1:dim(df_BASE)[2])
    removeColWidths(wb, sheet = "Creation_Raster", cols = 1:dim(df_BASE2)[2])
    setColWidths(wb, sheet = "Reecriture_SHP",
                 cols = 1:dim(df_BASE)[2], widths=rep("auto",dim(df_BASE)[2]))
    setColWidths(wb, sheet = "Creation_Raster",
                 cols = 1:dim(df_BASE2)[2], widths=rep("auto",dim(df_BASE2)[2]))

    # Sauvegarde du classeur
    setwd(rep1)
    saveWorkbook(wb,
                 "Out/Excel/Parametres_SIG.xlsx",
                 overwrite=T)
    # write.xlsx(df_BASE,
    #            file=paste0(rep1,"/Data/Excel/Table_Champs_SHP.xlsx"))
    Msg2 <- tk_messageBox(type="ok",
                          message=paste0("Le classeur 'Parametres_SIG.xlsx' a été écrit à l'emplacement : \n\n",
                                         paste0(rep1,"/Out/Excel")))
  }
  out <- list(df_BASE,df_BASE2)
  names(out) <- c("Reecriture_SHP","Creation_Raster")
  return(out)
}
