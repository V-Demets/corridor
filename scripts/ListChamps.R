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
#'
#' @examples
#' \dontrun{
#' res <- ListInfos()
#' tab <- ListChamps(res$rep, res$tableau)
#'}
#'
#' @export

############### Liste des couches vectorielles ###############
ListChamps <- function(rep1, rep2, df) {
  setwd(rep2)
  Shp_FILES <- unique(df$Source)
  names(Shp_FILES) <- df$Id[match(Shp_FILES, df$Source)] # Identifiant

  # Initialisation boucle
  List_SHP <- c()
  df_BASE <- data.frame(Id=character(),
                        shp=character(),
                        ColNames=character(),
                        stringsAsFactors=F)

  pb <- tkProgressBar("Progression", "Importation des shapes (%)", 0, 100, width=500)

  df_EPSG <- data.frame(Label=c("EPSG:2154 - RGF93/Lambert-93",
                                "EPSG:7421 - NTF(Paris)/Lambert Zone II",
                                "EPSG:4326 - WGS84 (gps)",
                                "EPSG:32632 - UTM 32N (gps téléphone)"),
                        EPSG=c(2154, 7421, 4326, 32632),
                        stringsAsFactors=F)

  for (i in 1:length(Shp_FILES)) {
    Name_file <- names(Shp_FILES)[i]
    file <- Shp_FILES[[i]]
    # "Protection/apb/apb2015_02.shp" : problème encodage
    shp <- readOGR(dsn=dirname(file),
                   layer=file_path_sans_ext(basename(file)),
                   verbose=F,
                   stringsAsFactors=F,
                   use_iconv = TRUE, encoding = "CP1252")

    if (is.na(proj4string(shp))) {
      Msg1 <- tk_messageBox(type="ok",
                            message=paste0("Aucun système de projection renseigné pour le shape :\n\n",
                                           file,
                                           "\n\nCliquer sur OK et choisir le système de projection parmi la sélection proposée"))
      Answer1 <- tk_select.list(title="Choix du système de projection",
                                choices=df_EPSG$Label,
                                preselect="EPSG:2154 - RGF93/Lambert-93",
                                multiple=F)
      EPSG <- df_EPSG$EPSG[match(Answer1,df_EPSG$Label)]
      proj4string(shp) <- CRS(paste0("+init=epsg:",EPSG)) # Attribution d'un système de projection au shp
    }

    # répartition en 2 listes : 1 accueillant les .shp et l'autre les @data
    # .shp
    List_SHP <- c(List_SHP, shp)
    names(List_SHP)[length(List_SHP)] <- Name_file
    # @data
    print(length(colnames(shp@data)))
    print(file)
    df1 <- data.frame(Id=rep(Name_file, length(colnames(shp@data))),
                     Source=rep(file, length(colnames(shp@data))),
                     shp=rep(file_path_sans_ext(basename(file)), length(colnames(shp@data))),
                     ColNames=colnames(shp@data),
                     stringsAsFactors=F)
    df_BASE <- rbind(df_BASE,df1)

    info <- round(i/length(Shp_FILES)*100)
    setTkProgressBar(pb, info, paste0("Lecture des shapes en cours : (",info," %)"),
                     paste0(info,"% done"))
  } #length(Shp_FILES)

  close(pb)

  # Ici, intégrer une sélection par défaut des champs des tables attributaires
  df_BASE <- mutate(df_BASE, Inclure="Oui")
  df_BASE$Inclure[1]="Non" # test

  Answer2 <- tk_messageBox(type="yesno",
                           message="Enregistrer le classeur listant les champs des tables attributaires\n(permet de choisir les champs à inclure)?")
  if (Answer2=="yes") {
    write.xlsx(df_BASE,
               file=paste0(rep1,"/Table_Champs_SHP.xlsx"))
    Msg2 <- tk_messageBox(type="ok",
                          message=paste0("Le classeur 'Table_Champs_SHP.xlsx' listant les champs des tables attributaires a été écrit à l'emplacement : \n\n",
                                         rep_Ilots))
  }
  return(list(List_SHP, df_BASE))
}
