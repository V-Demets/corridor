#' Première étape du traitement de données permettant de délimiter les îlots de sénescence au sein
#' d'un territoire.
#'
#' @description Liste des champs dans les couches recensées par la fonction ListInfos. A cette liste
#' sont ajoutés des colonnes permettant de paramétrer la suite du traitement de données, et
#' notamment la sélection et la transformation des données au format raster.
#'
#' @return La fonction exporte un fichier Excel contenant le liste des couches avec leurs tables
#' attributaires.
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


ListChamps2 <- function(rep1, rep2, df_Infos) {
library(foreign)
# rep1<- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
# setwd(rep1)
#
# rep2 <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
# df_Infos <- Path_DF


  setwd(rep2)
  Shp_FILES <- unique(df_Infos$Source)
  names(Shp_FILES) <- df_Infos$Id[match(Shp_FILES, df_Infos$Source)] # Identifiant

  # Initialisation boucle
  # List_SHP <- c()
  df_BASE <- data.frame(Id=character(),
                        shp=character(),
                        Attributs=character(),
                        stringsAsFactors=F)

  ListShp_DF <- c() # Espace de sauvegarde des tables attributaires déjà lues

  pb <- tkProgressBar("Progression", "Importation des shapes (%)", 0, 100, width=500)

  for (i in 1:length(Shp_FILES)) {
    # i <- 186
    file_name <- names(Shp_FILES)[i]
    file <- paste0(str_sub(Shp_FILES[[i]], 1, -4),"dbf")
    tab <- read.dbf(file,as.is=T)

    # Sauvegarde de la table attributaire lue :
    ListShp_DF <- c(ListShp_DF,list(tab))
    names(ListShp_DF)[i] <- file_name

    # Création de la table du classeur 'Parametres_SIG'
    df1 <- data.frame(Id=rep(file_name, dim(tab)[2]),
                      Source=rep(file, dim(tab)[2]),
                      shp=rep(file_path_sans_ext(basename(file)), dim(tab)[2]),
                      Attributs=names(tab),
                      stringsAsFactors=F)
    df_BASE <- rbind(df_BASE,df1)
    info <- round(i/length(Shp_FILES)*100)
    print(file)
    setTkProgressBar(pb, info, paste0("Lecture des tables attributaires en cours : (",info," %)"),
                     paste0(info,"% done"))
  } #length(Shp_FILES)

  close(pb)


  # Tableau paramétrage pour la réécriture des shape et la construction des raster:
  df_BASE <- mutate(df_BASE,
                    Encodage=NA,
                    Reecrire_SHP=NA,
                    Union_Champ=NA,
                    Thematique_RAS=NA, # Voir si à supprimer par la suite
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
    addWorksheet(wb,"Parametrage_SIG")

    # Création des styles de cellules
    Style1 <- createStyle(fontName="Arial", fontColour="black", border= "TopBottomLeftRight",
                          fgFill="lightskyblue", textDecoration="bold", wrapText=F,
                          valign="center",halign="center",
                          textRotation=0)
    Style2 <- createStyle(fontName="Arial", wrapText=T,
                          valign="center", halign="center")

    # Ecriture des données dans les feuilles correspondantes
    addStyle(wb,
             sheet="Parametrage_SIG", Style1, rows=1, cols=1:dim(df_BASE)[2],
             gridExpand=T)
    addStyle(wb,
             sheet="Parametrage_SIG", Style2, rows=1+(1:dim(df_BASE)[1]), cols=1:dim(df_BASE)[2],
             gridExpand=T)

    writeData(wb, "Parametrage_SIG", df_BASE)
    removeColWidths(wb, sheet = "Parametrage_SIG", cols = 1:dim(df_BASE)[2])
    setColWidths(wb, sheet = "Parametrage_SIG",
                 cols = 1:dim(df_BASE)[2], widths=rep("auto",dim(df_BASE)[2]))

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
  out <- list(df_BASE)
  names(out) <- c("Parametrage_SIG")
  return(out)

  setwd(rep1)
  save(ListShp_DF,
       file="Tables/TablesAttributaires_SHP.RData")
}
