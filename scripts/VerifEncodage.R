#' Liste des champs dans les différentes tables attributaires.
#'
#' @description Liste des champs dans les couches recensées par la fonction ListInfos.
#'
#' @return La fonction exporte un fichier Excel contenant le liste des couches avec leurs tables attributaires.
#'
#' @author Valentin Demets
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

library(foreign)
rep1<- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN"
setwd(rep1)

rep2 <- "/Users/Valentin/Foret/Travail/PNRVN/Ilots_PNRVN/Data/SIG/Vecteurs/DataBrutes"
df_Infos <- Path_DF
# ListChamps1 <- function(rep1, rep2, df_Infos) {
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
    tab <- read.dbf(fich) %>%
      distinct()
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
