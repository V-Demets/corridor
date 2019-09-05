#' Organisation en dossier et sous-dossier
#'
#' @description La fonction crée la structure de fichiers et de dossiers nécessaire aux traitements des données
#'
#' @return La fonction construit l'organisation des dossiers.
#'
#' @importFrom tcltk tk_choose.dir
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @examples
#' CreateProject()
#'
#' @export

CreateProject <- function() {
  # rep <- getwd()
  dir <- tk_choose.dir(getwd(), "Choix du répertoire de travail")
  if (!is.na(dir)) {
    setwd(dir)
    dir.create(paste0(dir,"/SIG"), showWarnings = F)
    dir.create(paste0(dir,"/SIG/Rasters"), showWarnings = F)
    dir.create(paste0(dir,"/SIG/Vecteurs"), showWarnings = F)
    dir.create(paste0(dir,"/SIG/Vecteurs/DataBrutes"), showWarnings = F)
    dir.create(paste0(dir,"/SIG/Vecteurs/DataProjet"), showWarnings = F)
    dir.create(paste0(dir,"/SIG/Vecteurs/DataProjet/Perimetres"), showWarnings = F)
    dir.create(paste0(dir,"/Template"), showWarnings = F)
    dir.create(paste0(dir,"/Table"), showWarnings = F)
    setwd("Table")
    save(dir, file="Archives.RData")
    # file.copy(system.file("rmd/gf_Verif.Rmd", package="PermGF"), paste(dir,Nom,"Template", sep="/"))
    # file.copy(system.file("rmd/gf_TypoPeuplt.Rmd", package="PermGF"), paste(dir,Nom,"Template", sep="/"))
  }
  setwd(dir)
  return(dir)
}
