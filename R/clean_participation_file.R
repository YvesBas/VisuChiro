


#' clean participation file and extract values
#'
#' @param participation_file file imported by participants
#' @param species_list list of all species in the program
#' @param ms ???
#'
#' @return
#' @export
#'
#' @examples
clean_participation_file <- function (participation_file, species_list = "SpeciesList.csv", ms = 4) {
  
  SpeciesList <- data.table::fread(species_list, encoding = "Latin-1")
  SpeciesList$color=factor(SpeciesList$Esp)
  groupes=unique(SpeciesList$GroupFR)
  especes=unique(SpeciesList$Esp)
  
  
  if (is.null(participation_file)){
    return(NULL)      
  }
  #if(!exists("AlleYoupi5")){
  AlleYoupi5 = data.table::fread(participation_file$datapath)
  #}
  
  ####### Construction de la colonne DateHeure compatible POSIX
  #bidouille pour protocoles routiers/pedestres dans les
  if(substr(AlleYoupi5$`nom du fichier`[1],1,3)=="Cir"){
    FileInfo=tstrsplit(AlleYoupi5$`nom du fichier`,split="-")
    NumTron=as.numeric(gsub("Tron","",FileInfo[[4]]))
    Heure <- substr(AlleYoupi5$`nom du fichier`,nchar(AlleYoupi5$`nom du fichier`)-11+ms,nchar(AlleYoupi5$`nom du fichier`)-8+ms)
    DateHeure=(NumTron-1)*365+as.numeric(Heure)/3
    DateHeure=as.Date.numeric(DateHeure,origin="01-01-1900")
  }else{
    
    DateHeure <- substr(AlleYoupi5$`nom du fichier`,nchar(AlleYoupi5$`nom du fichier`)-22+ms,nchar(AlleYoupi5$`nom du fichier`)-8+ms)
    DateHeure = lubridate::ymd_hms(DateHeure,tz="Europe/Paris") 
  }
  
  AlleYoupi5$DateHeure=DateHeure
  AlleYoupi5$Date_nuit=as.Date(DateHeure-12*3600)
  
  
  #Creation de variables pour l'app Shiny
  AlleYoupi5$Affiche <- paste(AlleYoupi5$`nom du fichier`, " sp: ", AlleYoupi5$tadarida_taxon, "Confiance: ", as.character(round(AlleYoupi5$tadarida_probabilite,1), sep=""))
  AlleYoupi5$duree_sequence=AlleYoupi5$temps_fin-AlleYoupi5$temps_debut
  test=match(AlleYoupi5$tadarida_taxon,SpeciesList$Esp)
  AlleYoupi5$groupe=SpeciesList$GroupFR[test]
  AlleYoupi5$color=SpeciesList$color[test]
  params <- c("frequence_mediane", "duree_sequence","temps_debut", "temps_fin")
  AlleYoupi5=as.data.frame(AlleYoupi5)
  #if(!exists("AlleYoupi7")){
  AlleYoupi7 <- AlleYoupi5 #tableau avec validations ? sauver
  #}
  fichierslash <- gsub("\\\\", "/", participation_file)
  coupe <- unlist(strsplit(fichierslash,"/"))
  participation_file_parameters <- list(
    AlleYoupi8 = AlleYoupi7[0, ], #tableau qui s'affiche dans le dernier onglet de l'appli (validations faites)
    gpnames = append("Tous", sort(as.character(unique(AlleYoupi5$Groupe)))),
    spnames = append("Toutes", sort(as.character(unique(AlleYoupi5$tadarida_taxon)))),
    timespan = max(DateHeure) - min(DateHeure),
    sliderlabel = paste("Intervalle depuis: ", min(AlleYoupi5$DateHeure), "  jusqu'Ã  ", max(AlleYoupi5$DateHeure), sep = ""),
    mintemps = min(DateHeure),
    maxtemps = max(DateHeure),
    titre = substr(coupe[length(coupe)], 1, nchar(coupe[length(coupe)])-4),
    fichiervu = gsub(".csv","_Vu.csv", participation_file),
    AlleYoupi5 = AlleYoupi5
  )
  return(participation_file_parameters)
}
