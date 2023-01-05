#' Extraction des données
#'
#' @param date_debut La date de début de la période
#' @param date_fin La date de fin de la période
#' @param NomIndice Le nom de l'indice
#'
#' @return les données boursières entre la date de début et la date de fin
#'
#' @examples
#' DJI <- dataFinaExtract(NomIndice = "^DJI",date_debut = "2007-02-01",date_fin = "2008-02-01")
#' View(DJI)
#' @export
dataFinaExtract <-function(date_debut,date_fin,NomIndice){
  return(pdfetch::pdfetch_YAHOO(NomIndice, from=as.Date(date_debut), to = as.Date(date_fin)))
}




