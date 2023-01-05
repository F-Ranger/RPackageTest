#' Modélisation linéaire high close
#'
#' @param Ind_data_frame Le nom de l"indice version data frame
#'
#' @return le modele linéaire de l'indice qui prédit la variable Close dépéndant de la variable High
#'
#' @export
modele_lineaire_highclose <- function(Ind_data_frame){
  return(ggplot2::ggplot(Ind_data_frame, ggplot2::aes(x=high,y=close))+
           ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm"))
}
