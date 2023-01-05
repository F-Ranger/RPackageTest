#' Modélisation linéaire low close
#'
#' @param Ind_data_frame Le nom de l"indice version data frame
#'
#' @return le modele linéaire de l'indice qui prédit la variable Close dépéndant de la variable Low
#'
#' @export
modele_lineaire_lowclose <- function(Ind_data_frame){
  return(ggplot2::ggplot(Ind_data_frame, ggplot2::aes(x=low,y=close))+
           ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm"))
}
