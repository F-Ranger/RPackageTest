#' modele logistique
#'
#' @param Ind_data_frame Nom de l'indice version data frame
#'
#' @return Retourner la variable glm_fit de l'indice
#'
#' @examples
#' glm_DJI <- glm_fits_indice(df_DJI)
#' summary(glm_DJI)
#'
#' glm.probs=predict(glm_DJI,type="response")
#' glm.probs[1:5]

#' glm.pred=ifelse(glm.probs>0.5,"Up","Down")
#' glm.pred[1:5]
#'
#' attach(df_DJI)
#' table(glm.pred,direction)
#'
#' @export
glm_fit_indice <- function(Ind_data_frame){
  return(stats::glm(direction~open+high+low+close+adj_close+volume,
             data=Ind_data_frame,
             family=binomial))

}
