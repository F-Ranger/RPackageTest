#' Feature Engineering
#'
#' @param nomInd Le nom de l'indice
#' @param annee l'année
#'
#' @return Générer les nouvelles variables pour une période donnée
#'
#' @examples
#' resultat_DJI <- feature_engin(DJI,'2021::')
#' View(resultat_DJI)
#' @export
feature_engin <- function(nomInd,annee){
  cbind(
    quantmod::dailyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::weeklyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::monthlyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::yearlyReturn(quantmod::Cl(nomInd),subset=annee),

    xts::apply.daily(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.weekly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.monthly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.yearly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),

    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.weekly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}),

    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.monthly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}),

    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.yearly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}))

}
